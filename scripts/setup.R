library(methods)
library(magrittr)
library(tidyverse)
library(forcats)
library(assertr)

# Function for verifying that a tibble with the specified rows
is_unique <- function(...) {
    tibble(...) %>%
        count(...) %>%
        filter(n > 1) %>%
        nrow() %>%
        !.
}

# Useful global definitions
params <- c('N', 'Cab', 'Car', 'Cbrown', 'Cw', 'Cm')
npar <- length(params)
models <- c('PROSPECT 4', 'PROSPECT 5', 'PROSPECT 5B')

# Load spectra and results databases
specdb <- src_sqlite('extdata/leaf_spectra.db')
results_raw <- src_sqlite('extdata/results.db') %>% 
    tbl('results') %>% 
    select(-resultid) %>% 
    collect(n = Inf)

# Information on spectra, subsetted to only spectra that have results
spectra_info_raw <- tbl(specdb, 'samples') %>% 
    select(-sampleid) %>% 
    semi_join(tbl(specdb, 'spectra_info') %>% 
              filter(spectratype != 'transmittance')) %>% 
    left_join(tbl(specdb, 'sample_condition') %>%
              select(samplecode, condition, conditionvalue)) %>%
    left_join(tbl(specdb, 'species')) %>%
    left_join(tbl(specdb, 'species_attributes')) %>%
    left_join(tbl(specdb, 'plots')) %>%
    left_join(tbl(specdb, 'sites')) %>%
    collect(n = Inf) %>% 
    semi_join(results_raw) %>% 
    group_by(samplecode, condition) %>%
    filter(row_number() == 1) %>%
    ungroup %>% 
    verify(is_unique(samplecode, condition)) %>% 
    spread(condition, conditionvalue) %>%
    select(-`<NA>`)

sun_projects <- c('ngee_tropics', 'ngee_arctic', 'lopex', 'angers', 'accp')

# Set up factor levels
spectra_info <- spectra_info_raw %>% 
    mutate(collectiondate = parse_date(collectiondate),
           sunshade = case_when(!is.na(.$sunshade) ~ .$sunshade,
                                is.na(.$CanopyPosition) & 
                                    .$projectcode %in% sun_projects ~ 'sun',
                                .$CanopyPosition == 'T' ~ 'sun',
                                .$CanopyPosition %in% c('M','B') ~ 'shade',
                                TRUE ~ NA_character_),
           sunshade = factor(sunshade, c('sun', 'shade')),
           leaf_type = case_when(!is.na(.$leaf_type) ~ .$leaf_type,
                                 .$projectcode == 'wu_brazil' ~ 'broad',
                                 TRUE ~ NA_character_),
           leaf_type = factor(leaf_type, c('broad', 'needle')),
           myco_asso = factor(myco_asso) %>% 
               fct_recode(
                'Arbuscular' = 'AM',
                'Arbuscular' = 'AM + NM',
                'Ecto' = 'ECM',
                'Ecto' = 'EM',
                'Either' = 'AM + ECM',
                'Ericoid' = 'ERM',
                'No association' = 'NM'
                ) %>% 
                fct_relevel('Arbuscular', 'Ecto', 'Either',
                            'Ericoid', 'No association'),
           family = factor(family),
           phenology = factor(phenology, c('deciduous', 'evergreen')),
           ps_type = factor(ps_type, c('C3', 'C4')),
           nitrogen_fixer = factor(nitrogen_fixer, c(0, 1)) %>% 
               fct_recode('No N fixation' = '0', 'N-fixer' = '1'),
           growth_form = factor(growth_form) %>% fct_relevel('tree'),
           growth_form_simple = fct_collapse(growth_form, 
                                            'woody' = c('tree', 'shrub'),
                                            'non-woody' = c('graminoid', 'herb',
                                                            'lichen', 'vine')),
           shade_tolerance = factor(shade_tolerance) %>% 
               fct_relevel('tolerant', 'intermediate', 'intolerant'))

save(spectra_info, file = 'data/spectra_info.RData')

nsamples <- tbl(specdb, 'samples') %>% count %>% collect %>% .[['n']]

results_all <- spectra_info %>% 
    left_join(results_raw) %>%
    verify(is_unique(samplecode, modelname, parameter)) %>%
    mutate(modelname = factor(modelname, models),
           parameter = factor(parameter, c(params, 'deviance', 'neff', 'residual')))

save(results_all, file = 'data/results_all.RData')

results <- results_all %>% filter(modelname == 'PROSPECT 5B')

results_wide <- results %>% 
    select(-resultid) %>% 
    data.table::setDT(.) %>% 
    data.table::dcast(... ~ parameter, value.var = value_vars) %>% 
    as_data_frame()

# averaged by species
results_wide_bysp <- results_wide %>% 
    group_by(speciescode) %>% 
    summarize_at(vars(starts_with('parametermean'),
                      -matches('deviance|residual|neff')),
                 mean) %>% 
    ungroup()

# Residuals on parametermeans
results_wide_resid <- results_wide %>% 
    group_by(speciescode) %>% 
    mutate_at(vars(starts_with('parametermean')), function(x) x - mean(x)) %>% 
    ungroup()

# Convenience vector for easy select_ and rename_ calls
longpar_names <- paste0('parametermean_', params)
names(longpar_names) <- params

convert2si <- function(value, parameter) {
    case_when(parameter %in% c('Cab', 'Car') ~ 
                udunits2::ud.convert(value, 'ug cm-2', 'kg m-2'),
              parameter %in% c('Cw', 'Cm') ~ 
                udunits2::ud.convert(value, 'g cm-2', 'kg m-2'),
              TRUE ~ NA_real_)
}

traits_all <- results_all %>%
    inner_join(tbl(specdb, 'trait_data') %>% 
               filter(samplecode != 'nasa_fft|PB02_ABBA_TN|2008') %>%
               collect(n = Inf)) %>% 
    mutate_at(vars(matches('parameter[[:alpha:]]+')),
              convert2si, parameter = .$parameter)

save(traits_all, file = 'data/traits_all.RData')

# Filter down to PROSPECT 5B, which will be used from now on
traits <- traits_all %>% filter(modelname == 'PROSPECT 5B')

valid_dat <- traits %>%
    filter((parameter == 'Cab' & trait == 'leaf_chltot_per_area') |
           (parameter == 'Car' & trait == 'leaf_cartot_per_area') |
           (parameter == 'Cw' & trait == 'leaf_water_thickness') |
           (parameter == 'Cm' & trait == 'leaf_mass_per_area')) %>%
    group_by(parameter) %>%
    mutate(month = lubridate::month(collectiondate),
           resid = parametermean - traitvalue,
           normresid = resid / traitvalue,
           scaledresid = resid / mean(traitvalue)) %>%
    ungroup()

# Results in wide form (one row per inversion)
value_vars <- paste0('parameter', c('mean', 'sd', 'q025', 'q500', 'q975'))

## Comparison of PROSPECT models
##modcodes <- c('PROSPECT 4' = 'p4', 'PROSPECT 5' = 'p5', 'PROSPECT 5B' = 'p5b')
#results_model <- results %>%
    #mutate(mod = modcodes[modelname]) %>%
    ##unite(par_mod, par, mod) %>%
    ##select(samplecode, par_mod, 
           ##mu = parametermean, lo = parameterq025, hi = parameterq975) %>%
    ##data.table::setDT(.) %>%
    ##data.table::dcast(samplecode ~ par_mod, value.var = c('mu', 'lo', 'hi')) %>%
    ##as_data_frame

# For main results, use PROSPECT 5B
