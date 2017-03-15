library(methods)
library(tidyverse)
library(forcats)
library(assertr)
library(knitr)
library(grid)
library(gridExtra)

select <- dplyr::select

# Useful global definitions
params <- c('N', 'Cab', 'Car', 'Cbrown', 'Cw', 'Cm')
npar <- length(params)
models <- c('PROSPECT 4', 'PROSPECT 5', 'PROSPECT 5B')

# Load spectra and results databases
specdb <- src_sqlite('leaf_spectra.db')

nsamples <- tbl(specdb, 'samples') %>% count %>% collect %>% .[['n']]

is_unique <- function(...) {
    tibble(...) %>%
        count(...) %>%
        filter(n > 1) %>%
        nrow() %>%
        !.
}

samples <- tbl(specdb, 'samples') %>%
    left_join(tbl(specdb, 'sample_condition') %>%
              select(samplecode, condition, conditionvalue)) %>%
    semi_join(tbl(specdb, 'spectra_info')) %>% 
    left_join(tbl(specdb, 'species')) %>%
    left_join(tbl(specdb, 'species_attributes')) %>%
    left_join(tbl(specdb, 'plots')) %>%
    left_join(tbl(specdb, 'sites')) %>%
    collect(n = Inf) %>%
    group_by(samplecode, condition) %>%
    filter(row_number() == 1) %>%
    ungroup %>%
    verify(is_unique(samplecode, condition)) %>%
    spread(condition, conditionvalue) %>%
    select(-`<NA>`)

results_all <- src_sqlite('results.db') %>% 
    tbl('results') %>%
    collect(n = Inf) %>%
    left_join(samples) %>%
    verify(is_unique(samplecode, modelname, parameter)) %>%
    mutate(modelname = factor(modelname, models),
           parameter = factor(parameter, c(params, 'deviance', 'neff', 'residual')),
           collectiondate = parse_date(collectiondate),
           leaf_type = if_else(projectcode == 'wu_brazil', 'broad', leaf_type) %>% 
               factor(),
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
           phenology = factor(phenology) %>% fct_relevel('deciduous'),
           ps_type = factor(ps_type) %>% fct_relevel('C3'),
           nitrogen_fixer = factor(nitrogen_fixer) %>% fct_relevel(0),
           growth_form = factor(growth_form) %>% fct_relevel('tree'),
           shade_tolerance = factor(shade_tolerance) %>% 
               fct_relevel('tolerant', 'intermediate', 'intolerant'))

traits_all <- results_all %>%
    inner_join(tbl(specdb, 'trait_data') %>% 
               filter(samplecode != 'nasa_fft|PB02_ABBA_TN|2008') %>%
               collect(n = Inf))

# Filter down to PROSPECT 5B, which will be used from now on
results <- results_all %>% filter(modelname == 'PROSPECT 5B')
traits <- traits_all %>% filter(modelname == 'PROSPECT 5B')

convert2si <- function(value, parameter) {
    case_when(parameter %in% c('Cab', 'Car') ~ 
                udunits2::ud.convert(value, 'ug cm-2', 'kg m-2'),
              parameter %in% c('Cw', 'Cm') ~ 
                udunits2::ud.convert(value, 'g cm-2', 'kg m-2'),
              TRUE ~ NA_real_)
}

valid_dat <- traits %>%
    filter((parameter == 'Cab' & trait == 'leaf_chltot_per_area') |
           (parameter == 'Car' & trait == 'leaf_cartot_per_area') |
           (parameter == 'Cw' & trait == 'leaf_water_thickness') |
           (parameter == 'Cm' & trait == 'leaf_mass_per_area')) %>%
    mutate_at(vars(matches('parameter[[:alpha:]]+')),
              convert2si, parameter = .$parameter) %>%
    group_by(parameter) %>%
    mutate(month = lubridate::month(collectiondate),
           resid = parametermean - traitvalue,
           normresid = resid / traitvalue,
           scaledresid = resid / mean(traitvalue)) %>%
    ungroup()

# Results in wide form (one row per inversion)
value_vars <- paste0('parameter', c('mean', 'sd', 'q025', 'q500', 'q975'))

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
