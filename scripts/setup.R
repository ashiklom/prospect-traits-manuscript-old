############################################################
# Script for preparing data for PROSPECT trait analysis (pta) package
# Author: Alexey Shiklomanov
############################################################
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
nparam <- length(params)
models <- c('PROSPECT 4', 'PROSPECT 5', 'PROSPECT 5B')

############################################################
# Load spectra and results databases
############################################################
specdb <- src_sqlite('extdata/leaf_spectra.db')
results_raw_long <- src_sqlite('extdata/results.db') %>% 
    tbl('results') %>% 
    select(-resultid) %>% 
    collect(n = Inf) %>% 
    mutate(modelname = factor(modelname, models),
           parameter = factor(parameter, c(params, 'deviance', 'neff', 'residual'))) %>% 
    verify(is_unique(samplecode, modelname, parameter))

############################################################
# Information on spectra, subsetted to only spectra that have results
############################################################
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
    semi_join(results_raw_long) %>% 
    group_by(samplecode, condition) %>%
    filter(row_number() == 1) %>%
    ungroup %>% 
    verify(is_unique(samplecode, condition)) %>% 
    spread(condition, conditionvalue) %>%
    select(-`<NA>`) %>% 
    verify(is_unique(samplecode))

sun_projects <- c('ngee_tropics', 'ngee_arctic', 'lopex', 'angers', 'accp')

# Set up factor levels
spectra_info <- spectra_info_raw %>% 
    mutate(collectiondate = parse_date(collectiondate),
           month = lubridate::month(collectiondate),
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
               fct_relevel('tolerant', 'intermediate', 'intolerant')) %>% 
    verify(is_unique(samplecode))
save(spectra_info, file = 'data/spectra_info.RData')

nsamples <- spectra_info %>% count %>% collect %>% .[['n']]

############################################################
# Process results
############################################################

results_all_long <- spectra_info %>% 
    left_join(results_raw_long) %>%
    verify(is_unique(samplecode, modelname, parameter))
save(results_all_long, file = 'data/results_all_long.RData')

value_suffix <- c('mean', 'sd', 'q025', 'q500', 'q975')
value_vars <- paste0('parameter', value_suffix)
names(value_vars) <- value_suffix
model_shorten <- function(modelname) gsub('PROSPECT ', '', modelname)

results_all_wide <- results_raw_long %>% 
    rename_(.dots = value_vars) %>% 
    mutate(modelname = fct_relabel(modelname, model_shorten)) %>% 
    data.table::setDT(.) %>% 
    data.table::dcast(... ~ parameter + modelname, value.var = value_suffix) %>% 
    as_data_frame() %>% 
    left_join(spectra_info) %>% 
    verify(is_unique(samplecode))
save(results_all_wide, file = 'data/results_all_wide.RData')

results_long <- results_all_long %>% 
    filter(modelname == 'PROSPECT 5B')
save(results_long, file = 'data/results_long.RData')

results_wide <- results_all_wide %>%
    select(-matches('_(4|5)$'))
save(results_wide, file = 'data/results_wide.RData')

# averaged by species
results_wide_bysp <- results_wide %>% 
    group_by(speciescode) %>% 
    summarize_at(vars(starts_with('mean'),
                      -matches('deviance|residual|neff')),
                 mean) %>% 
    ungroup()

# Residuals on parametermeans
results_wide_resid <- results_wide %>% 
    group_by(speciescode) %>% 
    mutate_at(vars(starts_with('mean')), function(x) x - mean(x)) %>% 
    ungroup()

save(results_wide_bysp, results_wide_resid, file = 'data/results_byspecies.RData')

############################################################
# Process traits
############################################################

convert_units <- function(value, trait) {
    case_when(grepl('(chl|car).*_per_area', trait) ~
                udunits2::ud.convert(value, 'kg m-2', 'ug cm-2'),
              grepl('(water_thickness|mass_per_area)', trait) ~ 
                udunits2::ud.convert(value, 'kg m-2', 'g cm-2'),
              TRUE ~ value)
}

traits_raw_long <- tbl(specdb, 'trait_data') %>% 
    select(samplecode, trait, traitvalue) %>% 
    filter(samplecode != 'nasa_fft|PB02_ABBA_TN|2008') %>% 
    collect(n = Inf) %>% 
    mutate(trait = factor(trait),
           traitvalue = convert_units(traitvalue, trait)) %>% 
    ## TODO: Fix this in curated leafspec post-processing
    group_by(samplecode, trait) %>% 
    summarize(traitvalue = mean(traitvalue, na.rm = TRUE)) %>% 
    ungroup() %>% 
    verify(is_unique(samplecode, trait))

traits_raw_wide <- traits_raw_long %>% 
    spread(trait, traitvalue)

traits_all_long <- results_all_long %>% 
    inner_join(traits_raw_long) %>% 
    verify(is_unique(samplecode, modelname, parameter, trait))
save(traits_all_long, file = 'data/traits_all_long.RData')

traits_all_wide <- results_all_wide %>% 
    inner_join(traits_raw_wide) %>% 
    verify(is_unique(samplecode))
save(traits_all_wide, file = 'data/traits_all_wide.RData')

traits_long <- results_long %>% 
    inner_join(traits_raw_long) %>% 
    verify(is_unique(samplecode, parameter, trait))
save(traits_long, file = 'data/traits_long.RData')

traits_wide <- results_wide %>% 
    inner_join(traits_raw_wide) %>% 
    verify(is_unique(samplecode))
save(traits_wide, file = 'data/traits_wide.RData')

traits_all_valid_long <- traits_all_long %>% 
    filter((parameter == 'Cab' & trait == 'leaf_chltot_per_area') |
           (parameter == 'Car' & trait == 'leaf_cartot_per_area') |
           (parameter == 'Cw' & trait == 'leaf_water_thickness') |
           (parameter == 'Cm' & trait == 'leaf_mass_per_area')) %>% 
    group_by(parameter, modelname) %>% 
    mutate(resid = parametermean - traitvalue,
           normresid = resid / traitvalue,
           scaledresid = resid / mean(traitvalue)) %>% 
    ungroup() %>% 
    verify(is_unique(samplecode, modelname, parameter, trait))
save(traits_all_valid_long, file = 'data/traits_all_valid_long.RData')

traits_valid_long <- traits_all_valid_long %>% 
    filter(modelname == 'PROSPECT 5B') %>% 
    verify(is_unique(samplecode, parameter, trait))
save(traits_valid_long, file = 'data/traits_valid_long.RData')

# Results in wide form (one row per inversion)

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
