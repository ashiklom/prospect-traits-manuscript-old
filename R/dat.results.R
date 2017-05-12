dat.results_raw_long <- function(specdb_path, resultsdb_path) {
    library(tidyverse)
    specdb <- src_sqlite(specdb_path)
    src_sqlite(resultsdb_path) %>% 
        tbl('results') %>% 
        select(-resultid) %>% 
        collect(n = Inf) %>% 
        mutate(modelname = factor(modelname, models),
               parameter = factor(parameter, params('D', TRUE))) %>% 
        verify(is_unique(samplecode, modelname, parameter))
}

dat.results_all_long <- function(results_raw_long, spectra_info) {
    left_join(results_raw_long, spectra_info) %>% 
        verify(is_unique(samplecode, modelname, parameter))
}

dat.results_wide <- function(results_raw_long, spectra_info) {
    value_suffix <- c('mean', 'sd', 'q025', 'q500', 'q975')
    value_vars <- paste0('parameter', value_suffix)
    names(value_vars) <- value_suffix
    model_shorten <- function(modelname) gsub('PROSPECT ', '', modelname)

    results_raw_long %>% 
        rename_(.dots = value_vars) %>% 
        mutate(modelname = fct_relabel(modelname, model_shorten)) %>% 
        data.table::setDT(.) %>% 
        data.table::dcast(... ~ parameter + modelname, value.var = value_suffix) %>% 
        as_data_frame() %>% 
        left_join(spectra_info) %>% 
        verify(is_unique(samplecode))
}

# averaged by species
dat.results_bysp <- function(results_wide) {
    results_wide %>% 
        group_by(speciescode) %>% 
        summarize_at(vars(starts_with('mean'),
                          -matches('deviance|residual|neff')),
                     mean) %>% 
        ungroup()
}

# Residuals on parametermeans
dat.results_species_residuals <- function(results_wide) {
    results_wide %>% 
        group_by(speciescode) %>% 
        mutate_at(vars(starts_with('mean')), function(x) x - mean(x)) %>% 
        ungroup()
}

dat.filter_results <- function(dat, model) {
    filter_(dat, modelname == model)
}
