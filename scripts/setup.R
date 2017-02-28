library(methods)
library(tidyverse)
library(assertr)

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
    left_join(tbl(specdb, 'species') %>% collect(n = Inf)) %>%
    left_join(tbl(specdb, 'species_attributes') %>% collect(n = Inf)) %>%
    left_join(tbl(specdb, 'plots') %>% collect(n = Inf)) %>%
    left_join(tbl(specdb, 'sites') %>% collect(n = Inf)) %>%
    mutate(modelname = factor(modelname, models),
           parameter = factor(parameter, c(params, 'deviance', 'neff', 'residual')),
           collectiondate = parse_date(collectiondate))

valid_dat <- results_all %>%
    inner_join(tbl(specdb, 'trait_data') %>% collect(n = Inf))

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
