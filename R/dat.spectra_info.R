############################################################
# Information on spectra, subsetted to only spectra that have results
############################################################
dat.spectra_info <- function(specdb_path, results_raw_long) {
    library(tidyverse)
    specdb <- src_sqlite(specdb_path)
    spectra_info_raw <- tbl(specdb, 'samples') %>% 
        semi_join(tbl(specdb, 'spectra_info') %>% 
                  filter(spectratype != 'transmittance')) %>% 
        left_join(tbl(specdb, 'sample_condition') %>%
                  select(samplecode, condition, conditionvalue)) %>%
        left_join(tbl(specdb, 'species')) %>%
        left_join(tbl(specdb, 'species_attributes')) %>%
        left_join(tbl(specdb, 'plots')) %>%
        left_join(tbl(specdb, 'sites')) %>%
        left_join(tbl(specdb, 'projects')) %>% 
        collect(n = Inf) %>% 
        semi_join(results_raw_long %>% distinct(samplecode)) %>% 
        group_by(samplecode, condition) %>%
        filter(row_number() == 1) %>%
        ungroup %>% 
        verify(is_unique(samplecode, condition)) %>% 
        spread(condition, conditionvalue) %>%
        select(-`<NA>`) %>% 
        verify(is_unique(samplecode))

# Set up factor levels. See R/factors.R for details
    spectra_info <- spectra_info_raw %>% 
        mutate(collectiondate = parse_date(collectiondate),
               month = lubridate::month(collectiondate),
               doy = lubridate::yday(collectiondate),
               family = factor(family),
               sunshade = factor.sunshade(sunshade, CanopyPosition, projectcode), 
               leaf_type = factor.leaf_type(leaf_type, projectcode),
               myco_asso = factor.myco_asso(myco_asso),
               phenology = factor(phenology, c('deciduous', 'evergreen')),
               ps_type = factor(ps_type, c('C3', 'C4')),
               nitrogen_fixer = factor.nitrogen_fixer(nitrogen_fixer),
               growth_form = factor.growth_form(growth_form),
               woodiness = factor.woodiness(growth_form)) %>% 
        verify(is_unique(samplecode))
    
    return(spectra_info)
}
