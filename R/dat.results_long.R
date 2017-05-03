dat.results_long <- function(specdb_path, resultsdb_path) {
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
