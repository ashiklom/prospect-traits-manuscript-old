dat.results_samples <- function(specdb_path, resultsdb_path) {
    specdb <- dplyr::src_sqlite(specdb_path)
    results_samples <- dplyr::src_sqlite(resultsdb_path) %>% 
        dplyr::tbl('results') %>% 
        distinct(samplecode) %>% 
        dplyr::collect()

    dplyr::tbl(specdb, 'samples') %>% 
        dplyr::collect() %>% 
        dplyr::anti_join(results_samples)
}
