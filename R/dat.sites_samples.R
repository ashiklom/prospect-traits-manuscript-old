dat.site_sample_counts <- function(results_samples, specdb_path) {
    specdb <- dplyr::src_sqlite(specdb_path)
    siteplot <- dplyr::tbl(specdb, 'sites') %>% 
        dplyr::left_join(dplyr::tbl(specdb, 'plots')) %>% 
        dplyr::select(sitecode, plotcode, latitude, longitude) %>% 
        dplyr::collect()
    dplyr::left_join(results_samples, siteplot) %>% 
        dplyr::count(sitecode, projectcode)
}
