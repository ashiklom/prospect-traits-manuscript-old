dat.site_samples <- function(result_samples, specdb_path) {
    specdb <- dplyr::src_sqlite(specdb_path)
    result_samples %>% 
        dplyr::left_join(dplyr::tbl(specdb, 'plots')) %>% 
        dplyr::left_join(dplyr::tbl(specdb, 'sites')) %>% 
        dplyr::count(projectcode, sitecode) %>% 
        collect()
}
