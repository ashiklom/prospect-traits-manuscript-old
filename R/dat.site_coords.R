dat.site_coords <- function(specdb_path) {
    specdb <- dplyr::src_sqlite(specdb_path)
    dplyr::tbl(specdb, 'sites') %>%
        dplyr::left_join(dplyr::tbl(specdb, 'plots')) %>%
        dplyr::group_by(projectcode, sitecode) %>%
        dplyr::summarize(site_lat = mean(latitude), site_lon = mean(longitude)) %>%
        dplyr::filter(!is.na(site_lat), !is.na(site_lon)) %>%
        dplyr::collect()
}
