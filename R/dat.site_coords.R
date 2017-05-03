# Create sites based on plots that are more than `dist_degrees` away from each other
siteplot_cluster <- function(specdb_path, dist_degrees) {
    plots <- src_sqlite(specdb_path) %>% 
        tbl('plots') %>% 
        filter(!is.na(latitude), !is.na(longitude)) %>% 
        collect()

    plot_matrix <- plots %>% 
        select(latitude, longitude) %>% 
        as.matrix()

    pdist <- dist(plot_matrix)
    pclust <- hclust(pdist)
    psites <- cutree(pclust, h = dist_degrees)

    siteplot <- plots %>% 
        mutate(sitecode = psites[plotcode])

    return(siteplot)
}

siteplot_naive <- function(specdb_path) {
    specdb <- dplyr::src_sqlite(specdb_path)
    dplyr::tbl(specdb, 'sites') %>%
        dplyr::left_join(dplyr::tbl(specdb, 'plots')) %>%
        dplyr::collect()
}

dat.site_coords <- function(siteplot) {
    siteplot %>% 
        dplyr::group_by(projectcode, sitecode) %>%
        dplyr::summarize(site_lat = mean(latitude), site_lon = mean(longitude)) %>%
        dplyr::filter(!is.na(site_lat), !is.na(site_lon))
}
