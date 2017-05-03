prep.site_biome <- function()

sitemap_dat <- spectra_info %>%
    left_join(site_info) %>%
    count(sitecode, site_lat, site_lon, temp, prec, projectcode) %>%
    ungroup() %>%
    group_by(projectcode) %>%
    mutate(nproj = sum(n)) %>%
    ungroup() %>%
    mutate(projectcode = forcats::fct_reorder(projectcode, nproj)) %>% 
    arrange(desc(nproj))
