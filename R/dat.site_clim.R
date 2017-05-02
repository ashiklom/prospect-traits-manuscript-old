dat.site_clim_full <- function(dat) {
    climvars <- c('air_temperature', 'precipitation', 'soil_moisture')
    dat %>% 
        dplyr::mutate(clim_dat = purrr::map2(site_lat, site_lon, 
                                             ~ncep_getvars(.x, .y, climvars)))
}

dat.site_clim_means <- function(dat) {
    dat %>% 
        tidyr::unnest() %>% 
        dplyr::mutate(year = lubridate::year(date)) %>% 
        dplyr::filter(year < 2017) %>% 
        dplyr::group_by(projectcode, sitecode, site_lat, site_lon, year) %>%
        dplyr::summarize(MAT = udunits2::ud.convert(mean(temp), 'Kelvin', 'Celsius'),
                         MAP = sum(rain),
                         MASM = mean(w)) %>% 
        dplyr::group_by(projectcode, sitecode, site_lat, site_lon) %>% 
        dplyr::summarize(MAT = mean(MAT), MAP = mean(MAP), MASM = mean(MASM))
}
