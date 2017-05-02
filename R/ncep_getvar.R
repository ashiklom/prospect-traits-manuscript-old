ncep_getvar <- function(lat, lon, varname) {
    riri::base_url() %>% 
        riri::ncep_var(varname) %>% 
        riri::filter_point(lat = lat, lon = lon) %>% 
        riri::generate_url() %>% 
        riri::retrieve_data() %>% 
        riri::read_nc2list(dims = 'T') %>% 
        riri::process_date() %>% 
        tibble::as_data_frame()
}

ncep_getvars <- function(lat, lon, varnames) {
    l <- lapply(varnames, function(x) ncep_getvar(lat, lon, x))
    Reduce(left_join, l) %>% dplyr::select(date, everything())
}
