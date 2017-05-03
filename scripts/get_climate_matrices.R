library(tidyverse)
library(methods)
specdb <- src_sqlite('extdata/leaf_spectra.db')

sites <- tbl(specdb, 'sites') %>%
    left_join(tbl(specdb, 'plots')) %>%
    group_by(sitecode) %>%
    summarize(site_lat = mean(latitude),
              site_lon = mean(longitude)) %>%
    filter(!is.na(site_lat), !is.na(site_lon)) %>%
    collect

coords <- sites %>%
    dplyr::select(site_lon, site_lat) %>%
    as.matrix() %>%
    sp::SpatialPoints(proj4string = sp::CRS('+proj=longlat +datum=WGS84'))

tempdat <- matrix(NA_real_, nrow(sites), 12)
precdat <- tempdat

iter <- 0
while(any(is.na(tempdat[,1]))) {
    iter <- iter + 1
    j <- which(is.na(tempdat[,1]))[1]
    lon <- sites[[j, 'site_lon']]
    lat <- sites[[j, 'site_lat']]
    print(c(iter, lon, lat))
    clim_data <- raster::getData('worldclim', 
                                 var = 'prec',
                                 res = 0.5, 
                                 lon = lon,
                                 lat = lat)
    clim_mat <- raster::extract(clim_data, coords)
    fill_inds <- which(!is.na(clim_mat[,1]))
    precdat[fill_inds,] <- clim_mat[fill_inds,]
}

