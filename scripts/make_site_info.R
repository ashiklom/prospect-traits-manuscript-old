library(tidyverse)
library(methods)
specdb <- src_sqlite('leaf_spectra.db')

options(noaakey = 'lkgCQKelzCVzjYJuRUYUYLcIrBrZEeoM')

sites <- tbl(specdb, 'sites') %>%
    left_join(tbl(specdb, 'plots')) %>%
    group_by(sitecode) %>%
    summarize(site_lat = mean(latitude),
              site_lon = mean(longitude)) %>%
    filter(!is.na(site_lat), !is.na(site_lon)) %>%
    collect

#data_sets <- ncdc_datasets(limit = 100)
#data_types <- ncdc_datatypes(datasetid = 'GSOY', limit = 1000)

datasetid <- 'GSOY'
datatypeid <- c('TAVG', 'PRCP')

# Find closest station
station_list <- list()
options(warn = 2)
for (i in seq_len(nrow(sites))) {
    #for (i in 8:nrow(sites)) {
    print(i)
    slat <- sites[[i, 'site_lat']]
    slon <- sites[[i, 'site_lon']]
    dst <- 1
    stat <- rnoaa::ncdc_stations(datasetid = datasetid,
                                 datatypeid = datatypeid,
                                 extent = c(slat - dst, slon - dst, 
                                            slat + dst, slon + dst))
    dff <- (slat - stat$data$latitude)^2 + (slon - stat$data$longitude)^2
    station_list[[i]] <- stat$data[which.min(dff),]
    Sys.sleep(1)
}
options(warn = 0)
sitedat <- bind_cols(sites, bind_rows(station_list))
#saveRDS(sitedat, 'sitedat.rds')

dat <- rnoaa::ncdc(datasetid = datasetid,
                   datatypeid = datatypeid,
                   startdate = '2005-01-01',
                   enddate = '2014-01-01',
                   stationid = sitedat$id,
                   limit = 1000)

site_info <- dat$data %>%
    spread(datatype, value) %>%
    group_by(station) %>%
    summarize(temp = mean(TAVG, na.rm = TRUE),
              prec = mean(PRCP, na.rm = TRUE)) %>%
    right_join(sitedat, by = c('station' = 'id'))

saveRDS(site_info, 'site_info.rds')
