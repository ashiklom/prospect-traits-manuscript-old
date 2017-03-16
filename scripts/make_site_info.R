library(tidyverse)
library(methods)

specdb <- src_sqlite('extdata/leaf_spectra.db')

temp_data <- readRDS('extdata/temperature_matrix.rds') %>%
    rowMeans()
prec_data <- readRDS('extdata/precipitation_matrix.rds') %>%
    rowMeans()

site_info <- tbl(specdb, 'sites') %>%
    left_join(tbl(specdb, 'plots')) %>%
    group_by(sitecode) %>%
    summarize(site_lat = mean(latitude),
              site_lon = mean(longitude)) %>%
    filter(!is.na(site_lat), !is.na(site_lon)) %>%
    collect %>%
    mutate(prec = prec_data, 
           temp = temp_data / 10)

dir.create('data', showWarnings = FALSE)
save(site_info, file = 'data/site_info.RData')
