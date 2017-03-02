library(tidyverse)
library(methods)
specdb <- src_sqlite('leaf_spectra.db')

temp_data <- readRDS('temperature_matrix.rds') %>%
    rowMeans()
prec_data <- readRDS('precipitation_matrix.rds') %>%
    rowMeans()

sites <- tbl(specdb, 'sites') %>%
    left_join(tbl(specdb, 'plots')) %>%
    group_by(sitecode) %>%
    summarize(site_lat = mean(latitude),
              site_lon = mean(longitude)) %>%
    filter(!is.na(site_lat), !is.na(site_lon)) %>%
    collect %>%
    mutate(prec = prec_data, 
           temp = temp_data / 10)

