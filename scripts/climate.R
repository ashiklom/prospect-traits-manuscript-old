devtools::load_all('.')

data(global_vars)
data(results_long)
data(site_info)

climate_dat <- results_long %>% 
    filter(parameter %in% params) %>%
    left_join(site_info) %>% 
    gather(climname, climval, prec, temp)

climate_dat_bysp <- climate_dat %>% 
    group_by(speciescode, climname, climval, parameter) %>% 
    summarize(parametermean = mean(parametermean, na.rm = TRUE))

climplot <- ggplot() + 
    aes(x = climval, y = parametermean) + 
    geom_point() + 
    geom_smooth(method = 'lm', se = FALSE) + 
    facet_grid(parameter ~ climname, scales = 'free')

climplot %+% climate_dat

climplot %+% climate_dat_bysp

results_long_bysp <- results_long
