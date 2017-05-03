devtools::load_all('.')
library(forcats)

data(global_vars)
data(results_long)
data(site_info)

climate_dat <- results_long %>% 
    filter(parameter %in% params) %>%
    left_join(site_info) %>% 
    gather(climname, climval, prec, temp) %>% 
    mutate(climname = factor(climname) %>% 
           fct_relevel('temp', 'prec') %>% 
           fct_recode(`Mean annual temperature (C)` = 'temp',
                      `Mean annual precipitation (mm)` = 'prec'))

climate_reg <- climate_dat %>% 
    nest(-parameter, -climname) %>% 
    mutate(linear = map(data, ~lm(parametermean ~ climval))

climate_dat_bysp <- climate_dat %>% 
    group_by(speciescode, climname, climval, parameter) %>% 
    summarize(parametermean = mean(parametermean, na.rm = TRUE))

climplot <- ggplot() + 
    aes(x = climval, y = parametermean) + 
    geom_point() + 
    geom_smooth(method = 'lm', se = FALSE) + 
    geom_smooth(formula = y ~ poly(x, 2), method = 'lm', se = FALSE, color = 'red') +
    facet_grid(parameter ~ climname, scales = 'free') + 
    xlab('Climate variable') +
    ylab('Parameter estimate') +
    theme_bw() + 
    theme(text = element_text(size = 15),
          strip.text = element_text(size = rel(1.2)))

climplot %+% climate_dat + 
ggsave('figures/climate_all.png')

climplot %+% climate_dat_bysp
ggsave('figures/climate_species.png')

results_long_bysp <- results_long
