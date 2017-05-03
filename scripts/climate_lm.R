devtools::load_all('.')

data(global_vars)
data(results_long)
data(site_info)

climfit <- . %>% 
    nest(-parameter, -climname) %>% 
    mutate(lin = map(data, function(x) lm(estimate ~ climval, x)),
           quad = map(data, function(x) lm(estimate ~ poly(climval, 2), x)),
           lin_aic = map(lin, extractAIC) %>% map_dbl(2),
           quad_aic = map(quad, extractAIC) %>% map_dbl(2),
           mod_char = if_else(lin_aic < quad_aic, 'linear', 'quadratic'),
           mod = if_else(lin_aic < quad_aic, lin, quad),
           mod_glance = map(mod, broom::glance),
           mod_p = map_dbl(mod_glance, 'p.value'),
           mod_r2 = map_dbl(mod_glance, 'r.squared'),
           climval_grid = map(data, modelr::data_grid, 
                              climval = modelr::seq_range(climval, 15)),
           mod_pred = map2(climval_grid, mod, modelr::add_predictions)) 

climplotdat <- . %>%
    unnest(mod_pred) %>% 
    mutate(pred = if_else(mod_p < 0.05, pred, NA_real_)) ->
        site_plotdat

climplot <- function(data, fit) {
    ggplot() + 
        geom_point(data = data, aes(x = climval, y = estimate)) + 
        geom_line(data = fit, aes(x = climval, y = pred, color = mod_char)) + 
        facet_grid(parameter ~ climname, scales = 'free') + 
        guides(color = guide_legend(title = 'Best-fitting model'))
} 

species_means <- results_long %>% 
    filter(parameter %in% params) %>% 
    group_by(speciescode, sitecode, parameter) %>% 
    summarize(estimate = mean(parametermean, na.rm = TRUE)) %>% 
    ungroup() %>% 
    inner_join(site_info %>% select(sitecode, prec, temp)) %>% 
    gather(climname, climval, prec, temp) %>% 
    mutate(climname = factor(climname) %>% 
           forcats::fct_relevel('temp', 'prec') %>% 
           forcats::fct_recode(`Mean annual temperature (C)` = 'temp',
                      `Mean annual precipitation (cm)` = 'prec'))

common_theme <- theme_bw() + 
    theme(text = element_text(size = 13),
          strip.text = element_text(size = rel(1.1)))

species_fits <- species_means %>% climfit
species_plotdat <- species_fits %>% climplotdat

species_climplot <- climplot(species_means, species_plotdat) + 
    ylab('Species x site mean trait estimate') + 
    xlab('Climate variable') + 
    common_theme + 
    theme(legend.position = 'bottom')
plot(species_climplot)
ggsave(plot = species_climplot, 
       filename = 'figures/climate_species.png',
       width = 7, height = 8)

fit_data <- results_long %>% 
    filter(parameter %in% params) %>% 
    mutate(project = factor(projectcode),
           species = factor(speciescode),
           project_species = interaction(project, species),
           site = interaction(sitecode, species),
           shade = interaction(site, sunshade),
           pft = interaction(leaf_type, phenology, growth_form_simple)
           ) %>% 
    rename(woodiness = growth_form_simple)

variables <- c('species', 'site')
variables_rxp <- stringr::str_c(variables, collapse = '|')

lm2 <- freqlm(fit_data, variables, response = 'parametermean')

tab <- lm2$table %>% 
    unnest(coefs) %>% 
    mutate(group = stringr::str_extract(term, variables_rxp) %>% 
            factor(levels = variables),
           contrast = stringr::str_replace(term, variables_rxp, ''))

site_coefs <- tab %>% 
    filter(group == 'site') %>% 
    separate(contrast, c('projectcode', 'site_id', 'speciescode'), 
             sep = '\\.', remove = FALSE) %>% 
    mutate(sitecode = paste(projectcode, site_id, sep = '.')) %>% 
    select(parameter, sitecode, everything()) 

site_climate_dat <- site_coefs %>% 
    left_join(site_info) %>% 
    gather(climname, climval, prec, temp) %>% 
    mutate(climname = factor(climname) %>% 
           forcats::fct_relevel('temp', 'prec') %>% 
           forcats::fct_recode(`Mean annual temperature (C)` = 'temp',
                      `Mean annual precipitation (cm)` = 'prec'))

site_fits <- site_climate_dat %>% climfit
site_plotdata <- site_fits %>% climplotdat

siteplot <- climplot(site_climate_dat, site_plotdata) + 
    ylab('Species x site fixed effect') + 
    xlab('Climate variable') + 
    guides(color = FALSE) + 
    common_theme
ggsave(plot = siteplot, 
       filename = 'figures/climate_coefs.png',
       width = 7, height = 7)
