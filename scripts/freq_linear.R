devtools::load_all('.')

data(global_vars)
data(results_long)

fit_data <- results_long %>% 
    filter(parameter %in% params) %>% 
    mutate(project = factor(projectcode),
           species = factor(speciescode),
           project_species = interaction(project, species),
           species_site = interaction(sitecode, species),
           species_site_sunshade = interaction(species_site, sunshade),
           pft = interaction(leaf_type, phenology, growth_form_simple)
           ) %>% 
    rename(woodiness = growth_form_simple)

#lm1 <- freqlm(fit_data, c('pft', 'myco_asso', 'species'))
#lm1$coef_plot

lm2_vars <- c('woodiness' = 'black', 
              'leaf_type' = 'green2',
              'phenology' = 'darkorchid4',
              'myco_asso' = 'yellow',
              'shade_tolerance' = 'deepskyblue4',
              'species' = 'red4',
              'species_site' = 'turquoise2', 
              'sunshade' = 'orange')

lm2 <- freqlm(fit_data, names(lm2_vars), 
              coef_exclude = c('species', 'species_site'))

lm2$coef_plot + 
    xlab('Attribute') +
    ylab('Linear model estimate') +
    theme_bw() + 
    theme(text = element_text(size = 15),
          axis.title = element_text(size = rel(1.5)),
          axis.text = element_text(size = rel(1.2)),
          strip.text = element_text(size = rel(1.3)))
ggsave('figures/lm_coefficients.png')

lm2$variance_plot + 
    xlab('Parameter') + 
    ylab('Normalized sum of squares') +
    guides(fill = guide_legend(title = 'Attribute')) + 
    scale_fill_manual(values = c(lm2_vars, 'Residuals' = 'grey')) + 
    theme_bw() + 
    theme(text = element_text(size = 15),
          axis.title = element_text(size = rel(1.5)),
          axis.text = element_text(size = rel(1.2)),
          legend.title = element_text(size = rel(1.5)),
          legend.text = element_text(size = rel(1.25)))
ggsave('figures/lm_anova.png', width = 12, height = 10)
