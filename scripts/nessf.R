nessf_dir <- '~/Documents/nessf_renewal'

valid_sub <- valid_dat %>%
    filter((parameter == 'Cab' & trait == 'leaf_chltot_per_area') |
           (parameter == 'Car' & trait == 'leaf_cartot_per_area') |
           (parameter == 'Cw' & trait == 'leaf_water_thickness') |
           (parameter == 'Cm' & trait == 'leaf_mass_per_area'),
       modelname == 'PROSPECT 5',
       !(projectcode %in% c('foster_beetle', 'wu_brazil', 'nasa_hyspiri'))) %>%
    mutate_at(vars(matches('parameter[[:alpha:]]+')),
              convert2si, parameter = .$parameter) %>%
    mutate(month = lubridate::month(collectiondate),
           growing_season = between(month, 5, 9)) %>%
    filter(!(parameter == 'Cab' & parametermean > 0.00125),
           !(parameter == 'Cm' & parametermean > 0.25),
           !(trait == 'leaf_water_thickness' & traitvalue > 1))

valid_plot <- valid_sub %>%
    ggplot(aes(x = parametermean, 
               xmin = parametermean - parametersd,
               xmax = parametermean + parametersd,
               y = traitvalue)) + 
        facet_wrap(~parameter + trait, scales = 'free') +
        geom_errorbarh(color = 'grey', alpha = 0.25, size = 0.5) +
        geom_point(aes(color = projectcode), size = 0.8, alpha = 0.5, stroke = 0) + 
        scale_fill_brewer(palette = 'Dark2') +
        geom_abline(linetype = 'dashed') + 
        theme_bw() +
        guides(color = guide_legend(override.aes = list(size = 4, 
                                                        alpha = 1, 
                                                        pch = 15))) +
        labs(x = 'PROSPECT 5 estimate',
             y = 'Measured value')
ggsave(file.path(nessf_dir, 'prospect_validation.png'), 
       width = 8, height = 6,
       plot = valid_plot)

# Figure 2 -- PCA

pcadat <- results_all %>%
    filter(modelname == 'PROSPECT 5', 
           parameter %in% c('N', 'Cab', 'Car', 'Cw', 'Cm')) %>%
    group_by(samplecode, parameter) %>%
    summarize(value = mean(parametermean)) %>%
    ungroup() %>%
    spread(parameter, value) %>%
    select(-samplecode)

fit <- princomp(pcadat, cor = TRUE)
biplot(fit, xlabs = rep('.', nrow(pcadat)))
