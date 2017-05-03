#library(pta)
devtools::load_all('.')
data(global_vars)

#data(results_wide)
#fit_data <- results_wide %>% 
    #mutate(project = factor(projectcode),
           #species = factor(speciescode),
           #project_species = factor(paste(project, species))) %>% 
    #filter(!is.na(mean_N_5B), !is.na(project), !is.na(species), !is.na(project_species))

##debugonce(jags_data)
#samples <- lapply(params, 
                  #jags_sample, 
                  #data = fit_data, 
                  #effect_list = c('project', 'species', 'project_species'),
                  #max.time = '5 m')
#names(samples) <- params
#saveRDS(samples, file = 'jags_results/project.species.project_species.rds')

#av1 <- jags_sumsq_table(samples$N, 
                        #effect_list = c('project', 'species', 'project_species'),
                        #response_var = NULL)

#anovas <- lapply(samples, jags_sumsq_table, 
                 #effect_list = c('project', 'species', 'project_species'),
                 #response_var = NULL) %>% 
    #mapply(`[[<-`, .data = ., i = 'parameter', value = params)

## Try with dplyr mapping
data(results_long)
filter_cols <- c('species', 'leaf_type', 'phenology', 'ps_type',
                 'nitrogen_fixer', 'shade_tolerance', 'growth_form',
                 'project')
fit_data <- results_long %>% 
    filter(parameter %in% params) %>% 
    mutate(project = factor(projectcode),
           species = factor(speciescode),
           project_species = factor(paste(project, species))) %>% 
    filter_(.dots = sprintf('!is.na(%s)', filter_cols))

effects_table <- tibble(i = 1,
    effect_list = list(
    c('species', 'leaf_type', 'phenology', 'ps_type', 'nitrogen_fixer', 
      'shade_tolerance', 'growth_form_simple')))#,
    #'project',
    #'species',
    #c('project', 'species'),
    #c('project', 'species', 'project_species')))#,
    #c('project', 'species', 'leaf_type', 'phenology', 'ps_type', 'nitrogen_fixer', 'shade_tolerance', 'growth_form_simple'),
    #c('species', 'leaf_type', 'phenology', 'nitrogen_fixer', 'shade_tolerance', 'growth_form_simple'),
    #c('species', 'leaf_type', 'phenology', 'shade_tolerance', 'growth_form_simple'),
    #c('project', 'species', 'project_species', 'leaf_type'),
    #c('project', 'species', 'leaf_type', 'phenology'),
    #c('project', 'species', 'leaf_type', 'phenology', 'growth_form')))

fit_data_nested <- fit_data %>% 
    nest(-parameter) %>% 
    mutate(i = 1) %>% 
    full_join(effects_table)

myfit <- function(data, effect_list) {
    jags_sample(data = data,
                response_var = NULL,
                mean_prefix = 'parametermean',
                sd_prefix = 'parametersd',
                suffix = NULL,
                effect_list = effect_list,
                max.time = '4 m')
}

jags_fits <- fit_data_nested %>% 
    mutate(jags_fit = map2(data, effect_list, myfit))

saveRDS(jags_fits, file = 'jags_results/new_jags_fits.rds')

sum2rn <- function(x) {
    x$summaries %>% add_rownames()
}



jags_fits %>% 
    group_by(parameter) %>% 
    filter(row_number() == 1) %>% 
    mutate(summaries = map(jags_fit, function(x) x$summaries %>% 
                           as.data.frame %>%
                           rownames_to_column)) %>% 
    unnest(summaries) %>% 
    mutate(term = gsub('\\[.*\\]', '', rowname),
           term = gsub('\\.term', '', term)) -> jags_coefs

jags_coefs %>% 
    ungroup() %>% 
    filter(!term %in% c('species', 'y0', 'y_tau')) %>% 
    select(parameter, term, Mean)
