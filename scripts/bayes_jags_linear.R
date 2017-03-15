source('scripts/setup.R')
library(runjags)
runjags.options(force.summary = TRUE, modules = 'glm')

my_sumsq <- function(value, attribute, bayesfit_summary, attribute_name) {
    coef_names <- rownames(bayesfit_summary)
    coef_inds <- grep(paste0('^', attribute_name, '\\['), coef_names)
    coefs <- bayesfit_summary[coef_inds, 'Mean']
    stopifnot(max(attribute) == length(coefs))
    value_predicted <- coefs[attribute]
    effect_sumsq <- sum((value_predicted - mean(value_predicted))^2)
    tot_sumsq <- sum((value - mean(value))^2)
    return(c(effect_ss = effect_sumsq, 
             total_ss = tot_sumsq, 
             frac_expl = effect_sumsq / tot_sumsq))
}

getresult <- function(bayesfit_summary, parameter) {
    rnames <- rownames(bayesfit_summary)
    inds <- grep(paste0('^', parameter, '_'), rnames)
    bayesfit_summary[inds,]
}

model_code <- 'model{
# Parameter uncertainty
for (i in 1:n_row) {
    x_mu[i] ~ dnorm(x[i], x_tau[i])

    # Model uncertainty
    x[i] ~ dnorm(y[i], y_tau)

    # Linear model
    y[i] = y0 + 
        species_term[species[i]] + 
        site_term[site[i]] + 
        species_site_term[species_site[i]] + 
        project_term[project[i]]
}

# Priors
y0 ~ dnorm(0, 0.01)
y_tau ~ dgamma(0.01, 0.01)

for (i in 1:n_species) {species_term[i] ~ dnorm(0, 0.01)}
for (i in 1:n_site) {site_term[i] ~ dnorm(0, 0.01)}
for (i in 1:n_species_site) {species_site_term[i] ~ dnorm(0, 0.01)}
for (i in 1:n_project) {project_term[i] ~ dnorm(0, 0.01)}
        
}
'

# Data definitions
fit_data <- results_wide %>% 
    filter(!is.na(speciescode), !is.na(sitecode)) %>% 
    mutate(species = factor(speciescode), 
           site = factor(sitecode),
           project = factor(projectcode),
           species_site = paste(species, site) %>% factor())

x_mu <- fit_data[['parametermean_N']]
x_tau <- 1 / (fit_data[['parametersd_N']])^2
species <- fit_data[['species']] %>% as.numeric
n_species <- max(species)
site <- fit_data[['site']] %>% as.integer
n_site <- max(site)
species_site <- fit_data[['species_site']] %>% as.integer
n_species_site <- max(species_site)
project <- fit_data[['project']] %>% as.integer
n_project <- max(project)
n_row <- length(x_mu)

out2 <- autorun.jags(model = model_code,
                    startburnin = 500,
                    startsample = 4000,
                    monitor = c('y0', 'y_tau', 'species_term', 'site_term',
                                'species_site_term', 'project_term', 'dic'),
                    data = list(x_mu = x_mu, x_tau = x_tau,
                                species = species, n_species = n_species,
                                site = site, n_site = n_site,
                                species_site = species_site, 
                                n_species_site = n_species_site,
                                project = project, n_project = n_project,
                                n_row = n_row),
                    modules = c('glm'))

out$dic$dic
# sum.mean.deviance 
#         -6678.469 

my_anova <- mapply(my_sumsq, 
                   attribute = list(species, site, species_site, project),
                   attribute_name = as.list(paste0(c('species',
                                                     'site',
                                                     'species_site',
                                                     'project'),
                                                   '_term')),
                   MoreArgs = list(value = x_mu, bayesfit_summary = out$summaries))  %>% 
    t() %>% 
    'rownames<-'(c('species', 'site', 'species_site', 'project'))

getresult(out$summaries, 'project') %>% 
    'rownames<-'(fit_data[['project']] %>% levels) %>% 
    data.frame %>% 
    rownames_to_column() %>% 
    as_data_frame %>% 
    mutate(project = factor(rowname) %>% forcats::fct_reorder(Mean)) %>% 
    ggplot() + 
        aes(x = project, y = Mean) + 
        geom_point()
