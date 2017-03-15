jags_model <- function(effect_list) {
    prior <- c('y0 ~ dnorm(0, 0.01)', 
               'y_tau ~ dgamma(0.01, 0.01)')
    if (length(effect_list) == 0) {
        linear_model <- 'y0'
    } else {
        effect_terms <- sprintf('%1$s.term[%1$s.ind[i]]', effect_list)
        effect_priors <- sprintf('for (i in 1:%1$s.n) {%1$s.term[i] ~ dnorm(0, 0.01)}',
                                 effect_list)
        linear_model <- paste0('    y[i] = ', 
                               paste(c('y0', effect_terms), collapse = ' + '))
        prior <- c(prior, effect_priors)
    }
    code_vec <- c('model{',
                  'for (i in 1:length(x_mu)) {',
                  '    x_mu[i] ~ dnorm(x[i], x_tau[i])',
                  '    x[i] ~ dnorm(y[i], y_tau)',
                  linear_model, 
                  '}',
                  prior,
                  '}')
    code_string <- paste(code_vec, collapse = '\n')
    return(code_string)
}

jags_data <- function(data, effect_list, mu_var, sigma_var) {
    x_mu <- data[[mu_var]]
    x_tau <- data[[sigma_var]]^(-2)
    effects.ind <- lapply(effect_list, getElement, object = data) %>% 
        lapply(as.numeric)
    names(effects.ind) <- paste0(effect_list, '.ind')
    effects.n <- lapply(effects.ind, max)
    names(effects.n) <- paste0(effect_list, '.n')
    data_list <- c(list(x_mu = x_mu, x_tau = x_tau), effects.ind, effects.n)
    return(data_list)
}

jags_monitors <- function(effect_list) {
    c('y0', 'y_tau', paste(effect_list, 'term', sep = '.'), 'dic')
}

jags_sample <- function(data, effect_list, response_var, ...) {
    mu_var <- paste0('parametermean_', response_var)
    sd_var <- paste0('parametersd_', response_var)
    model_code <- jags_model(effect_list)
    model_data <- jags_data(data, effect_list, mu_var, sd_var)
    model_monitors <- jags_monitors(effect_list)
    jags_result <- autorun.jags(model = model_code, 
                                data = model_data,
                                monitor = model_monitors,
                                startburnin = 1000,
                                startsample = 5000)
    jags_result$input_data <- model_data
    return(jags_result)
}

jags_sumsq <- function(value, attribute, bayesfit_summary, attribute_name) {
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

jags_sumsq_table <- function(jags_result, effect_list, response_var, ...) {
    attribute <- jags_result$input_data[paste0(effect_list, '.ind')]
    attribute_names <- paste0(effect_list, '.term')
    sumsq_table <- mapply(
                jags_sumsq,
                attribute = attribute,
                attribute_name = attribute_names,
                MoreArgs = list(value = jags_result$input_data$x_mu,
                                bayesfit_summary = jags_result$summaries)
                         ) %>% 
        t() %>% 
        as_data_frame() %>% 
        mutate(effect = effect_list)
    return(sumsq_table)
}

r1 <- jags_complete(fit_data, 'project', 'N')

