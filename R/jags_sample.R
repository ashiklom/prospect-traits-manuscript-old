#' @export
jags_sample <- function(data, effect_list, response_var,
                        mean_prefix = 'mean', 
                        sd_prefix = 'sd',
                        suffix = '5B',
                        sep = '_', 
                        ...) {
    runjags::runjags.options(force.summary = TRUE, 
                             modules = 'glm')
    mu_var <- stringr::str_c(mean_prefix, response_var, suffix, sep = sep)
    sd_var <- stringr::str_c(sd_prefix, response_var, suffix, sep = sep)
    message('Response mean: ', mu_var)
    message('Response SD: ', sd_var)
    message('Effects: ', paste(effect_list, collapse = ', '))
    model_code <- jags_model(effect_list)
    model_data <- jags_data(data, effect_list, mu_var, sd_var)
    model_monitors <- jags_monitors(effect_list)
    jags_result <- runjags::autorun.jags(model = model_code, 
                                         data = model_data,
                                         monitor = model_monitors,
                                         startburnin = 1000,
                                         startsample = 5000,
                                         ...)
    jags_result$input_data <- model_data
    return(jags_result)
}

