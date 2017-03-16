#' @export
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

