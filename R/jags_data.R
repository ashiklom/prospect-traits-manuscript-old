#' @export
jags_data <- function(data, effect_list, mu_var, sigma_var) {
    x_mu <- data[[mu_var]]
    x_tau <- data[[sigma_var]]^(-2)
    stopifnot(length(x_mu) > 0, length(x_tau) > 0)
    effects.ind <- lapply(effect_list, getElement, object = data) %>% 
        lapply(as.numeric) %>% 
        lapply(`-`, 1)
    names(effects.ind) <- paste0(effect_list, '.ind')
    effects.n <- lapply(effects.ind, max)
    names(effects.n) <- paste0(effect_list, '.n')
    data_list <- c(list(x_mu = x_mu, x_tau = x_tau), effects.ind, effects.n)
    return(data_list)
}

