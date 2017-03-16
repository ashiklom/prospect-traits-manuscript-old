#' @export
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
