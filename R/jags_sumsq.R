#' @export
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

