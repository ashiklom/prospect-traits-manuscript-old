#' @export
jags_sumsq_table <- function(jags_result, effect_list, ...) {
    attribute <- jags_result[['input_data']][paste0(effect_list, '.ind')]
    attribute_names <- paste0(effect_list, '.term')
    sumsq_table <- mapply(
                jags_sumsq,
                attribute = attribute,
                attribute_name = attribute_names,
                MoreArgs = list(value = jags_result[['input_data']][['x_mu']],
                                bayesfit_summary = jags_result[['summaries']])
                         ) %>% 
        t() %>% 
        as_data_frame() %>% 
        mutate(effect = effect_list)
    return(sumsq_table)
}

