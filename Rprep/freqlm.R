#' @export
freqlm <- function(data, variables, 
                   response = 'log(parametermean)',
                   coef_exclude = c('species', 'family', 'genus')) {
    variables_rxp <- paste(variables, collapse = '|')
    form <- paste(response, '~', paste(variables, collapse = ' + '))
    linfit <- data %>% 
        nest(-parameter) %>% 
        mutate(model = map(data, function(data) lm(formula(form), data = data)),
               coefs = map(model, broom::tidy),
               ANOVA = map(model, anova) %>% map(broom::tidy))

    # Partitioning variability
    linfit %>% 
        unnest(ANOVA) %>% 
        mutate(term = factor(term, rev(c(variables, 'Residuals')))) %>% 
        group_by(parameter) %>% 
        mutate(normss = sumsq/sum(sumsq)) %>% 
        ungroup() %>% 
        ggplot() + 
        aes(x = parameter, y = normss, fill = term) + 
        geom_bar(stat = 'identity') + 
        scale_fill_brewer(palette = 'Paired') -> variance_plot

    # Coefficients
    linfit %>% 
        unnest(coefs) %>% 
        mutate(group = stringr::str_extract(term, variables_rxp) %>% 
                factor(levels = variables),
               contrast = stringr::str_replace(term, variables_rxp, '')) %>% 
        filter(!group %in% c(coef_exclude, NA)) %>% 
        ggplot() +
        aes(x = contrast, 
            y = estimate,
            ymin = estimate - std.error, 
            ymax = estimate + std.error) +
        geom_pointrange() +
        geom_hline(yintercept = 0, color = 'red', linetype = 'dashed') +
        facet_grid(parameter ~ group, scales = 'free', space = 'free_x') -> coef_plot

    return(list(table = linfit, 
                variance_plot = variance_plot, 
                coef_plot = coef_plot))
}
