#' @export
jags_monitors <- function(effect_list) {
    c('y0', 'y_tau', paste(effect_list, 'term', sep = '.'), 'dic')
}
