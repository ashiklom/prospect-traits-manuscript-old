# Useful global variables
params <- function(version = 'D', stats = FALSE) {
    version <- as.character(version)
    params <- switch(version,
           `4` = c('N', 'Cab', 'Cw', 'Cm'),
           `5` = c('N', 'Cab', 'Car', 'Cw', 'Cm'),
           `5B` = c('N', 'Cab', 'Car', 'Cbrown', 'Cw', 'Cm'),
           `D` = c('N', 'Cab', 'Car', 'Canth', 'Cbrown', 'Cw', 'Cm'))

    if (stats) {
        params <- c(params, 'deviance', 'neff', 'residual')
    }
    return(params)
}

nparam <- function(...) {
    length(params(...))
}

models <- c('PROSPECT 4', 'PROSPECT 5', 'PROSPECT 5B')
