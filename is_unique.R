# Function for verifying that a tibble with the specified rows
is_unique <- function(...) {
    tibble::tibble(...) %>%
        dplyr::count(...) %>%
        dplyr::filter(n > 1) %>%
        nrow() %>%
        !.
}
