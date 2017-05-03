dat.site_sample_counts <- function(spectra_info) {
    dplyr::count(spectra_info, sitecode, projectcode)
}
