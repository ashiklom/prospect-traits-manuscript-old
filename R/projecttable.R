#' @export
projecttable <- function(spectra_info){
    byproject <- spectra_info %>%
        group_by(projectcode, projectdescription) %>%
        summarize(sites = n_distinct(sitecode),
                  species = n_distinct(speciescode),
                  samples = n_distinct(samplecode)) %>%
        arrange(desc(samples))

    totals <- spectra_info %>%
        summarize(sites = n_distinct(sitecode),
                  species = n_distinct(speciescode),
                  samples = n_distinct(samplecode)) %>%
    mutate(projectcode = '', projectdescription = 'Total')

    projecttable <- bind_rows(byproject, totals) %>%
        rename(code = projectcode,
            Description = projectdescription,
            '# sites' = sites, 
            '# species' = species,
            '# spectra' = samples)

    return(projecttable)
}


