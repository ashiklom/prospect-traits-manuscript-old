convert_traits_units <- function(value, trait) {
    case_when(grepl('(chl|car).*_per_area', trait) ~
                udunits2::ud.convert(value, 'kg m-2', 'ug cm-2'),
              grepl('(water_thickness|mass_per_area)', trait) ~ 
                udunits2::ud.convert(value, 'kg m-2', 'g cm-2'),
              TRUE ~ value)
}

dat.traits_raw_long <- function(specdb_path) {
    specdb <- src_sqlite(specdb_path)
    tbl(specdb, 'trait_data') %>% 
        select(samplecode, trait, traitvalue) %>% 
        filter(samplecode != 'nasa_fft|PB02_ABBA_TN|2008') %>% 
        collect(n = Inf) %>% 
        mutate(trait = factor(trait),
               traitvalue = convert_traits_units(traitvalue, trait)) %>% 
        ## TODO: Fix this in curated leafspec post-processing
        group_by(samplecode, trait) %>% 
        summarize(traitvalue = mean(traitvalue, na.rm = TRUE)) %>% 
        ungroup() %>% 
        verify(is_unique(samplecode, trait))
}

dat.traits_raw_wide <- function(traits_raw_long) {
    spread(traits_raw_long, trait, traitvalue)
}

dat.traits_all_long <- function(results_all_long, traits_raw_long) {
    results_all_long %>% 
        inner_join(traits_raw_long) %>% 
        verify(is_unique(samplecode, modelname, parameter, trait))
}

dat.traits_wide <- function(results_wide, traits_raw_wide) {
    results_wide %>% 
        inner_join(traits_raw_wide) %>% 
        verify(is_unique(samplecode))
}

dat.traits_long <- function(results_long, traits_raw_long) {
    results_long %>% 
        inner_join(traits_raw_long) %>% 
        verify(is_unique(samplecode, parameter, trait))
}

dat.traits_wide <- function(results_wide, traits_raw_wide){
    results_wide %>% 
        inner_join(traits_raw_wide) %>% 
        verify(is_unique(samplecode))
}

dat.traits_all_valid_long <- function(traits_all_long) {
    traits_all_long %>% 
        filter((parameter == 'Cab' & trait == 'leaf_chltot_per_area') |
               (parameter == 'Car' & trait == 'leaf_cartot_per_area') |
               (parameter == 'Cw' & trait == 'leaf_water_thickness') |
               (parameter == 'Cm' & trait == 'leaf_mass_per_area')) %>% 
        group_by(parameter, modelname) %>% 
        mutate(resid = parametermean - traitvalue,
            normresid = resid / traitvalue,
            scaledresid = resid / mean(traitvalue)) %>% 
        ungroup() %>% 
        verify(is_unique(samplecode, modelname, parameter, trait))
}

dat.traits_valid_long <- function(traits_all_valid_long, model) {
    traits_all_valid_long %>% 
        filter(modelname == model) %>% 
        verify(is_unique(samplecode, parameter, trait))
}
