factor.sunshade <- function(sunshade, canopyposition, projectcode) {
    # These projects are assumed to only sample sun leaves
    sun_projects <- c('ngee_tropics', 'ngee_arctic', 'lopex', 'angers', 'accp')
    sunshade <- case_when(!is.na(sunshade) ~ sunshade,
              is.na(canopyposition) & projectcode %in% sun_projects ~ 'sun',
              canopyposition == 'T' ~ 'sun',
              canopyposition %in% c('M','B') ~ 'shade',
              TRUE ~ NA_character_)
    sunshade <- factor(sunshade, c('sun', 'shade'))
    return(sunshade)
}

factor.leaf_type <- function(leaf_type, projectcode) {
    leaf_type <- case_when(!is.na(leaf_type) ~ leaf_type,
                           projectcode == 'wu_brazil' ~ 'broad',
                           TRUE ~ NA_character_)
    leaf_type <- factor(leaf_type, c('broad', 'needle'))
    return(leaf_type)
}

factor.myco_asso <- function(myco_asso) {
    factor(myco_asso) %>% 
        fct_recode('Arbuscular' = 'AM',
                   'Arbuscular' = 'AM + NM',
                   'Ecto' = 'ECM',
                   'Ecto' = 'EM',
                   'Either' = 'AM + ECM',
                   'Ericoid' = 'ERM',
                   'No association' = 'NM') %>% 
        fct_collapse('Arbuscular' = 'Arbuscular',
                     'Ecto' = c('Ecto', 'Either'),
                     'Other/None' = c('Ericoid', 'No association')) %>% 
        fct_relevel('Arbuscular', 'Ecto', 'Other/None')
}

factor.nitrogen_fixer <- function(nitrogen_fixer) {
    factor(nitrogen_fixer, c(0, 1)) %>% 
        fct_recode('No N fixation' = '0', 'N-fixer' = '1')
}

factor.growth_form <- function(growth_form) {
    fct_relevel(factor(growth_form), 'tree')
}

factor.woodiness <- function(growth_form) {
    fct_collapse(growth_form, 
                 'woody' = c('tree', 'shrub'),
                 'non-woody' = c('graminoid', 'herb', 'lichen', 'vine')
                 )
}

factor.shade_tolerance <- function(shade_tolerance) {
    fct_relevel(factor(shade_tolerance), 'tolerant', 'intermediate', 'intolerant')
}
