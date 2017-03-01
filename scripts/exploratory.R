valid_dat %>% 
    mutate(badscale = abs(scaledresid) > 1,
           badnorm = normresid > 1) %>%
    group_by(parameter) %>%
    summarize(
        fracscale = mean(badscale) * 100,
        countscale = sum(badscale),
        fracnorm = mean(badnorm) * 100,
        countnorm = sum(badnorm)
              ) %>%
    arrange(desc(fracnorm))
