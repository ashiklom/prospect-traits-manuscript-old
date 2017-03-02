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


## Correlation analysis
cor_data <- results %>% 
    select(samplecode, parameter, parametermean) %>%
    spread(parameter, parametermean)

resid_cor_dat <- valid_dat %>%
    select(samplecode, parameter, resid) %>%
    mutate(parameter = paste(parameter, 'resid', sep = '_')) %>%
    spread(parameter, resid) %>%
    left_join(cor_data)

resid_cor_mat <- resid_cor_dat %>%
    select(-samplecode) %>%
    as.matrix()

cor_matrix <- cor(resid_cor_mat, use = 'pairwise.complete.obs')

corrplot::corrplot.mixed(cor_matrix, lower = 'ellipse', upper = 'number', 
                         main = 'Residual correlations', 
                         mar = c(2, 0, 2, 1))

mat_sub <- na.omit(resid_cor_mat[,c(-2, -3, -4)])
test <- vegan::metaMDS(mat_sub)

dst <- dist(mat_sub)
fit <- MASS::isoMDS(dst, k = 4)

#pcafit <- princomp(na.omit(resid_cor_mat[,c(-2, -3, -4)]), cor = TRUE)
#biplot(pcafit)

#plot(Cab_resid ~ N, data = resid_cor_dat)
#plot(Cab_resid ~ Cab, data = resid_cor_dat)
#plot(Cm_resid ~ Cw, data = resid_cor_dat)
