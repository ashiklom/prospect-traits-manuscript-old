fig.biomes <- function(site_sample_counts, site_clim_means, biome_polygons, biome_labels) {
    biome_colors <- biome_labels[['color']]
    names(biome_colors) <- biome_labels[['biome']]
    site_info <- dplyr::left_join(site_sample_counts, site_clim_means)
    ggplot() + 
        xlab('Mean annual precipitation (cm)') +
        ylab(expression(paste('Mean annual temperature (', degree, 'C)'))) +
        geom_polygon(data = biome_polygons, 
                     mapping = aes(x = prec_mm, y = temp_degC, fill = biome), 
                     size = 0.5, alpha = 0.2, color = 'black') + 
        geom_point(data = site_info, aes(x = MAP, y = MAT, size = n, color = projectcode)) + 
        geom_label(data = biome_labels, aes(x = x, y = y, label = biome_label)) + 
        scale_fill_manual(values = biome_colors) + 
        scale_color_brewer(palette = 'Paired') +
        theme_bw() +
        guides(fill = FALSE,
            color = guide_legend(title = 'Project', 
                                    override.aes = list(size = 4)))
} 

fig.sitemap <- function(site_clim_means) {
    ggplot(site_clim_means) + 
        aes(x = site_lon, y = site_lat, fill = projectcode) +
        borders('world') +
        coord_cartesian(xlim = c(-180, 25), ylim = c(-15, 75)) +
        geom_point(pch = 21, color = 'black', size = 2.5) + 
        scale_fill_brewer(palette = 'Paired') + 
        theme_bw() +
        theme(axis.title.x = element_blank(),
              axis.title.y = element_blank()) + 
        guides(fill = guide_legend(title = 'Project', override.aes = list(size = 4)))
}

fig.site_biome <- function(site_sample_counts, site_clim_means, biome_polygons, biome_labels) {
    sitemap <- fig.sitemap(site_clim_means)
    biomes <- fig.biomes(site_sample_counts, site_clim_means, biome_polygons, biome_labels)
    gridExtra::grid.arrange(sitemap + theme(legend.position = 'bottom'),
                            biomes + 
                                theme(legend.position = 'bottom') + 
                                guides(color = FALSE))
}

#spectra_info %>%
    #left_join(site_info) %>%
    #count(sitecode, site_lat, site_lon, temp, prec, projectcode) %>%
    #ungroup() %>%
    #group_by(projectcode) %>%
    #mutate(nproj = sum(n)) %>%
    #ungroup() %>%
    #mutate(projectcode = forcats::fct_reorder(projectcode, nproj)) %>% 
    #arrange(desc(nproj))


#sitemap <- ggplot(sitemap_dat) + 
    #aes(x = site_lon, y = site_lat, fill = projectcode) +
    #borders('world') +
    #coord_cartesian(xlim = c(-180, 25), ylim = c(-15, 75)) +
    #geom_point(pch = 21, color = 'black', size = 2.5) + 
    #scale_fill_brewer(palette = 'Paired') + 
    #theme_bw() +
    #theme(axis.title.x = element_blank(),
          #axis.title.y = element_blank()) + 
    #guides(fill = guide_legend(title = 'Project', 
                               #override.aes = list(size = 4)))
#plot(sitemap)
#ggsave(filename = 'figures/sitemap.png', plot = sitemap, width = 8, height = 4)

#plot(biomeplt)
#ggsave(filename = 'figures/biomes.png', plot = biomeplt, width = 10, height = 6)

#site_biome <- 
    #gridExtra::grid.arrange(sitemap + theme(legend.position = 'bottom'),
                            #biomeplt + 
                                #theme(legend.position = 'bottom') + 
                                #guides(color = FALSE))
#plot(site_biome)
#ggsave(plot = site_biome, 
       #filename = 'figures/site_biome.png', width = 8, height = 11)
