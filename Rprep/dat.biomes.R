dat.biomes <- function() {
    biomes <- read_csv('extdata/biomes.csv') %>%
        mutate(temp = x, prec = y * 10)

    biome_colors <- c("Subtropical desert" = "navajowhite3",
                    'Temperate grassland desert' = "darkgoldenrod1",
                    'Woodland shrubland' = "sienna",
                    'Temperate forest' = "darkolivegreen4",
                    'Boreal forest' = "darkseagreen3",
                    'Temperate rain forest' = "forestgreen",
                    'Tropical rain forest' = "darkgreen",
                    'Tropical forest savanna' = "olivedrab",
                    'Tundra' = "gray")

    biome_labs <- tribble(
        ~biome, ~x, ~y,
        'Boreal forest', 110, 0,
        'Subtropical desert', 42.78, 26,
        'Temperate forest', 160, 10,
        'Temperate\ngrassland\ndesert', 16.64, 6.86,
        'Temperate rain forest', 250, 12.5,
        'Tropical rain forest', 340, 24,
        'Tropical forest savanna', 150.53, 22.53,
        'Tundra', 30, -10,
        'Woodland\nshrubland', 95, 16)

    return(list(biomes = biomes, 
                biome_colors = biome_colors, 
                biome_labs = biome_labs))
}
