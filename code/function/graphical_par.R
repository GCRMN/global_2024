# 1. Required packages ----

library(scico)
library(sysfonts)
library(showtext)
library(showtextdb)

# 2. Set the default font family ----

font_add_google("Open Sans", "opsan") # Add a font from Google Font

font_choose_graph <- "opsan"
font_choose_map <- "opsan"

fig_resolution <- 300

showtext::showtext_auto()
showtext::showtext_opts(dpi = fig_resolution)

# 3. Set the colors ----

palette_first <- scico(5, palette = "oslo", begin = 0.8, end = 0)
palette_second <- c("#fac484", "#f8a07e", "#ce6693", "#a059a0", "#5c53a5")

# palette_second taken from https://carto.com/carto-colors/ (SunsetDark)

palette_regions <- tibble(region = c("Australia", "Brazil", "Caribbean", "EAS", "ETP",
                                    "PERSGA", "Pacific", "ROPME", "South Asia", "WIO"),
                         color = c("#9e0142", "#66c2a5", "#f46d43", "#fdae61", "#fee08b",
                                   "#e6f598", "#abdda4", "#d53e4f", "#3288bd", "#5e4fa2"))
