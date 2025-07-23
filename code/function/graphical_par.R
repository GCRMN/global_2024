# 1. Required packages ----

library(scico)
library(sysfonts)
library(showtext)

# 2. Set the default font family ----

font_add_google("Open Sans", "opsan") # Install a font from Google Font

font_choose_graph <- "opsan"
font_choose_map <- "opsan"

fig_resolution <- 300

showtext::showtext_auto()
showtext::showtext_opts(dpi = fig_resolution)

# 3. Set the colors ----

palette_first <- scico(5, palette = "oslo", begin = 0.8, end = 0)
palette_second <- c("#fac484", "#f8a07e", "#ce6693", "#a059a0", "#5c53a5")

# palette_second taken from https://carto.com/carto-colors/ (SunsetDark)
