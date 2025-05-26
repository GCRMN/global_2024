# 1. Required packages ----

library(extrafont)
library(scico)

# 2. Set the default font family ----

windowsFonts("Open Sans" = windowsFont("Open Sans"))
windowsFonts("MinionPro-Semibold" = windowsFont("MinionPro-Semibold"))

font_choose_graph <- "Open Sans"
font_choose_map <- "MinionPro-Semibold"

fig_resolution <- 300

# 3. Set the colors ----

palette_first <- scico(5, palette = "oslo", begin = 0.8, end = 0)
palette_second <- c("#fac484", "#f8a07e", "#ce6693", "#a059a0", "#5c53a5")

# palette_second taken from https://carto.com/carto-colors/ (SunsetDark)

# 4. CRS in meters ----

crs_meters <- "+proj=eqc +lat_ts=0 +lat_0=0 +lon_0=160 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs"
crs_180 <- "+proj=eqc +lat_ts=0 +lat_0=0 +lon_0=155 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs"
