# 1. Load packages ----

library(tidyverse) # Core tidyverse packages
library(patchwork)
library(ggtext)
library(sf)
sf_use_s2(FALSE)
library(colorspace)
library(deSolve)
library(tidybayes)
library(xkcd)

# 2. Source functions ----

source("code/function/graphical_par.R")
source("code/function/theme_graph.R")

# 3. Load data ----

load("data/model-results.RData")

# 4. Figure 1 - Hard coral cover ----

## 4.1 Load data ----

data_models_hc <- data_models |> 
  filter(category == "Hard coral" & level == "global") |> 
  mutate(color = "#1A497C")

data_languages <- tibble(language = c("EN",
                                      "SP",
                                      "FR"),
                         y_axis = c("Hard coral cover (%)",
                                    "Cobertura bentónica (%)",
                                    "Couverture corallienne (%)"),
                         x_axis = c("Year",
                                    "Año",
                                    "Année"),
                         title = c("Changes in <span style='color:#FFFFFF'>hard coral cover</span> at the global<br>scale from 1980 to 2024",
                                   "Cambio en la cobertura de <span style='color:#FFFFFF'>coral pétreo</span> a escala<br>mundial entre 1980 y 2024",
                                   "Évolution du recouvrement en <span style='color:#FFFFFF'>coraux durs</span> à l'échelle<br>mondiale de 1980 à 2024"),
                         subtitle = c("The bold line represents the median, while the<br>ribbon indicates the 95% credible interval",
                                      "La línea gruesa representa la mediana, mientras que las<br>bandas indican los intervalos creíbles al 95 %",
                                      "La ligne en gras représente la médiane, tandis que les bandes<br>indiquent les intervalles de crédibilité à 95 %"))

## 4.2 Create the function ----

plot_exsum_hc <- function(language_i){
  
  data_languages_i <- data_languages |> 
    filter(language == language_i)
  
  ggplot(data = data_models_hc, aes(x = year, fill = color, color = color)) +
    geom_ribbon(aes(ymin = lower_ci_95, ymax = upper_ci_95), alpha = 0.35, color = NA) +
    geom_line(aes(y = mean)) +
    scale_color_identity() +
    scale_fill_identity() +
    theme_graph() +
    theme(panel.background = element_rect(fill = "transparent", colour = NA),
          plot.background = element_rect(fill = "transparent", colour = NA),
          plot.title = element_markdown(color = "black", size = 18, lineheight = 1.2),
          plot.subtitle = element_markdown(color = "#4D4D4D", size = 14)) +
    scale_x_continuous(breaks = seq(1980, 2025, 5),
                       limits = c(1979, 2026),
                       labels = seq(1980, 2025, 5)) +
    labs(x = unique(data_languages_i$x_axis), y = unique(data_languages_i$y_axis),
         title = unique(data_languages_i$title),
         subtitle = unique(data_languages_i$subtitle))# +
    #annotate(geom = "label", x = 1995, y = 25, label = "-9.5%", size = 5, family = font_choose_graph,
    #         fill = "#1A497C", color = "white")
  
  ggsave(paste0("figs/01_ex-summ/hard-coral_", str_to_lower(language_i), "_raw.pdf"), height = 6, width = 9)
  
}

## 4.3 Map over the function ----

map(unique(data_languages$language), ~plot_exsum_hc(language_i = .x))

# 5. Figure 2 - Macroalgal cover ----

## 5.1 Load data ----

data_models_ma <- data_models |> 
  filter(category == "Macroalgae" & level == "global") |> 
  mutate(color = "#40A6AA")

data_languages <- tibble(language = c("EN",
                                      "SP",
                                      "FR"),
                         y_axis = c("Macroalgal cover (%)",
                                    "Cobertura de macroalgas (%)",
                                    "Couverture en macroalgues (%)"),
                         x_axis = c("Year",
                                    "Año",
                                    "Année"),
                         title = c("Changes in <span style='color:#FFFFFF'>macroalgal cover</span> at the global<br>scale from 1980 to 2024",
                                   "Cambio en la cobertura de <span style='color:#FFFFFF'>macroalgas</span> a escala<br>mundial entre 1980 y 2024",
                                   "Évolution du recouvrement en <span style='color:#FFFFFF'>macroalgues</span> à l'échelle<br>mondiale de 1980 à 2024"),
                         subtitle = c("The bold line represents the median, while the<br>ribbon indicates the 95% credible interval.",
                                      "La línea gruesa representa la mediana, mientras que las<br>bandas indican los intervalos creíbles al 95 %",
                                      "La ligne en gras représente la médiane, tandis que les bandes<br>indiquent les intervalles de crédibilité à 95 %"))

## 5.2 Create the function ----

plot_exsum_ma <- function(language_i){
  
  data_languages_i <- data_languages |> 
    filter(language == language_i)
  
  ggplot(data = data_models_ma, aes(x = year, fill = color, color = color)) +
    geom_ribbon(aes(ymin = lower_ci_95, ymax = upper_ci_95), alpha = 0.35, color = NA) +
    geom_line(aes(y = mean)) +
    scale_color_identity() +
    scale_fill_identity() +
    theme_graph() +
    theme(panel.background = element_rect(fill = "transparent", colour = NA),
          plot.background = element_rect(fill = "transparent", colour = NA),
          plot.title = element_markdown(color = "black", size = 18, lineheight = 1.2),
          plot.subtitle = element_markdown(color = "#4D4D4D", size = 14)) +
    scale_x_continuous(breaks = seq(1980, 2025, 5),
                       limits = c(1979, 2026),
                       labels = seq(1980, 2025, 5)) +
    scale_y_continuous(limits = c(3, 9)) +
    labs(x = unique(data_languages_i$x_axis), y = unique(data_languages_i$y_axis),
         title = unique(data_languages_i$title),
         subtitle = unique(data_languages_i$subtitle))# +
    #annotate(geom = "label", x = 1990, y = 7, label = "+44.1%", size = 5, family = font_choose_graph,
    #         fill = "#40A6AA", color = "white")
  
  ggsave(paste0("figs/01_ex-summ/macroalgae_", str_to_lower(language_i), "_raw.pdf"), height = 6, width = 9)
  
}

## 5.3 Map over the function ----

map(unique(data_languages$language), ~plot_exsum_ma(language_i = .x))

# 6. Figure 3 - Regional trends ----

## 6.1 Create the data ----

pal <- colorRampPalette(c("#C44D56", "#E6E6E6", "#013C5E"))(101)

data_arrow <- tibble(region = c("Australia", "Brazil", "Caribbean", "EAS", "ETP", "Pacific", "RSGA", "ROPME", "South Asia", "WIO", "Global"),
                     position = c("Bottom", "Bottom", "Top", "Top", "Bottom", "Bottom", "Top", "Top", "Top", "Bottom", "Bottom"),
                     x = 1,
                     y = 1,
                     change = c(-10.9, 0, -43.4, 0, 0, -18.3, 0, -48.7, 16.2, 43.0, -9.5),
                     color = pal[pmax(1, pmin(101, round(change) + 51))],
                     change_label = case_when(change == 0 ~ "NC",
                                              change < 0 ~ paste0(change, "%"),
                                              change > 0 ~ paste0("+", change, "%"))) |> 
  mutate(angle = 90 * change / 100,
         length = 0.2,
         xstart = x - length * cos(angle * pi / 180),
         ystart = y - length * sin(angle * pi / 180),
         xend = x + length * cos(angle * pi / 180),
         yend = y + length * sin(angle * pi / 180))

## 6.2 Make the map ----

crs_selected <- st_crs("+proj=eqc +x_0=0 +y_0=0 +lat_0=0 +lon_0=160")

offset <- 180 - 160

polygon <- st_polygon(x = list(rbind(
  c(-0.0001 - offset, 90),
  c(0 - offset, 90),
  c(0 - offset, -90),
  c(-0.0001 - offset, -90),
  c(-0.0001 - offset, 90)))) %>%
  st_sfc() %>%
  st_set_crs(4326)

data_countries <- st_read("data/01_maps/01_raw/03_natural-earth/ne_110m_land/ne_110m_land.shp")
  
data_countries <- st_crop(x = data_countries, 
                          y = st_as_sfc(st_bbox(c(xmin = -180, ymin = -48, xmax = 180, ymax = 48), crs = 4326))) %>%
  st_difference(polygon) %>%
  st_transform(crs = crs_selected)

data_region <- read_sf("data/01_maps/02_clean/03_regions/gcrmn_regions.shp")

data_region_pac <- data_region %>% 
  filter(region == "Pacific") %>% 
  st_difference(polygon) %>% 
  st_transform(crs = crs_selected)

data_region_pac %<>% # Special pipe from magrittr
  st_buffer(10) %>% # To join polygon (remove vertical line)
  nngeo::st_remove_holes(.)

data_region <- data_region %>% 
  filter(region != "Pacific") %>% 
  st_difference(polygon) %>% 
  st_transform(crs = crs_selected) %>% 
  bind_rows(., data_region_pac) %>% 
  mutate(region = case_when(region == "PERSGA" ~ "RSGA",
                            TRUE ~ region)) %>%
  left_join(., data_arrow %>% select(region, color))

data_tropics <- tibble(tropic = c("Cancer", "Cancer", "Equator", "Equator", "Capricorn", "Capricorn"),
                       lat = c(23.4366, 23.4366, 0, 0, -23.4366, -23.4366),
                       long = c(-180, 180, -180, 180, -180, 180)) %>% 
  st_as_sf(coords = c("long", "lat"), crs = 4326) %>%
  group_by(tropic) %>%
  dplyr::summarize(do_union = FALSE) %>%
  st_cast("LINESTRING") %>% 
  st_difference(polygon) %>%
  st_transform(crs = crs_selected)

plot_map <- ggplot() +
  geom_sf(data = data_tropics, linetype = "dashed", col = "lightgrey", linewidth = 0.3) +
  geom_sf(data = data_region, aes(fill = color), color = "black",
          show.legend = FALSE, alpha = 1, linewidth = 0.25) +
  scale_fill_identity() +
  geom_sf(data = data_countries, fill = "#2f3542", color = "#2f3542") +
  coord_sf(ylim = c(-5000000, 5000000), expand = FALSE,
           label_axes = list(top = "E", left = "N", right = "N", bottom = "E")) +
  theme(panel.border = element_rect(fill = NA, color = "black"),
        panel.background = element_rect(fill = "white"),
        panel.grid = element_blank(),
        plot.background = element_rect(fill = "transparent", color = NA),
        axis.ticks = element_blank(),
        axis.text = element_blank())

plot_spacer() + plot_map + plot_spacer() + 
  plot_layout(ncol = 1, heights = c(0.5, 1, 0.5)) + 
  plot_annotation(title = "Comparison of relative changes in hard coral cover between 1980-2009\nto 2020-2024 across the ten GCRMN regions",
                  theme = theme(plot.title = element_text(size = 18, hjust = 0, vjust = 1,
                                                          lineheight = 1.2, margin = margin(b = 7.5),
                                                          family = font_choose_graph))) &
  theme(plot.background  = element_rect(fill = "transparent", colour = NA),
        panel.background = element_rect(fill = "transparent", colour = NA))

ggsave("figs/01_ex-summ/map_regions_raw.png", bg = "transparent", height = 6, width = 9, dpi = 300)

ggsave("figs/01_ex-summ/map_regions_raw.pdf", bg = "transparent", height = 6, width = 9)

if(FALSE){

## 6.2 Make the map (full) ----

  data_land <- st_read("data/01_maps/01_raw/03_natural-earth/ne_110m_land/ne_110m_land.shp")
  
  crs_selected <- st_crs("+proj=eqc +x_0=0 +y_0=0 +lat_0=0 +lon_0=160")
  
  offset <- 180 - 160
  
  polygon <- st_polygon(x = list(rbind(
    c(-0.0001 - offset, 90),
    c(0 - offset, 90),
    c(0 - offset, -90),
    c(-0.0001 - offset, -90),
    c(-0.0001 - offset, 90)))) %>%
    st_sfc() %>%
    st_set_crs(4326)
  
  data_land <- data_land %>%
    st_difference(polygon) %>%
    st_simplify() %>%
    st_collection_extract("POLYGON") %>%
    st_cast("POLYGON", warn = FALSE) %>%
    st_transform(crs = crs_selected) %>%
    st_make_valid()

  data_region <- st_read("data/01_maps/02_clean/03_regions/gcrmn_regions.shp")
  
  data_region_pac <- data_region %>% 
    filter(region == "Pacific") %>% 
    st_difference(polygon) %>% 
    st_transform(crs = crs_selected)
  
  data_region_pac %<>% # Special pipe from magrittr
    st_buffer(10) %>% # To join polygon (remove vertical line)
    nngeo::st_remove_holes(.)
  
  data_region <- data_region %>% 
    filter(region != "Pacific") %>% 
    st_difference(polygon) %>% 
    st_transform(crs = crs_selected) %>% 
    bind_rows(., data_region_pac)
  
  plot_i <- ggplot() +
    geom_sf(data = data_region, fill = "grey99") +
    geom_sf(data = data_land, fill = "#2f3542", color = "#2f3542") +
    coord_sf(expand = FALSE,
             label_axes = list(top = "E", left = "N", right = "N", bottom = "E")) +
    theme(panel.border = element_rect(fill = NA, color = "black"),
          panel.background = element_rect(fill = "white"),
          panel.grid = element_blank(),
          plot.background = element_rect(fill = "transparent", color = NA),
          axis.ticks = element_blank(),
          axis.text = element_blank())
  
  ggsave("figs/01_ex-summ/map_regions_full.svg", height = 9, width = 12, device = svglite::svglite, bg = "transparent")
  
}

## 6.3 Arrows ----

plot_arrow <- function(region_i){
  
  plot_i <- ggplot(data = data_arrow |> filter(region == region_i)) +
    geom_point(aes(x = x, y = y, color = color), size = 35, shape = 19) +
    scale_color_identity() +
    geom_segment(aes(x = xstart, y = ystart, xend = x, yend = y, color = "black"),
                 linewidth = 1) +
    geom_segment(aes(x = x, y = y, xend = xend, yend = yend, color = "black"),
                 linewidth = 1, arrow = arrow(length = unit(0.25, "cm"), type = "closed", angle = 25)) +
    scale_x_continuous(limits = c(0,2), expand = expansion(mult = 0)) +
    scale_y_continuous(limits = c(0,2), expand = expansion(mult = 0)) +
    coord_equal(clip = "off") +
    theme_void() +
    theme(legend.position = "none",
          plot.margin = margin(0, 0, 0, 0)) 
  
  ggsave(paste0("figs/01_ex-summ/arrow_", str_replace_all(str_to_lower(region_i), " ", "-"), ".png"),
         bg = "transparent", height = 3, width = 3)
  
  data_arrow <- tibble(region = c("Australia", "Brazil", "Caribbean", "EAS", "ETP", "Pacific", "RSGA", "ROPME", "South Asia", "WIO", "Global"),
                       position = c("Bottom", "Bottom", "Top", "Top", "Bottom", "Bottom", "Top", "Top", "Top", "Bottom", "Bottom"),
                       x = 1,
                       y = 1,
                       change = c(-10.9, 0, -43.4, 0, 0, -18.3, -12.5, -48.7, 16.2, 43.0, -9.5),
                       color = pal[pmax(1, pmin(101, round(change) + 51))],
                       change_label = case_when(change == 0 ~ "NC",
                                                change < 0 ~ paste0(change, "%"),
                                                change > 0 & region == "WIO" ~ paste0("+", change, "%*"),
                                                change > 0 ~ paste0("+", change, "%"))) |> 
    mutate(angle = 90 * change / 100,
           length = 1,
           xstart = x - length * cos(angle * pi / 180),
           ystart = y - length * sin(angle * pi / 180),
           xend = x + length * cos(angle * pi / 180),
           yend = y + length * sin(angle * pi / 180))
  
  plot_i <- ggplot(data = data_arrow |> filter(region == region_i)) +
    geom_point(aes(x = x+1, y = y), size = 25, shape = 21, color = "white", fill = "#0C2F3B") +
    geom_segment(aes(x = xstart+1, y = ystart, xend = x+1, yend = y), color = "white", linewidth = 1) +
    geom_segment(aes(x = x+1, y = y, xend = xend+1, yend = yend), color = "white",
                 linewidth = 1, arrow = arrow(length = unit(0.3, "cm"), type = "closed", angle = 25)) +
    geom_text(aes(x = x, y = y, label = change_label), family = font_choose_graph, size = 10, nudge_x = 3, hjust = 0) +
    scale_x_continuous(limits = c(0,4), expand = expansion(mult = 0)) +
    scale_y_continuous(limits = c(0,2), expand = expansion(mult = 0)) +
    coord_equal(clip = "off") +
    theme_void() +
    theme(legend.position = "none",
          plot.margin = margin(0, 120, 0, 0))
  
  ggsave(paste0("figs/02_part-1/fig_arrow-", str_replace_all(str_to_lower(region_i), " ", "-"), ".pdf"),
         bg = "transparent", height = 1, width = 2.5)
  
}

walk(unique(data_arrow$region), ~plot_arrow(region_i = .x))

# 7. Figure 4 - Future trajectories ----

data_simulation <- readRDS("data/02_misc/data_simulation.rds") |> 
  group_by(scenario, time) |>
  select(-draw) |>
  summarise_draws(median = median,
                  lower = ~quantile(., 0.025),
                  upper = ~quantile(., 0.975)) |>
  rename(lower = `2.5%`, upper = `97.5%`) |> 
  rename(year = time) |> 
  mutate(year = year+2023, across(c("median", "lower", "upper"), ~.x*100),
         scenario = case_when(scenario == "1years recovery period" ~ "1 year",
                              scenario == "2years recovery period" ~ "2 years",
                              scenario ==  "4years recovery period" ~ "4 years",
                              scenario == "10years recovery period" ~ "10 years"))

data_models_hc <- data_models |> 
  filter(category == "Hard coral" & level == "global")

data_languages <- tibble(language = c("EN",
                                      "SP",
                                      "FR"),
                         y_axis = c("Hard coral cover (%)",
                                    "Cobertura bentónica (%)",
                                    "Couverture corallienne (%)"),
                         x_axis = c("Year",
                                    "Año",
                                    "Année"),
                         title = c("Possible future trajectories of global hard coral<br>cover based on historical trend",
                                   "Posibles trayectorias futuras de la cobertura mundial de<br>corales duros basadas en la tendencia histórica",
                                   "Trajectoires futures possibles de la couverture mondiale en<br>coraux durs fondées sur la tendance historique"))

plot_exsum_trajectories <- function(language_i){
  
  data_languages_i <- data_languages |> 
    filter(language == language_i)
  
  plot_i <- ggplot() +
    geom_ribbon(data = data_models_hc, aes(x = year, ymin = lower_ci_80, ymax = upper_ci_80),
                alpha = 0.35, color = NA, fill = "#747d8c") +
    geom_ribbon(data = data_models_hc, aes(x = year, ymin = lower_ci_95, ymax = upper_ci_95),
                alpha = 0.45, color = NA, fill = "#747d8c") +
    geom_line(data = data_models_hc, aes(x = year, y = mean), color = "#0C2F3B") +
    geom_ribbon(data = data_simulation, aes(x = year, ymin = lower, ymax = upper, fill = scenario),
                alpha = 0.8, linewidth = 0.5, show.legend = FALSE) +
    scale_fill_manual(values = c("1 year" = palette_second[5],
                                  "2 years" = palette_second[4],
                                  "4 years" = palette_second[3],
                                  "10 years" = palette_second[2]),
                       name = "Recovery window",
                       guide = guide_legend(override.aes = list(alpha = 1, linewidth = 1))) +
    theme_graph() +
    theme(panel.background = element_rect(fill = "transparent", colour = NA),
          plot.background = element_rect(fill = "transparent", colour = NA),
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank(),
          axis.ticks.y = element_blank(),
          axis.text.y = element_blank(),
          panel.grid = element_blank(),
          axis.line.y = element_line(linewidth = 0.4),
          legend.position = c(0.06, 0.06),
          legend.justification = c(0, 0),
          legend.direction = "vertical",
          legend.background = element_blank(),
          legend.key = element_blank(),
          legend.title = element_text(family = font_choose_graph),
          legend.text = element_text(family = font_choose_graph),
          plot.title = element_markdown(color = "black", size = 18, lineheight = 1.2)) +
    scale_x_continuous(breaks = seq(1980, 2095, 10),
                       limits = c(1979, 2096),
                       labels = seq(1980, 2095, 10)) +
    scale_y_continuous(limits = c(0, 40)) +
    labs(x = unique(data_languages_i$x_axis), y = unique(data_languages_i$y_axis),
         title = unique(data_languages_i$title))
    
  ggsave(paste0("figs/01_ex-summ/trajectories_", str_to_lower(language_i), "_raw.pdf"), height = 6, width = 9)
  
}

map(unique(data_languages$language), ~plot_exsum_trajectories(language_i = .x))

# 8. Global map of GCRMN regions ----

## 8.1 Load data ----

crs_pacific <- "+proj=eqearth +lon_0=160 +datum=WGS84 +units=m +no_defs"

data_land <- st_read("data/01_maps/01_raw/03_natural-earth/ne_110m_land/ne_110m_land.shp") %>% 
  st_transform(4326) %>%
  st_break_antimeridian(lon_0 = 160) %>%
  st_transform(crs = crs_pacific)

data_graticules <- st_read("data/01_maps/01_raw/03_natural-earth/ne_10m_graticules_20/ne_10m_graticules_20.shp") %>% 
  st_transform(4326) %>%
  st_break_antimeridian(lon_0 = 160) %>%
  st_transform(crs = crs_pacific)

data_regions <- st_read("data/01_maps/02_clean/03_regions/gcrmn_regions.shp") %>% 
  st_transform(4326) %>%
  st_break_antimeridian(lon_0 = 160) %>%
  st_transform(crs = crs_pacific)

# Merge the Pacific polygons
data_pacific <- data_regions %>%
  filter(region == "Pacific") %>%
  st_buffer(1000) %>%
  summarise(region = "Pacific",
            geometry = st_union(geometry)) %>%
  st_buffer(-1000)

# Recombine with all other regions
data_regions <- data_regions %>%
  filter(region != "Pacific") %>%
  bind_rows(data_pacific)

## 8.2 Create the border of background map ----

lon_min <- -20 + 0.001
lon_max <- 340 - 0.001

lon_seq <- seq(lon_min, lon_max, length.out = 1000)

border_coords <- rbind(
  
  # Bottom edge
  cbind(lon_seq, -89.999),
  
  # Right edge
  cbind(lon_max, seq(-89.999, 89.999, length.out = 500)),
  
  # Top edge
  cbind(rev(lon_seq), 89.999),
  
  # Left edge
  cbind(lon_min, seq(89.999, -89.999, length.out = 500))
)

border_coords <- rbind(
  border_coords,
  border_coords[1, ]
)

background_map_border <- border_coords %>%
  st_linestring() %>%
  st_sfc(crs = 4326) %>%
  st_transform(crs_pacific)

## 8.3 Create the plot ----

ggplot() +
  geom_sf(data = data_graticules, color = "#ecf0f1", linewidth = 0.25) +
  geom_sf(data = background_map_border, fill = NA, color = "grey30", linewidth = 0.25) +
  geom_sf(data = data_regions, aes(fill = region), color = "white", show.legend = FALSE, linewidth = 0.3) +
  scale_fill_manual(values = c("Australia" = "#0C2F3B",
                               "Brazil" = "#40A6AA",
                               "Caribbean" = "#31BDEA",
                               "EAS" = "#40A6AA",
                               "ETP" = "#0C2F3B",
                               "Pacific" = "#1A497C",
                               "ROPME" = "#0C2F3B",
                               "PERSGA" = "#40A6AA",
                               "South Asia" = "#31BDEA",
                               "WIO" = "#1A497C")) +
  geom_sf(data = data_land, color = "#24252a", fill = "#dadfe1") +
  theme(text = element_text(family = font_choose_graph),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        legend.position = "bottom",
        legend.background = element_rect(fill = "transparent", color = NA),
        legend.title = element_blank(),
        panel.background = element_blank(),
        plot.background = element_rect(fill = "transparent", color = NA)) +
  guides(fill = guide_legend(override.aes = list(size = 5, color = NA)))

ggsave("figs/00_misc/fig_map-regions_raw.png", bg = "transparent", height = 4, width = 8, dpi = 300)

ggsave("figs/00_misc/fig_map-regions_raw.pdf", bg = "transparent", height = 4, width = 8)

# 9. Map of coral reefs distribution ----

data_reefs <- st_read("data/01_maps/02_clean/02_reefs-buffer/reefs_buffer_100.shp") %>%
  st_transform(4326) %>%
  st_wrap_dateline(
    options = c("WRAPDATELINE=YES", "DATELINEOFFSET=180"),
    quiet = TRUE
  ) %>%
  st_make_valid() %>%
  st_break_antimeridian(lon_0 = 160) %>%
  st_transform(crs = crs_pacific)

ggplot() +
  geom_sf(data = data_graticules, color = "#ecf0f1", linewidth = 0.25) +
  geom_sf(data = background_map_border, fill = NA, color = "grey30", linewidth = 0.25) +
  geom_sf(data = data_reefs, color = "#40A6AA", fill = "#40A6AA", show.legend = FALSE) +
  geom_sf(data = data_land, color = "#24252a", fill = "#dadfe1") +
  theme(text = element_text(family = font_choose_graph),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        legend.position = "bottom",
        legend.background = element_rect(fill = "transparent", color = NA),
        legend.title = element_blank(),
        panel.background = element_blank(),
        plot.background = element_rect(fill = "transparent", color = NA)) +
  guides(fill = guide_legend(override.aes = list(size = 5, color = NA)))

ggsave("figs/00_misc/fig_map-reef-distribution.png", bg = "transparent", height = 4, width = 8, dpi = 300)
