# 1. Load packages ----

library(tidyverse)
library(magrittr) # For special pipe %<>%
library(s2)
library(sf)
sf_use_s2(TRUE)
library(ggspatial) # For annotation_scale function
library(terra)
library(tidyterra)
library(ggtext)
library(cowplot)

# 2. Load functions ----

source("code/function/graphical_par.R")
source("code/function/map_sphere.R")
source("code/function/data_descriptors.R")
source("code/function/map_region_geography.R")

# 3. Sphere maps ----

## 3.1 Load data --

data_land <- read_sf("data/01_maps/01_raw/03_natural-earth/ne_10m_land/ne_10m_land.shp") %>% 
  st_transform(crs = 4326)

data_region <- read_sf("data/01_maps/02_clean/03_regions/gcrmn_regions.shp") %>% 
  st_transform(crs = 4326) %>% 
  st_difference(., st_union(data_land))

data_graticules <- read_sf("data/01_maps/01_raw/03_natural-earth/ne_10m_graticules_15/ne_10m_graticules_15.shp") %>% 
  st_transform(crs = 4326)

## 3.2 Define the parameters ----

data_parameters <- tibble(region = c("Australia", "Brazil", "Caribbean", "EAS", "ETP",
                                     "PERSGA", "Pacific", "ROPME", "South Asia", "WIO"),
                          longitude = c("130", "-60", "-70", "115", "-90",
                                        "40", "-170", "50", "75", "35"))

## 3.3 Map over the function ----

map(unique(data_region$region), ~map_sphere(region_i = .))

# 4. Geographic maps (regions) ----

## 4.1 Load and transform data ----

data_countries <- read_sf("data/01_maps/01_raw/03_natural-earth/ne_10m_admin_0_countries/ne_10m_admin_0_countries.shp")

data_subregions <- read_sf("data/01_maps/02_clean/04_subregions/gcrmn_subregions.shp")

data_tif <- rast("data/01_maps/01_raw/03_natural-earth/HYP_HR_SR_OB_DR/HYP_HR_SR_OB_DR.tif")

data_reefs <- read_sf("data/01_maps/02_clean/02_reefs-buffer/reefs_buffer_20.shp")

data_tropics <- tibble(tropic = c("Cancer", "Cancer", "Equator", "Equator", "Capricorn", "Capricorn"),
                       lat = c(23.4366, 23.4366, 0, 0, -23.4366, -23.4366),
                       long = c(-180, 180, -180, 180, -180, 180)) %>% 
  st_as_sf(coords = c("long", "lat"), crs = 4326) %>%
  group_by(tropic) %>%
  dplyr::summarize(do_union = FALSE) %>%
  st_cast("LINESTRING")

sf_use_s2(FALSE)

data_subregions <- st_difference(data_subregions, st_union(data_countries))

data_tropics <- st_difference(data_tropics, st_union(data_countries))

## 4.2 Make the maps ----

map(unique(data_subregions$region), ~map_region_geography(region_i = .))

# 5. Geographic map (global) ----

## 5.1 Load and transform data ----

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
  left_join(., palette_regions)

data_tropics <- tibble(tropic = c("Cancer", "Cancer", "Equator", "Equator", "Capricorn", "Capricorn"),
                       lat = c(23.4366, 23.4366, 0, 0, -23.4366, -23.4366),
                       long = c(-180, 180, -180, 180, -180, 180)) %>% 
  st_as_sf(coords = c("long", "lat"), crs = 4326) %>%
  group_by(tropic) %>%
  dplyr::summarize(do_union = FALSE) %>%
  st_cast("LINESTRING") %>% 
  st_difference(polygon) %>%
  st_transform(crs = crs_selected)

## 5.2 Make the map ----

plot_i <- ggplot() +
  geom_sf(data = data_tropics, linetype = "dashed", col = "white", linewidth = 0.3) +
  geom_sf(data = data_region, aes(fill = color), color = "white",
          show.legend = FALSE, alpha = 0.75, linewidth = 0.5) +
  scale_fill_identity() +
  geom_sf(data = data_countries) +
  coord_sf(ylim = c(-5000000, 5000000), expand = FALSE,
           label_axes = list(top = "E", left = "N", right = "N", bottom = "E")) +
  theme(panel.border = element_rect(fill = NA, color = "black"),
        panel.background = element_rect(fill = "white"),
        panel.grid = element_blank(),
        plot.background = element_rect(fill = "transparent", color = NA),
        axis.ticks.length = unit(-0.1, "cm"),
        axis.text = element_text(family = font_choose_map, color = "black", size = 12, margin = margin(t = -6)),
        axis.text.x.top = element_text(hjust = 0.5, vjust = -7),
        axis.text.x.bottom = element_text(hjust = 0.5, vjust = 8),
        axis.text.y = element_text(angle = 90, hjust = 0.5, vjust = -7),
        axis.text.y.right = element_text(angle = -90, hjust = 0.5, vjust = -7))

ggsave("figs/02_part-1/fig-regions.png", dpi = 600, height = 3.4, width = 12)

## 5.3 Donuts charts ----

data_reef_extent <- readxl::read_xlsx("figs/06_supp-mat/reef-extent.xlsx") %>% 
  filter(subregion == "All" & region != "All") %>% 
  select(region, reef_extent_abs, reef_extent_rel_world) %>% 
  mutate(reef_extent_rel_world = as.numeric(reef_extent_rel_world),
         label = paste0("<span style='font-size:80pt;'><b>", round(reef_extent_rel_world, 1), "</b>%</span>", "<br>",
                        "<span style='font-size:65pt;'>",
                        reef_extent_abs, " km²</span>"))

plot_donutchart <- function(region_i){
  
  data_reef_extent_i <- data_reef_extent %>% 
    left_join(., palette_regions) %>% 
    mutate(color = case_when(region == region_i ~ color,
                             TRUE ~ "#b2bec3"))

  data_order <- data_reef_extent %>%
    arrange(desc(reef_extent_rel_world)) %>%
    pull(region)
  
  value_region <- data_reef_extent %>% 
    filter(region == region_i) %>% 
    select(label) %>% 
    pull()
  
  data_reef_extent_i <- data_reef_extent_i %>% 
    mutate(reef_extent_rel_world = case_when(reef_extent_rel_world < 1 ~ 1.5,
                                             TRUE ~ reef_extent_rel_world),
           region = factor(region, levels = data_order))
  
  ggplot() +
    geom_bar(data = data_reef_extent_i, aes(x = 2, y = reef_extent_rel_world, fill = color, group = 1),
             stat = "identity", width = 1, show.legend = FALSE, color = "white", linewidth = 3) +
    coord_polar("y", start = 0, clip = "off") + 
    xlim(0.5, 2.5) + 
    theme_void() +
    scale_fill_identity() +
    labs(x = NULL, y = NULL) +
    theme(axis.text = element_blank(),
          panel.grid = element_blank(),
          plot.background  = element_rect(fill = "transparent", color = NA),
          panel.background = element_rect(fill = "transparent", color = NA)) +
    annotate("rect", xmin = 0, xmax = 1.5, ymin = -Inf, ymax = Inf, fill = "white") +
    geom_richtext(aes(x = 0.5, y = 0.5), label = value_region, size = 20, family = font_choose_map, label.color = NA)
  
  ggsave(filename = paste0("figs/02_part-1/fig-2_",
                           str_replace_all(str_replace_all(str_to_lower(region_i), " ", "-"), "---", "-"),
                           ".png"),
         width = 7, height = 7,
         bg = "transparent", dpi = 600)
  
}

map(unique(data_region$region), ~plot_donutchart(region_i = .x))

# 6. Distribution of coral reefs ----

data_reefs <- read_sf("data/01_maps/02_clean/02_reefs-buffer/reefs_buffer_100.shp")

plot_i <- ggplot() +
  geom_sf(data = data_tropics, linetype = "dashed", col = "black", linewidth = 0.3) +
  geom_sf(data = data_reefs, fill = "#ad5fad", color = "#ad5fad") +
  geom_sf(data = data_countries) +
  coord_sf(ylim = c(-5000000, 5000000), expand = FALSE,
           label_axes = list(top = "E", left = "N", right = "N", bottom = "E")) +
  theme(panel.border = element_rect(fill = NA, color = "black"),
        panel.background = element_rect(fill = "white"),
        panel.grid = element_blank(),
        plot.background = element_rect(fill = "transparent", color = NA),
        axis.ticks.length = unit(-0.1, "cm"),
        axis.text = element_text(family = font_choose_map, color = "black", size = 12, margin = margin(t = -6)),
        axis.text.x.top = element_text(hjust = 0.5, vjust = -7),
        axis.text.x.bottom = element_text(hjust = 0.5, vjust = 8),
        axis.text.y = element_text(angle = 90, hjust = 0.5, vjust = -7),
        axis.text.y.right = element_text(angle = -90, hjust = 0.5, vjust = -7))

ggsave("figs/02_part-1/fig-1_raw.png", dpi = 600, height = 3.4, width = 12)
