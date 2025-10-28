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

crs_selected <- "+proj=eqc +lat_ts=0 +lat_0=0 +lon_0=160 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs"

data_tif <- project(data_tif, crs_selected)

data_graticules <- st_difference(data_graticules %>% st_transform(crs = 4326), 
                                 st_union(data_countries %>% st_transform(crs = 4326))) %>% 
  st_make_valid() %>% 
  st_transform(., crs_selected) 

data_countries <- read_sf("data/01_maps/01_raw/03_natural-earth/ne_10m_admin_0_countries/ne_10m_admin_0_countries.shp") %>% 
  st_transform(crs = crs_selected)

data_region <- data_region %>% 
  st_transform(crs = crs_selected)

## 5.2 Make the map ----

plot_i <- ggplot() +
  geom_spatraster_rgb(data = data_tif, maxcell = 5e+06) +
  #geom_sf(data = data_region, aes(color = region), fill = "white", linewidth = 0.15,
  geom_sf(data = data_region, color = "white", fill = "white", linewidth = 0.15,
          alpha = 0.1, show.legend = FALSE) +
  geom_sf(data = data_countries, fill = NA, color = "black", linewidth = 0.05) +
  geom_sf(data = data_graticules, color = "white", linewidth = 0.02) +
  coord_sf(ylim = c(-5000000, 5000000), expand = FALSE,
           label_axes = list(top = "E", left = "N", right = "N")) +
  theme(panel.border = element_rect(color = "black", fill = NA, linewidth = 0.15),
        plot.background = element_blank(),
        axis.ticks = element_line(color = "black", linewidth = 0.15),
        axis.text.x = element_text(size = 5, color = "black"),
        axis.text.y = element_text(angle = 90, size = 5, hjust = 0.5, color = "black"),
        axis.text.y.right = element_text(angle = 90, size = 4, hjust = 0.5, color = "black"))
  
ggsave("figs/01_part-1/fig-1-raw.png", height = 2, width = 6.5)

plot_i <- ggplot() +
  #geom_spatraster_rgb(data = data_tif, maxcell = 5e+06) +
  #geom_sf(data = data_region, aes(color = region), fill = "white", linewidth = 0.15,
  geom_sf(data = data_region, color = "white", fill = "white", linewidth = 0.15,
          alpha = 0.1, show.legend = FALSE) +
  geom_sf(data = data_countries, fill = NA, color = "black", linewidth = 0.05) +
  geom_sf(data = data_graticules, color = "white", linewidth = 0.02) +
  #coord_sf(ylim = c(-5000000, 5000000), expand = FALSE,
  #         label_axes =  "NWE") +
  coord_sf(expand = FALSE,
           label_axes =  "NWE") +
  scale_x_continuous(expand = c(2000, 2000)) +
  scale_y_continuous(expand = c(2000, 2000)) +
  theme(panel.border = element_rect(color = "black", fill = NA, linewidth = 0.15),
        panel.grid = element_blank(),
        plot.background = element_blank(),
        axis.ticks = element_line(color = "black", linewidth = 0.15),
        axis.text.x = element_text(size = 5, color = "black"),
        axis.text.y = element_text(angle = 90, size = 5, hjust = 0.5, color = "black"),
        axis.text.y.right = element_text(angle = 90, size = 4, hjust = 0.5, color = "black"))

ggsave("figs/01_part-1/fig-2-raw.png", height = 2, width = 6.5)

## 5.3 Map guide for subregion labels ----

ggplot() +
  geom_sf(data = data_subregions %>% st_transform(crs = crs_selected), aes(fill = region),
          show.legend = FALSE, alpha = 0.5) +
  ggsflabel::geom_sf_label(data = data_subregions %>% st_transform(crs = crs_selected),
                           aes(label = str_sub(subregion, -1, -1), fill = region),
                           show.legend = FALSE, alpha = 0.75) +
  labs(x = NULL, y = NULL)

ggsave("figs/02_part-2/fig-1/00_guide-subregions-labels.png", width = 12, height = 3.5)
