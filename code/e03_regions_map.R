# 1. Load packages ----

library(tidyverse)
library(magrittr) # For special pipe %<>%
library(s2)
library(sf)
sf_use_s2(TRUE)
library(ggspatial) # For annotation_scale function
library(terra)
library(tidyterra)

# 2. Load functions ----

source("code/function/graphical_par.R")
source("code/function/map_sphere.R")

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

# 4. Geographic maps ---

## 4.1 Load data ----

data_countries <- read_sf("data/01_maps/01_raw/03_natural-earth/ne_10m_admin_0_countries/ne_10m_admin_0_countries.shp")

data_subregions <- read_sf("data/01_maps/02_clean/04_subregions/gcrmn_subregions.shp")

data_tif <- rast("data/01_maps/01_raw/03_natural-earth/HYP_HR_SR_OB_DR/HYP_HR_SR_OB_DR.tif")

data_reefs <- read_sf("data/01_maps/02_clean/01_reefs/reefs.shp")

data_tropics <- tibble(tropic = c("Cancer", "Cancer", "Equator", "Equator", "Capricorn", "Capricorn"),
                       lat = c(23.4366, 23.4366, 0, 0, -23.4366, -23.4366),
                       long = c(-180, 180, -180, 180, -180, 180)) %>% 
  st_as_sf(coords = c("long", "lat"), crs = 4326) %>%
  group_by(tropic) %>%
  dplyr::summarize(do_union = FALSE) %>%
  st_cast("LINESTRING")

## 4.2 Make the maps ----

map_region_geography <- function(region_i, color_scalebar = "white"){
  
  data_subregions_i <- data_subregions %>% 
    filter(region == region_i)
  
  plot_i <- ggplot() +
    geom_spatraster_rgb(data = data_tif, maxcell = 10e6) +
    geom_sf(data = data_reefs, fill = "#c44d56", color = "#c44d56") +
    geom_sf(data = data_tropics, linetype = "dashed", linewidth = 0.25) +
    geom_sf(data = data_subregions_i, color = "white", fill = NA, linewidth = 0.3) +
    geom_sf(data = data_countries, fill = "#dadfe1", color = "black") +
    theme(panel.border = element_rect(fill = NA, color = "black"),
          axis.text = element_text(family = font_choose_map, color = "black"))
  
  if(region_i == "South Asia"){
    
    plot_i <- plot_i + 
      coord_sf(xlim = c(60, 103), ylim = c(-12, 30),
               label_axes = list(bottom = "E", top = "E", left = "N")) +
      annotation_scale(location = "br",
                       width_hint = 0.25, text_family = font_choose_map, text_col = color_scalebar,
                       text_cex = 0.7, style = "bar", line_width = 1, height = unit(0.04, "cm"),
                       line_col = color_scalebar, pad_x = unit(0.5, "cm"), pad_y = unit(0.5, "cm"),
                       bar_cols = c(color_scalebar, color_scalebar))
    
    ggsave(paste0("figs/02_part-2/fig-2/", str_replace_all(str_to_lower(region_i), " ", "-"), "_raw.png"),
           height = 5.6, width = 5.5)
    
  }else if(region_i == "EAS"){
    
    plot_i <- plot_i + 
      coord_sf(xlim = c(88, 145), ylim = c(-13, 35),
               label_axes = list(bottom = "E", top = "E", left = "N")) +
      annotation_scale(location = "bl",
                       width_hint = 0.25, text_family = font_choose_map, text_col = color_scalebar,
                       text_cex = 0.7, style = "bar", line_width = 1, height = unit(0.04, "cm"),
                       line_col = color_scalebar, pad_x = unit(0.5, "cm"), pad_y = unit(0.5, "cm"),
                       bar_cols = c(color_scalebar, color_scalebar))
    
    ggsave(paste0("figs/02_part-2/fig-2/", str_replace_all(str_to_lower(region_i), " ", "-"), "_raw.png"),
           height = 5.5, width = 6)
    
  }else if(region_i == "Caribbean"){
    
    plot_i <- plot_i + 
      coord_sf(xlim = c(-100, -55), ylim = c(7.5, 35),
               label_axes = list(top = "E", left = "N", right = "N"))
    
    ggsave(paste0("figs/02_part-2/fig-2/", str_replace_all(str_to_lower(region_i), " ", "-"), "_raw.png"),
           height = 5.3, width = 8)
    
  }else if(region_i == "WIO"){
    
    plot_i <- plot_i + 
      coord_sf(xlim = c(30, 68), ylim = c(11, -32),
               label_axes = list(bottom = "E", top = "E", left = "N")) +
      annotation_scale(location = "br",
                       width_hint = 0.25, text_family = font_choose_map, text_col = color_scalebar,
                       text_cex = 0.7, style = "bar", line_width = 1, height = unit(0.04, "cm"),
                       line_col = color_scalebar, pad_x = unit(0.5, "cm"), pad_y = unit(0.5, "cm"),
                       bar_cols = c(color_scalebar, color_scalebar))
    
    ggsave(paste0("figs/02_part-2/fig-2/", str_replace_all(str_to_lower(region_i), " ", "-"), "_raw.png"),
           height = 6.6, width = 5.8)
    
  }else if(region_i == "ROPME"){
    
    plot_i <- plot_i + 
      coord_sf(xlim = c(45, 67), ylim = c(13, 32),
               label_axes = list(bottom = "E", top = "E", left = "N")) +
      annotation_scale(location = "br",
                       width_hint = 0.25, text_family = font_choose_map, text_col = color_scalebar,
                       text_cex = 0.7, style = "bar", line_width = 1, height = unit(0.04, "cm"),
                       line_col = color_scalebar, pad_x = unit(0.5, "cm"), pad_y = unit(0.5, "cm"),
                       bar_cols = c(color_scalebar, color_scalebar))
    
    ggsave(paste0("figs/02_part-2/fig-2/", str_replace_all(str_to_lower(region_i), " ", "-"), "_raw.png"),
           height = 5.9, width = 5.9)
    
  }
  
}

map_region_geography(region_i = "South Asia")

map(c("South Asia", "ROPME", "Caribbean", "EAS", "WIO"), ~map_region_geography(region_i = .))


# Reef buffer
# intersect reefs et region pour filter que reef region après
# Créer les fichiers inkscape et ajouter les labels (pays limitrophes, océans, tropiques, ecoregions)
# Créer une fonction map_region_monitoring
