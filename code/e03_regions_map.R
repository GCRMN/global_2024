# 1. Load packages ----

library(tidyverse)
library(magrittr) # For special pipe %<>%
library(s2)
library(sf)
sf_use_s2(TRUE)
library(ggspatial) # For annotation_scale function

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
