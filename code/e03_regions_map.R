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
source("code/function/map_region_geography.R")
source("code/function/map_region_monitoring.R")

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

## 4.1 Load and transform data ----

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

map(unique(data_subregions$region), ~map_region_geography(region_i = .))

# 5. Monitoring distribution maps ----

## 5.1 Load and transform data ----

load("data/02_misc/data-benthic.RData")

data_benthic_sites <- data_benthic %>% 
  select(decimalLatitude, decimalLongitude, region, subregion, year) %>% 
  distinct() %>% 
  group_by(decimalLatitude, decimalLongitude, region, subregion) %>% 
  count(name = "nb_years") %>% 
  ungroup() %>% 
  mutate(int_class = cut(nb_years, 
                         breaks = c(-Inf, 1, 5, 10, 15, Inf),
                         labels = c("1 year", "2-5 years", "6-10 years", "11-15 years", ">15 years")),
         int_class = as.factor(int_class)) %>% 
  arrange(int_class) %>% 
  select(-nb_years) %>% 
  st_as_sf(coords = c("decimalLongitude", "decimalLatitude"), crs = 4326)

## 5.2 Make the maps ----

map(unique(data_subregions$region), ~map_region_monitoring(region_i = .))
