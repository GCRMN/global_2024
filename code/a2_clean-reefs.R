# 1. Load packages ----

library(tidyverse)
library(sf)
sf_use_s2(FALSE)

# 2. Source functions ----

source("code/function/graphical_par.R")

# 3. Load ACA coral reefs distribution data ----

data_reefs <- st_read("data/01_raw-shp/01_coral-reefs/reef-aca.shp") %>% 
  st_union()

data_reefs_buffer <- data_reefs %>% 
  st_transform(crs = crs_meters) %>% 
  st_buffer(dist = 100000) %>% 
  st_union() %>% 
  st_transform(crs = 4326)

ggplot() +
  geom_sf(data = data_reefs, aes(fill = "red", color = "red"), show.legend = FALSE) +
  geom_sf(data = data_reefs_buffer, aes(fill = "lightgrey"), alpha = 0.2, show.legend = FALSE)
  
