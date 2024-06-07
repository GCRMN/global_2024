# 1. Load packages ----

library(tidyverse)
library(sf)
sf_use_s2(FALSE)

# 2. Source functions ----

source("code/function/graphical_par.R")
source("code/function/theme_map.R")

# 3. Load data ----

crs_180 <- "+proj=eqc +lon_0=155 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"

data_admin <- st_read("data/01_raw-shp/03_land/ne_10m_admin_0_countries/ne_10m_admin_0_countries.shp") %>%
  st_break_antimeridian(lon_0 = 155) %>% # insert this before transformation
  st_transform(crs = crs_180)

data_region <- st_read("data/01_raw-shp/04_gcrmn-regions/gcrmn_regions.shp") %>%
  st_break_antimeridian(lon_0 = 155) %>% # insert this before transformation
  st_transform(crs = crs_180)

ggplot() +
  geom_sf(data = data_region, aes(fill = region)) +
  geom_sf(data = data_admin)
