# 1. Load packages ----

library(tidyverse)
library(sf)
sf_use_s2(FALSE)

# 2. Source functions ----

source("code/function/graphical_par.R")

# 3. Load GCRMN subregions ----

data_subregion <- st_read("data/01_maps/02_clean/04_subregions/gcrmn_subregions.shp") %>% 
  st_transform(crs = 4326)

# 4. Load coral reef distribution ----

# reefs_corrected.shp is from gcrmndb_benthos (https://github.com/GCRMN/gcrmndb_benthos)
# See scripts 01_clean-reefs and 02_clean-buffer-reefs in this repository

data_reefs <- st_read("data/01_maps/01_raw/02_reefs-corrected/reefs_corrected.shp") %>% 
  st_transform(crs = 4326) %>% 
  st_wrap_dateline(options = "WRAPDATELINE=YES") %>% 
  st_make_valid()

ggplot() +
  geom_sf(data = data_reefs) # Visual check

# 5. Make the intersection ---- 

data_reefs <- st_intersection(data_reefs, data_subregion)

ggplot() +
  geom_sf(data = data_reefs, aes(color = subregion)) + # Visual check
  theme(legend.position = "bottom")

# 6. Export ----

st_write(data_reefs, "data/01_maps/02_clean/01_reefs/global_2024_reefs.shp", append = FALSE)
