# 1. Required packages ----

library(tidyverse) # Core tidyverse packages
library(terra)
library(sf)
sf_use_s2(FALSE)

# 2. Load data ----

## 2.1 Site coordinates ----

site_coords <- st_read("data/03_site-coords/site-coords_all.shp")

## 2.2 Gravity ----

data_gravity <- st_read("data/02_misc/gravity/Total Gravity of Coral Reefs 1.0.shp") %>% 
  rename(pred_gravity = Grav_tot) %>% 
  select(-reef_ID)

# 3. Visual check ----

ggplot() +
  geom_sf(data = data_gravity) +
  geom_sf(data = site_coords)

# 4. Extract gravity for each site ----

st_intersection(site_coords, data_gravity) %>% 
  st_drop_geometry() %>% 
  left_join(site_coords %>% st_drop_geometry(), .) %>% 
  write.csv(., file = "data/08_predictors/pred_gravity.csv", row.names = FALSE)
