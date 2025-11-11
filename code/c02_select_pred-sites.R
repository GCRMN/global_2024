# 1. Load packages ----

library(tidyverse) # Core tidyverse packages
library(sf)
sf_use_s2(FALSE)

# 2. Load data ----

data_reefs <- read_sf("data/01_maps/02_clean/01_reefs/global_2024_reefs.shp")

data_eez <- read_sf("data/01_maps/01_raw/05_eez/eez_v12.shp") %>% 
  select(TERRITORY1) %>% 
  rename(territory = TERRITORY1)

# 3. Intersects reefs and EEZ ----

data_reefs <- st_intersection(data_reefs, data_eez)

# 4. Calculate reef extent ----

data_reef_extent <- data_reefs %>% 
  group_by(region, subregion, territory) %>% 
  summarise(reef_extent_abs = sum(st_area(geometry))) %>% 
  ungroup()

# 5. Calculate number of sites to predict ----

total_sites <- 20000 # 20,000 sites

data_reef_extent <- data_reef_extent %>% 
  mutate(reef_extent_abs = as.numeric(reef_extent_abs),
         reef_extent_rel = reef_extent_abs/sum(reef_extent_abs)) %>% 
  mutate(nb_sites = round(total_sites*reef_extent_rel, 0),
         # Replace 0 to 1 site
         nb_sites = case_when(nb_sites == 0 ~ 1,
                              TRUE ~ nb_sites),
         # Reduce number of sites in greatest region/subregion/territory
         # to sum up to total_sites (necessary due to rounding and previous step)
         nb_sites = case_when(nb_sites == max(nb_sites) ~ nb_sites-(sum(nb_sites)-total_sites),
                              TRUE ~ nb_sites)) %>% 
  select(-reef_extent_abs, -reef_extent_rel)

# 6. Sample sites for predictions ----

## 6.1 Create the function ----

sample_sites <- function(row_i){
  
  data_i <- data_reef_extent %>% 
    filter(row_number() == row_i) %>% 
    st_transform(crs = "EPSG:3857")
  
  result <- st_sample(data_i, size = as.numeric(data_i$nb_sites), oriented = TRUE) %>% 
    st_as_sf() %>% 
    mutate(region = as.character(data_i$region),
           subregion = as.character(data_i$subregion),
           territory = as.character(data_i$territory)) %>% 
    st_transform(crs = "EPSG:4326")
  
  return(result)
  
}
  
## 6.2 Map over the function ----

data_sites_pred <- map(1:nrow(data_reef_extent), ~sample_sites(row_i = .x)) %>% 
  list_rbind() %>% 
  st_as_sf()

# 7. Checks ----

ggplot() +
  geom_sf(data = data_sites_pred)

data_sites_pred %>% 
  st_drop_geometry() %>% 
  group_by(region) %>% 
  count() %>% 
  ungroup() %>% 
  mutate(rel = (n/sum(n))*100)

data_sites_pred %>% 
  st_drop_geometry() %>% 
  group_by(subregion) %>% 
  count() %>% 
  ungroup() %>% 
  mutate(rel = (n/sum(n))*100)

data_sites_pred %>% 
  st_drop_geometry() %>% 
  group_by(territory) %>% 
  count() %>% 
  ungroup() %>% 
  mutate(rel = (n/sum(n))*100)

# 8. Export the shapefile ----

data_sites_pred %>% 
  mutate(type = "pred",
         site_id = as.character(row_number(.))) %>% 
  select(-region, -subregion, -territory) %>% 
  st_write(., dsn = "data/03_site-coords/global_2024_site-coords_pred.shp",
           delete_dsn = TRUE, delete_layer = TRUE, append = FALSE)
