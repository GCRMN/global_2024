# 1. Load packages ----

library(tidyverse)
library(sf)
sf_use_s2(FALSE)

# 2. Load and combine files ----

## 2.1 List of files to combine ----

files_list <- list.files(path = "data/01_maps/01_raw/04_wdpa/",
                         full.names = TRUE, recursive = TRUE) %>% 
  as_tibble() %>% 
  filter(str_detect(value, "\\.shp") == TRUE & str_detect(value, "polygons") == TRUE) %>% 
  pull(value)

## 2.2 Combine the files ----

data_wdpa <- map(files_list, ~st_read(.)) %>% 
  list_rbind() %>% 
  st_as_sf()
  
# 3. Filter MPA data ----

data_reefs <- st_read("data/01_maps/02_clean/01_reefs/reefs.shp")

data_wdpa <- data_wdpa %>% 
  # Filter protected areas in marine realms
  filter(MARINE != "0") %>% 
  # Filter protected areas on coral reefs
  st_filter(., data_reefs, .predicate = st_intersects)

# 4. Export the file ----

st_write(data_wdpa, "data/01_maps/02_clean/06_wdpa/wdpa_reefs.shp")
