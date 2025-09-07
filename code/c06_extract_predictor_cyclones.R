# 1. Load packages ----

library(tidyverse)
library(sf)
sf_use_s2(FALSE)
library(lwgeom)
library(RcppRoll)

# 2. Load data ----

## 2.1 Site coordinates ----

data_sites <- st_read("data/03_site-coords/site-coords_all.shp") %>% 
  st_transform(crs = 4326)

## 2.2 Cyclones lines ----

load("data/07_cyclones/01_cyclones_lines.RData")

data_ts_lines <- data_ts_lines %>% 
  st_transform(crs = 4326)

## 2.3 Cyclones points ----

load("data/07_cyclones/01_cyclones_points.RData")

data_ts_points <- data_ts_points %>% 
  st_transform(crs = 4326)

## 2.4 Coral reef distribution 100 km buffer ----

data_reef_buffer <- st_read("data/01_maps/02_clean/02_reefs-buffer/reefs_buffer_100.shp") %>% 
  st_transform(crs = 4326) %>% 
  st_wrap_dateline() %>% 
  st_make_valid()

# 3. Extract cyclones passing within 100 km from coral reefs ----

data_ts_lines_reef <- st_intersection(data_reef_buffer, data_ts_lines)

data_ts_points <- data_ts_points %>% 
  filter(ts_id %in% unique(data_ts_lines_reef$ts_id)) %>% 
  filter(windspeed >= 119)

# 4. Create buffer of 111 km around sites with coral reefs ----

data_sites_buffer <- data_sites %>%  
  # Create the buffer around the site (1 degree ~ 111 km)
  st_buffer(dist = 1) %>% 
  st_wrap_dateline() %>% 
  st_make_valid()

# 5. Extract cyclones passing within 111 km from each site ----

pred_cyclones <- st_intersection(data_sites_buffer, data_ts_points) %>% 
  mutate(year = year(time)) %>% 
  st_drop_geometry() %>% 
  select(site_id, type, year, ts_id, ts_name, windspeed) %>% 
  distinct() %>% 
  group_by(site_id, type, year, ts_id, ts_name) %>% 
  filter(windspeed == max(windspeed)) %>% 
  ungroup()

# 6. Transform data to create predictors ----

pred_cyclones <- pred_cyclones %>% 
  group_by(site_id, type, year) %>% 
  mutate(windspeed = max(windspeed),
         nb_cyclones = n()) %>% 
  ungroup() %>% 
  select(-ts_id, -ts_name) %>%
  distinct() %>% 
  tidyr::complete(year = seq(1980, 2024), nesting(site_id, type), 
                  fill = list(ts_id = NA, ts_name = NA, windspeed = NA)) %>% 
  # Wind speed of cyclones over year n-1 (last year)
  arrange(site_id, type, year) %>% 
  mutate(windspeed_y1 = roll_max(windspeed, n = 1, align = "right", fill = NA, na.rm = TRUE),
         windspeed_y1 = if_else(windspeed_y1 == -Inf, 0, windspeed_y1)) %>% 
  # Number of cyclones over year n-5 (last year)
  mutate(nb_cyclones_y1 = roll_sum(nb_cyclones, n = 1, align = "right", fill = NA, na.rm = TRUE)) %>% 
  group_by(site_id, type) %>% 
  mutate(cyclones_freq = sum(nb_cyclones, na.rm = TRUE)/(2024-1980)) %>% 
  ungroup() %>% 
  select(-windspeed, -nb_cyclones)

# 7. Export the results ----

write.csv(pred_cyclones, file = "data/08_predictors/pred_cyclones.csv", row.names = FALSE)
