# 1. Load packages ----

library(tidyverse)
library(sf)
sf_use_s2(FALSE)
library(units)

# 2. Load data ----

data_reefs <- st_read("data/01_maps/02_clean/01_reefs/global_2024_reefs.shp")

data_regions <- st_read("data/01_maps/02_clean/03_regions/gcrmn_regions.shp")

data_eez <- st_read("data/01_maps/01_raw/05_eez/eez_v12.shp")

# 3. Number of countries and territories ----

data_countries <- st_intersection(data_reefs, data_eez) %>% 
  st_drop_geometry() %>% 
  select(region, SOVEREIGN1, TERRITORY1) %>% 
  distinct() %>% 
  arrange(region) %>% 
  group_by(region) %>% 
  mutate(nb_countries = n_distinct(TERRITORY1)) %>% 
  ungroup() %>% 
  # Correction on the number of countries
  mutate(nb_countries = case_when(region == "PERSGA" ~ 9,
                                  region == "Caribbean" ~ 44,
                                  region == "ROPME" ~ 8,
                                  region == "Brazil" ~ 2,
                                  region == "Australia" ~ 3,
                                  TRUE ~ nb_countries)) %>% 
  # Add denomination
  mutate(denomination = case_when(region %in% c("Caribbean", "Pacific", "ETP", "EAS",
                                                "WIO", "South Asia") ~ "countries and territories",
                                  TRUE ~ "countries"))

# 4. Countries with coral reefs on the north, south, east, and west ----

data_orientation <- st_intersection(data_reefs %>% select(-subregion), data_eez %>%
                                    select(SOVEREIGN1, TERRITORY1)) %>% 
  mutate(bbox = map(geometry, ~st_bbox(.x))) %>% 
  mutate(xmin = map_dbl(bbox, "xmin"),
         ymin = map_dbl(bbox, "ymin"),
         xmax = map_dbl(bbox, "xmax"),
         ymax = map_dbl(bbox, "ymax")) %>%
  select(-bbox) %>% 
  st_drop_geometry() %>% 
  pivot_longer(c("xmin", "xmax"), values_to = "x", names_to = "name1") %>% 
  pivot_longer(c("ymin", "ymax"), values_to = "y", names_to = "name2") %>% 
  select(-name1, -name2) %>% 
  group_by(region) %>% 
  mutate(border_east = case_when(x == min(x) ~ "east", TRUE ~ NA),
         border_west = case_when(x == max(x) ~ "west", TRUE ~ NA),
         border_north = case_when(y == max(y) ~ "north", TRUE ~ NA),
         border_south = case_when(y == min(y) ~ "south", TRUE ~ NA)) %>% 
  select(region, TERRITORY1, border_east, border_west, border_north, border_south) %>% 
  pivot_longer("border_east":"border_south", names_to = "value", values_to = "border") %>% 
  select(-value) %>% 
  drop_na(border) %>% 
  distinct() %>% 
  pivot_wider(names_from = "border", values_from = "TERRITORY1", names_glue = "border_{.name}") %>% 
  # Correction for Pacific
  mutate(border_east = ifelse(region == "Pacific", "Pitcairn", border_east),
         border_west = ifelse(region == "Pacific", "Palau", border_west))

# 5. Region latitudinal and longitudinal axes ----

## 5.1 Create the function -----

distance_points <- function(region_i){
  
  require(sf)
  require(units)
  
  data_bbox <- data_regions %>% 
    filter(region == region_i) %>% 
    st_bbox()
  
  point_long_min <- st_sfc(st_point(c(as.numeric(data_bbox$xmin), as.numeric(data_bbox$ymin)))) %>% 
    st_set_crs(4326)
  
  point_long_max <- st_sfc(st_point(c(as.numeric(data_bbox$xmax), as.numeric(data_bbox$ymin)))) %>% 
    st_set_crs(4326)
  
  point_lat_min <- st_sfc(st_point(c(as.numeric(data_bbox$xmin), as.numeric(data_bbox$ymin)))) %>% 
    st_set_crs(4326)
  
  point_lat_max <- st_sfc(st_point(c(as.numeric(data_bbox$xmin), as.numeric(data_bbox$ymax)))) %>% 
    st_set_crs(4326)
  
  plot <- ggplot() +
    geom_sf(data = data_regions %>% 
              filter(region == region_i)) +
    geom_sf(data = st_combine(c(point_lat_min, point_lat_max)) %>% st_cast("LINESTRING"), color = "red") +
    geom_sf(data = st_combine(c(point_long_min, point_long_max)) %>% st_cast("LINESTRING"), color = "blue")
  
  print(plot)
  
  result <- tibble(region = region_i,
                   lat_axis = round(as.numeric(st_distance(point_lat_min, point_lat_max))/1000, -2),
                   long_axis = round(as.numeric(st_distance(point_long_min, point_long_max))/1000, -2))
  
  return(result)
  
}

## 5.2 Map over the function ----

data_distance <- map(unique(data_countries$region), ~distance_points(region_i = .x)) %>% 
  list_rbind() %>% 
  # Correction for Pacific
  mutate(long_axis = ifelse(region == "Pacific", 11400, long_axis))

# 6. Combine and export data ----

data_countries %>%
  select(region, nb_countries, denomination) %>% 
  distinct() %>% 
  left_join(., data_orientation) %>% 
  left_join(., data_distance) %>% 
  write.csv(., file = "figs/08_text-gen/region_characteristics.csv", row.names = FALSE)
