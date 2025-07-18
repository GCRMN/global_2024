# 1. Load packages ----

library(tidyverse)
library(sf)
sf_use_s2(FALSE)

# 2. Load data ----

## 2.1 Cyclones lines ----

load("data/07_cyclones/01_cyclones_lines.RData")

## 2.2 Cyclones points ----

load("data/07_cyclones/01_cyclones_points.RData")

## 2.3 Coral reef distribution ----

data_reef <- st_read("data/01_maps/02_clean/01_reefs/reefs.shp")

## 2.4 Coral reef distribution 100 km buffer --

data_reefs_buffer <- st_read("data/01_maps/02_clean/02_reefs-buffer/reefs_buffer_100.shp") %>% 
  st_make_valid() %>% 
  group_by(subregion) %>% 
  summarise(geometry = st_union(geometry)) %>% 
  ungroup() 

# 3. Extract tropical storms ----

## 3.1 Create the function ----

extract_cyclone <- function(subregion_i){
  
  # Filter data for area
  
  data_reefs_buffer_i <- data_reefs_buffer %>% 
    filter(subregion == subregion_i)
  
  data_reef_i <- data_reef %>% 
    filter(subregion == subregion_i) %>% 
    summarise(geometry = st_union(geometry))
  
  # Extract tropical storms passing within 100 km from a reef
  
  data_ts_lines_i <- st_filter(data_ts_lines, data_reefs_buffer_i, .predicate = st_intersects) %>% 
    select(-type) %>% 
    mutate(dist = as.numeric(st_distance(data_reef_i, .))/1000) %>% 
    st_drop_geometry()
  
  print(subregion_i)
  print(data_ts_lines_i)
  
  if(nrow(data_ts_lines_i) > 0){
    
    # Extract characteristics of the tropical storm
    
    data_res <- NULL
    
    for(i in unique(data_ts_lines_i$ts_id)){
      
      data_ts_points_i <- data_ts_points %>% 
        filter(ts_id == i)
      
      data_ts_points_i <- data_ts_points_i[st_nearest_feature(data_reef_i, data_ts_points_i),] %>% 
        st_drop_geometry()
      
      data_res <- bind_rows(data_res, data_ts_points_i)
      
    }
    
    # Return the results
    
    results <- left_join(data_res, data_ts_lines_i) %>% 
      mutate(subregion = subregion_i, .before = "ts_id")
    
    return(results)
    
  }
  
}

## 3.2 Map over the function ----

data_cyclones <- map(unique(data_reef$subregion), ~extract_cyclone(subregion_i = .)) %>% 
  list_rbind()

## 3.3 Export the results ----

save(data_cyclones, file = "data/07_cyclones/02_cyclones_extracted.RData")
