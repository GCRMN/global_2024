# 1. Required packages ----

library(tidyverse)
library(terra)
library(sf)
sf_use_s2(FALSE)

# 2. Load site coordinates data ----

site_coords <- st_read("data/03_site-coords/site-coords_all.shp")

# 3. List of ncdf files ----

ncdf_files <- list.files("data/05_crw_year/", full.names = TRUE) %>% 
  as_tibble() %>% 
  mutate(year = as.numeric(str_sub(value, start = -7, end = -4)),
         pred = paste("pred",
                      str_split_fixed(value, "_|/", 8)[,5],
                      str_split_fixed(value, "_|/", 8)[,6],
                      sep = "_"))

# 4. Create a function to extract value for each site ----

extract_ncdf <- function(path_i){
  
  # 1. Filter path i
  
  ncdf_files_i <- ncdf_files %>% 
    filter(value == path_i)
  
  # 2. Load the raster
  
  if(ncdf_files_i$pred %in% c("pred_ssta_max", "pred_ssta_mean")){
    
    ncdf_i <- terra::rast(ncdf_files_i$value)$sea_surface_temperature_anomaly
    
  }else if(ncdf_files_i$pred == "pred_dhw_max"){
    
    ncdf_i <- terra::rast(ncdf_files_i$value)$degree_heating_week
    
  }else if(ncdf_files_i$pred %in% c("pred_sst_min", "pred_sst_mean", "pred_sst_max")){
    
    ncdf_i <- terra::rast(ncdf_files_i$value)$sea_surface_temperature
    
  }
  
  crs(ncdf_i) <- "epsg:4326"
  
  # 3. Extract values
  
  results <- terra::extract(x = ncdf_i, y = site_coords, method = "bilinear") %>% 
    as_tibble() %>% 
    rename(!!ncdf_files_i$pred := 2) %>% 
    mutate(year = ncdf_files_i$year) %>% 
    bind_cols(site_coords %>% st_drop_geometry(), .) %>% 
    select(-ID)
  
  return(results)
  
}

# 5. Map over the function ----

## 5.1 pred_dhw_max ---- 

pred_dhw_max <- map_dfr(ncdf_files %>% 
                          filter(ncdf_files$pred == "pred_dhw_max") %>%
                          select(value) %>%
                          pull(),
                ~extract_ncdf(path_i = .))

write.csv(pred_dhw_max, file = "data/08_predictors/pred_dhw_max.csv", row.names = FALSE)

## 5.2 pred_ssta_max ---- 

pred_ssta_max <- map_dfr(ncdf_files %>% 
                          filter(ncdf_files$pred == "pred_ssta_max") %>%
                          select(value) %>%
                          pull(),
                        ~extract_ncdf(path_i = .))

write.csv(pred_ssta_max, file = "data/08_predictors/pred_ssta_max.csv", row.names = FALSE)

## 5.3 pred_ssta_mean ---- 

pred_ssta_mean <- map_dfr(ncdf_files %>% 
                          filter(ncdf_files$pred == "pred_ssta_mean") %>%
                          select(value) %>%
                          pull(),
                        ~extract_ncdf(path_i = .))

write.csv(pred_ssta_mean, file = "data/08_predictors/pred_ssta_mean.csv", row.names = FALSE)

## 5.4 pred_sst_min ---- 

pred_sst_min <- map_dfr(ncdf_files %>% 
                            filter(ncdf_files$pred == "pred_sst_min") %>%
                            select(value) %>%
                            pull(),
                          ~extract_ncdf(path_i = .))

write.csv(pred_sst_min, file = "data/08_predictors/pred_sst_min.csv", row.names = FALSE)

## 5.5 pred_sst_mean ---- 

pred_sst_mean <- map_dfr(ncdf_files %>% 
                          filter(ncdf_files$pred == "pred_sst_mean") %>%
                          select(value) %>%
                          pull(),
                        ~extract_ncdf(path_i = .))

write.csv(pred_sst_mean, file = "data/08_predictors/pred_sst_mean.csv", row.names = FALSE)

## 5.6 pred_sst_max ---- 

pred_sst_max <- map_dfr(ncdf_files %>% 
                           filter(ncdf_files$pred == "pred_sst_max") %>%
                           select(value) %>%
                           pull(),
                         ~extract_ncdf(path_i = .))

write.csv(pred_sst_max, file = "data/08_predictors/pred_sst_max.csv", row.names = FALSE)
