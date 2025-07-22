# 1. Required packages ----

library(tidyverse)
library(terra)
library(sf)
sf_use_s2(FALSE)
library(future)
library(furrr)
library(RcppRoll)
source("code/function/extract_coeff.R")

plan(multisession, workers = 4) # Set parallelization with 4 cores

# 2. Load and transform data ----

## 2.1 Transform coral reefs ----

data_reefs <- st_read("data/01_maps/02_clean/01_reefs/reefs.shp")

data_reefs_a <- data_reefs %>% 
  group_by(region) %>%
  summarise(do_union = TRUE) %>% 
  ungroup() %>% 
  mutate(subregion = "All")

data_reefs_b <- data_reefs %>% 
  summarise(do_union = TRUE) %>% 
  mutate(region = "All", subregion = "All")

data_reefs <- data_reefs %>% 
  bind_rows(., data_reefs_a) %>% 
  bind_rows(., data_reefs_b)

rm(data_reefs_a, data_reefs_b)

## 2.2 Transform regions ----

data_regions <- st_read("data/01_maps/02_clean/03_regions/gcrmn_regions.shp")

data_subregions <- st_read("data/01_maps/02_clean/04_subregions/gcrmn_subregions.shp")

data_vect <- data_regions %>% 
  summarise(do_union = TRUE) %>% 
  mutate(region = "All", subregion = "All") %>% 
  bind_rows(., data_regions %>% mutate(subregion = "All")) %>% 
  bind_rows(., data_subregions)

rm(data_regions, data_subregions)

# 3. Extract yearly mean SST anomaly ----

## 3.1 List of files ----

list_url <- data.frame(year = seq(from = 1986, to = 2024, by = 1)) %>% 
  mutate(url = paste0("https://www.star.nesdis.noaa.gov/pub/socd/mecb/crw/data/5km/v3.1_op/nc/v1.0/annual/ct5km_",
                      "ssta-mean",
                      "_v3.1_",
                      year,
                      ".nc"),
         filename = str_split_fixed(url, "/", Inf)[,14])

## 3.2 Function to download raster and extract values ----

extract_ssta_mean_year <- function(row_nb, data_reef = data_reef){
  
  # A. Remove files in folder
  
  file.remove(list.files("data/04_crw/", full.names = TRUE))
  
  # B. Download file
  
  list_url_i <- list_url %>% 
    filter(row_number(.) == row_nb)
  
  download.file(url = list_url_i[1, "url"],
                destfile = paste0("data/04_crw/", list_url_i[1, "filename"]),
                mode = "wb",   # Use mode "wb" for windows otherwise issue to read the file with terra
                timeout = max(600, getOption("timeout"))) # 600 seconds to download the file, else error message
  
  # C. Load the raster
  
  ncdf <- terra::rast(paste0("data/04_crw/", list_url_i[1, "filename"]))
  
  ncdf <- ncdf$sea_surface_temperature_anomaly
  
  crs(ncdf) <- "epsg:4326"
  
  # D. Extract values
  
  data_results <- terra::extract(x = ncdf, y = data_reef, fun = mean, na.rm = TRUE) %>% 
    as_tibble() %>% 
    dplyr::select("ID", "sea_surface_temperature_anomaly") %>% 
    dplyr::mutate(year = year(unique(time(ncdf)))) %>% 
    dplyr::rename(ssta_mean = sea_surface_temperature_anomaly)
  
  # E. Delete raw file
  
  file.remove(paste0("data/04_crw/", list_url_i[1, "filename"]))
  
  # F. Return the results
  
  return(data_results)
  
}

## 3.3. Map over the function ----

data_ssta_mean_year <- map(1:nrow(list_url), ~extract_ssta_mean_year(row_nb = ., data_reef = data_reefs)) %>% 
  list_rbind() %>% 
  left_join(., data_reefs %>% 
              st_drop_geometry() %>% 
              mutate(ID = row_number())) %>% 
  select(-ID)

## 3.4 Save the data ----

save(data_ssta_mean_year, file = "data/02_misc/data_ssta_mean_year.RData")

# 4. Extract yearly DHW ----

## 4.1 List of files ----

list_url <- data.frame(year = seq(from = 1986, to = 2024, by = 1)) %>% 
  mutate(url = paste0("https://www.star.nesdis.noaa.gov/pub/socd/mecb/crw/data/5km/v3.1_op/nc/v1.0/annual/ct5km_",
                      "dhw-max",
                      "_v3.1_",
                      year,
                      ".nc"),
         filename = str_split_fixed(url, "/", Inf)[,14])

## 4.2 Function to download raster and extract values ----

extract_dhw_max_percent_year <- function(row_nb, data_reef = data_reefs, data_vect = data_vect){
  
  # A. Remove files in folder
  
  file.remove(list.files("data/04_crw/", full.names = TRUE))
  
  # B. Download file
  
  list_url_i <- list_url %>% 
    filter(row_number(.) == row_nb)
  
  download.file(url = list_url_i[1, "url"],
                destfile = paste0("data/04_crw/", list_url_i[1, "filename"]),
                mode = "wb",   # Use mode "wb" for windows otherwise issue to read the file with terra
                timeout = max(600, getOption("timeout"))) # 600 seconds to download the file, else error message
  
  # C. Load the raster
  
  ncdf <- terra::rast(paste0("data/04_crw/", list_url_i[1, "filename"]))
  
  ncdf <- ncdf$degree_heating_week
  
  crs(ncdf) <- "epsg:4326"
  
  # D. Extract values
  
  ncdf_crop <- crop(ncdf, data_reefs, mask = TRUE)
  
  data_results <- freq(ncdf_crop, zones = vect(data_vect)) %>% 
    dplyr::mutate(year = year(unique(time(ncdf)))) %>% 
    left_join(., data_vect %>% st_drop_geometry() %>% mutate(zone = row_number())) %>% 
    select(-layer, -zone) %>% 
    rename(dhw = value, nb_cells = count)
  
  # E. Delete raw file
  
  file.remove(paste0("data/04_crw/", list_url_i[1, "filename"]))
  
  # F. Return the results
  
  return(data_results)
  
}

## 4.3 Map over the function ----

data_dhw_freq <- map(1:nrow(list_url),
                     ~extract_dhw_max_percent_year(row_nb = ., data_reef = data_reefs, data_vect = data_vect)) %>% 
  list_rbind()

## 4.4 Save the data ----

save(data_dhw_freq, file = "data/02_misc/data_dhw_freq.RData")

# 5. Extract daily mean SST ----

## 5.1 List of files ----

## 5.2 Function to download raster and extract values ----

## 5.3. Map over the function ----
