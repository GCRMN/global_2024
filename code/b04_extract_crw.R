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

# 2. Load and transform coral reefs ----

## 2.1 Load file ----

data_reefs <- st_read("data/01_maps/02_clean/01_reefs/reefs.shp")

## 2.2 Add regions and summarise ----

data_reefs_a <- data_reefs %>% 
  group_by(region) %>%
  summarise(do_union = TRUE) %>% 
  ungroup() %>% 
  mutate(subregion = "All")

## 2.3 Add global reefs ----

data_reefs_b <- data_reefs %>% 
  summarise(do_union = TRUE) %>% 
  mutate(region = "All", subregion = "All")

## 2.4 Bind shp ----

data_reefs <- data_reefs %>% 
  bind_rows(., data_reefs_a) %>% 
  bind_rows(., data_reefs_b)

## 2.5 Remove useless objects ----

rm(data_reefs_a, data_reefs_b)

# 3. Extract yearly SST anomaly ----

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


# dhw percent
# daily sst


