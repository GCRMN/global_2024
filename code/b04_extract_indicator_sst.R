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

# 3. List files of SST to download ----

list_url <- data.frame(date = seq(from = ymd("1985-01-01"), to = ymd("2023-12-31"), by = "1 day")) %>% 
  mutate(year = year(date),
         date = str_remove_all(date, "-"),
         url = paste0("https://www.star.nesdis.noaa.gov/pub/socd/mecb/crw/data/5km/v3.1_op/nc/v1.0/daily/sst/",
                      year,
                      "/coraltemp_v3.1_",
                      date,
                      ".nc"),
         filename = str_split_fixed(url, "/", Inf)[,16])

# 4. Create the function to extract daily mean SST over territories' coral reefs ----

extract_sst <- function(row_nb, data_reef = data_reefs){
  
  # 1. Download file
  
  list_url_i <- list_url %>% 
    filter(row_number(.) == row_nb)
  
  # Use mode "wb" for windows otherwise issue to read the file with terra
  download.file(url = list_url_i[1, "url"],
                destfile = paste0("data/04_sst/", list_url_i[1, "filename"]),
                mode = "wb",
                timeout = max(300, getOption("timeout"))) # 300 seconds to download the file, else error message
  
  # 2. Load the raster
  
  ncdf <- terra::rast(paste0("data/04_sst/", list_url_i[1, "filename"]))$analysed_sst
  
  crs(ncdf) <- "epsg:4326"
  
  # 3. Extract SST
  
  sst_i <- terra::extract(x = ncdf, y = data_reef, fun = mean, na.rm = TRUE) %>% 
    as_tibble() %>% 
    dplyr::select("ID", "analysed_sst") %>% 
    dplyr::mutate(date = unique(time(ncdf)))
  
  # 4. Delete raw file
  
  file.remove(paste0("data/04_sst/", list_url_i[1, "filename"]))
  
  # 5. Return the results
  
  return(sst_i)
  
}

# 5. Map over the function ----

data_sst <- future_map_dfr(1:100, ~extract_sst(row_nb = ., data_reef = data_reefs)) %>% 
  rename(sst = analysed_sst) %>% 
  left_join(., data_reefs %>% 
              st_drop_geometry() %>% 
              mutate(ID = row_number())) %>% 
  select(-ID)



















# 6. Export the data ----

save(data_sst, file = "data/09_misc/data-sst_raw.RData")

# 7. Derive other SST metrics ----

## 7.1 Calculate long-term average SST ----

data_sst <- data_sst %>% 
  group_by(TERRITORY1) %>% 
  mutate(mean_sst = mean(sst, na.rm = TRUE)) %>% 
  ungroup()

## 7.2 Map over extract_coeff function ----

data_warming <- data_sst %>% 
  # Convert date as numeric
  mutate(date = as.numeric(as_date(date))) %>% 
  # Extract linear model coefficients
  group_by(TERRITORY1) %>% 
  group_modify(~extract_coeff(data = .x, var_y = "sst", var_x = "date")) %>% 
  ungroup() %>% 
  # Calculate increase in SST over the period
  mutate(min_date = as.numeric(as_date(min(data_sst$date))),
         max_date = as.numeric(as_date(max(data_sst$date)))) %>% 
  mutate(sst_increase = ((max_date)*slope+intercept) - ((min_date)*slope+intercept)) %>% 
  select(-min_date, -max_date) %>% 
  # Calculate the warming rate (°C per year)
  mutate(warming_rate = sst_increase/(year(max(data_sst$date))-year(min(data_sst$date)))) %>% 
  # Add mean_sst for each territory
  left_join(., data_sst %>% 
              select(TERRITORY1, mean_sst) %>% 
              distinct())

write.csv(data_warming, "data/09_misc/data-warming.csv", row.names = FALSE)

## 7.3 Calculate SST anomaly ----

data_sst <- data_sst %>% 
  group_by(TERRITORY1) %>% 
  mutate(mean_sst = mean(sst, na.rm = TRUE),
         sst_anom = sst - mean_sst,
         sst_anom_mean = roll_mean(x = sst_anom, n = 365, align = "center", fill = NA)) %>% 
  ungroup()

save(data_sst, file = "data/09_misc/data-sst_processed.RData")
