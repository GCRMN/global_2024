# 1. Required packages and functions ----

library(tidyverse)
library(sf)
sf_use_s2(TRUE)

# 2. Download data ----

download_data <- TRUE

if(download_data == TRUE){
  
  download.file(url = "https://www.ncei.noaa.gov/data/international-best-track-archive-for-climate-stewardship-ibtracs/v04r01/access/shapefile/IBTrACS.since1980.list.v04r01.points.zip",
                destfile = "data/07_cyclones/IBTrACS.since1980.list.v04r01.points.zip", mode = "wb")
  
  unzip("data/07_cyclones/IBTrACS.since1980.list.v04r01.points.zip", exdir = "data/07_cyclones/")
  
  file.remove("data/07_cyclones/IBTrACS.since1980.list.v04r01.points.zip")
  
}

# 3. Clean data ----

data_ts_points <- st_read("data/07_cyclones/IBTrACS.since1980.list.v04r01.points.shp") %>% 
  st_transform(crs = 4326) %>% 
  st_make_valid() %>%  
  select(SID, NAME, ISO_TIME, ends_with("WIND"), -WMO_WIND) %>% 
  # Coalesce to put all wind speed in a unique column
  mutate(windspeed = coalesce(USA_WIND, TOK_WIND, CMA_WIND, HKO_WIND, KMA_WIND,
                               NEW_WIND, REU_WIND, BOM_WIND, NAD_WIND, WEL_WIND,
                               DS8_WIND, TD6_WIND, TD5_WIND, NEU_WIND, MLC_WIND)) %>% 
  select(-ends_with("WIND")) %>% 
  rename(ts_id = SID, ts_name = NAME, time = ISO_TIME) %>% 
  mutate(time = as_date(time),
         ts_name = str_replace_all(ts_name, "UNNAMED", NA_character_),
         windspeed = ifelse(windspeed < 0, abs(windspeed), windspeed), # Absolute value if negative
         windspeed = windspeed*1.852) %>% # Convert from knots to km/h
  # Add max_windspeed and saffir simpson scale level
  group_by(ts_id) %>% 
  mutate(max_windspeed = max(windspeed, na.rm = TRUE)) %>% 
  filter(time >= as.Date("1980-01-01") & time <= as.Date("2024-12-31")) %>% 
  filter(max_windspeed != -Inf) %>% 
  ungroup() %>% 
  mutate(saffir = case_when(max_windspeed < 119 ~ 0,
                            max_windspeed >= 119 & max_windspeed <= 153 ~ 1,
                            max_windspeed > 153 & max_windspeed <= 177 ~ 2,
                            max_windspeed > 177 & max_windspeed <= 210 ~ 3,
                            max_windspeed > 210 & max_windspeed <= 251 ~ 4,
                            max_windspeed > 251 ~ 5))

# 4. Export the data ----

save(data_ts_points, file = "data/07_cyclones/01_cyclones_points.RData")

# 5. Transform points to lines ----

data_ts_lines <- data_ts_points %>% 
  # Identify points lower or greater than 180
  mutate(lat = st_coordinates(.)[,2],
         long = st_coordinates(.)[,1],
         type = sign(long)) %>% 
  # Convert to lines
  group_by(ts_id, type) %>%
  summarise(do_union = FALSE) %>%
  st_cast("LINESTRING") %>% 
  # Remove linestring with only one point (else error with the st_intersection() function)
  mutate(n = str_count(geometry, ",")) %>% 
  filter(n > 1) %>% 
  select(-n) %>% 
  ungroup() %>% 
  st_make_valid() %>% 
  st_wrap_dateline(options = c("WRAPDATELINE=YES")) %>% 
  st_make_valid()

# 6. Visual check ----

ggplot() +
  geom_sf(data = data_ts_lines)

# 7. Export the data ----

save(data_ts_lines, file = "data/07_cyclones/01_cyclones_lines.RData")
