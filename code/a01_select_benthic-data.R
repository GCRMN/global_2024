# 1. Load packages ----

library(tidyverse) # Core tidyverse packages
library(sf)
sf_use_s2(FALSE)

# 2. Load gcrmndb_benthos data ----

load("C:/Users/jerem/Desktop/Recherche/03_projects/2022-02-10_gcrmndb_benthos/gcrmndb_benthos/data/09_gcrmndb_benthos.RData")

# 3. Filter required data ----

# -------------------------------------------------------- #
# /!\     MAKE SURE TO REMOVE DATASET ID WHOSE USE     /!\ #
# /!\        IS NOT AUTHORISED FOR THIS REPORT         /!\ #
# -------------------------------------------------------- #

data_benthic <- synthetic_data %>% 
  # Remove useless datasets
  filter(!(datasetID %in% c("0009"))) %>%
  filter(!(region == "Brazil" & datasetID == "0015")) %>% 
  # Filter data on the period of interest
  filter(year >= 1970 & year <= 2025) %>% 
  # Filter depth of shallow coral reefs
  filter(is.na(verbatimDepth) | verbatimDepth <= 30)

# 4. Save the data ----

save(data_benthic, file = "data/02_misc/data-benthic.RData")

# 5. Export site coordinates (for predictors extraction) ----

data_benthic %>% 
  select(decimalLatitude, decimalLongitude) %>% 
  distinct() %>% 
  mutate(type = "obs",
         site_id = as.character(row_number(.))) %>% 
  st_as_sf(coords = c("decimalLongitude", "decimalLatitude"), crs = 4326) %>% 
  st_write(., dsn = "data/03_site-coords/global_2024_site-coords_obs.shp",
           delete_dsn = TRUE, delete_layer = TRUE, append = FALSE)
