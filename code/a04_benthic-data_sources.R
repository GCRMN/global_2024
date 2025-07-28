# 1. Load packages ----

library(tidyverse)
library(readxl)
library(sf)

# 2. Load data ----

data_region <- st_read("data/01_maps/02_clean/03_regions/gcrmn_regions.shp")

load("data/02_misc/data-benthic.RData")

# 3. DatasetID per region ----

data_benthic %>% 
  select(region, datasetID) %>% 
  distinct() %>% 
  arrange(region, datasetID) %>% 
  group_by(region) %>% 
  summarise(datasetID = paste0(datasetID, collapse = ", ")) %>% 
  left_join(data_region %>% st_drop_geometry(), .) %>% 
  arrange(region) %>% 
  openxlsx::write.xlsx(., file = "figs/05_supp-mat/tbl-1_datasetid-per-region.xlsx")

# 4. List of contributors per datasetID ----

read_xlsx("C:/Users/jerem/Desktop/Recherche/03_projects/2022-02-10_gcrmndb_benthos/gcrmndb_benthos/data/05_data-sources.xlsx") %>% 
  filter(datasetID %in% unique(data_benthic$datasetID)) %>% 
  select(datasetID, rightsHolder, last_name, first_name, email) %>% 
  openxlsx::write.xlsx(., file = "figs/05_supp-mat/tbl-2_contacts-contributors-per-datasetid.xlsx")

# 5. List of contributors emails ----

read_xlsx("C:/Users/jerem/Desktop/Recherche/03_projects/2022-02-10_gcrmndb_benthos/gcrmndb_benthos/data/05_data-sources.xlsx") %>% 
  filter(datasetID %in% unique(data_benthic$datasetID)) %>% 
  select(last_name, first_name, email) %>% 
  distinct() %>% 
  arrange(last_name) %>% 
  openxlsx::write.xlsx(., file = "figs/05_supp-mat/tbl-3_list-contacts-data-contributors.xlsx")

# 6. List of contributors' names per region ----

read_xlsx("C:/Users/jerem/Desktop/Recherche/03_projects/2022-02-10_gcrmndb_benthos/gcrmndb_benthos/data/05_data-sources.xlsx") %>% 
  filter(datasetID %in% unique(data_benthic$datasetID)) %>% 
  select(datasetID, last_name, first_name) %>% 
  full_join(data_benthic %>% 
              select(datasetID, region) %>% 
              distinct(),
            .) %>% 
  drop_na(last_name) %>% 
  arrange(region, last_name) %>% 
  mutate(last_name = str_to_title(last_name),
         name = paste0(first_name, " ", last_name)) %>% 
  select(-datasetID, -last_name, -first_name) %>% 
  distinct() %>% 
  group_by(region) %>% 
  mutate(name = paste0(name, collapse = ", ")) %>% 
  distinct() %>% 
  openxlsx::write.xlsx(., file = "figs/05_supp-mat/tbl-4_data-contributors-names-per-datasetid.xlsx")

# 7. List of contributors' contacts per region ----

read_xlsx("C:/Users/jerem/Desktop/Recherche/03_projects/2022-02-10_gcrmndb_benthos/gcrmndb_benthos/data/05_data-sources.xlsx") %>% 
  filter(datasetID %in% unique(data_benthic$datasetID)) %>% 
  select(datasetID, rightsHolder, last_name, first_name, affiliation, email) %>% 
  full_join(data_benthic %>% 
              select(region, datasetID) %>% 
              distinct(),
            .) %>% 
  arrange(region, datasetID) %>% 
  distinct() %>% 
  openxlsx::write.xlsx(., file = "figs/05_supp-mat/tbl-5_contacts-contributors-per-region.xlsx")
