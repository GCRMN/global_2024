# 1. Load packages ----

library(tidyverse)
library(readxl)
library(sf)

# 2. Load data ----

data_subregion <- st_read("data/01_maps/02_clean/04_subregions/gcrmn_subregions.shp") %>% 
  st_drop_geometry()

data_region <- data_subregion %>% 
  select(region) %>% 
  distinct()

load("data/02_misc/data-benthic.RData")

data_sources <- read_xlsx("C:/Users/jerem/Desktop/Recherche/03_projects/2022-02-10_gcrmndb_benthos/gcrmndb_benthos/data/05_data-sources.xlsx") %>% 
  select(datasetID, rightsHolder) %>% 
  distinct()

# 3. DatasetID per region ----

data_benthic %>% 
  select(region, datasetID) %>% 
  distinct() %>% 
  arrange(region, datasetID) %>% 
  # To add datasets names
  left_join(., data_sources) %>% 
  mutate(datasetID = paste0(rightsHolder, " (", datasetID, ")")) %>% 
  # Collapse
  group_by(region) %>% 
  summarise(datasetID = paste0(datasetID, collapse = ", ")) %>% 
  ungroup() %>% 
  left_join(data_region, .) %>%
  arrange(region) %>% 
  openxlsx::write.xlsx(., file = "figs/05_supp-mat/tbl-1_datasetid-per-region.xlsx")

# 4. DatasetID per subregion ----

## 4.1 Supplementary Materials ----

data_benthic %>% 
  select(subregion, datasetID) %>% 
  distinct() %>% 
  arrange(subregion, datasetID) %>% 
  # To add datasets names
  left_join(., data_sources) %>% 
  mutate(datasetID = paste0(rightsHolder, " (", datasetID, ")")) %>% 
  # Collapse
  group_by(subregion) %>% 
  summarise(datasetID = paste0(datasetID, collapse = ", ")) %>% 
  ungroup() %>% 
  left_join(data_subregion %>% select(-region), .) %>%
  arrange(subregion) %>% 
  openxlsx::write.xlsx(., file = "figs/05_supp-mat/tbl-2_datasetid-per-subregion.xlsx")

## 4.2 Additional materials ----

data_subregion <- data_benthic %>% 
  select(region, subregion, datasetID) %>% 
  distinct() %>% 
  arrange(region, subregion, datasetID) %>% 
  # To add datasets names
  left_join(., data_sources) %>% 
  rename(datasetName = rightsHolder)

export_datasetID <- function(region_i){
  
  data_subregion %>%
    filter(region == region_i) %>% 
    select(-region) %>% 
    openxlsx::write.xlsx(., file = paste0("figs/06_additional/01_misc/list-datasetid_",
                                          str_replace_all(str_to_lower(region_i), " ", "-"),
                                          ".xlsx"))
  
}

map(data_region$region, ~export_datasetID(region_i = .x))

rm(data_subregion, export_datasetID)

# 5. List of contributors per datasetID ----

read_xlsx("C:/Users/jerem/Desktop/Recherche/03_projects/2022-02-10_gcrmndb_benthos/gcrmndb_benthos/data/05_data-sources.xlsx") %>% 
  filter(datasetID %in% unique(data_benthic$datasetID)) %>% 
  select(datasetID, rightsHolder, last_name, first_name, email) %>% 
  openxlsx::write.xlsx(., file = "figs/06_additional/05_contributors/contributors_datasetid.xlsx")

# 6. List of contributors emails ----

read_xlsx("C:/Users/jerem/Desktop/Recherche/03_projects/2022-02-10_gcrmndb_benthos/gcrmndb_benthos/data/05_data-sources.xlsx") %>% 
  filter(datasetID %in% unique(data_benthic$datasetID)) %>% 
  select(last_name, first_name, email) %>% 
  distinct() %>% 
  arrange(last_name) %>% 
  openxlsx::write.xlsx(., file = "figs/06_additional/05_contributors/contributors_contacts.xlsx")

# 7. List of contributors' names per region ----

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
  openxlsx::write.xlsx(., file = "figs/06_additional/05_contributors/contributors_region.xlsx")

# 8. List of contributors' contacts per region ----

read_xlsx("C:/Users/jerem/Desktop/Recherche/03_projects/2022-02-10_gcrmndb_benthos/gcrmndb_benthos/data/05_data-sources.xlsx") %>% 
  filter(datasetID %in% unique(data_benthic$datasetID)) %>% 
  select(datasetID, rightsHolder, last_name, first_name, affiliation, email) %>% 
  full_join(data_benthic %>% 
              select(region, datasetID) %>% 
              distinct(),
            .) %>% 
  select(region, last_name, first_name, email) %>% 
  arrange(region, last_name) %>% 
  distinct() %>% 
  drop_na(last_name) %>% 
  openxlsx::write.xlsx(., file = "figs/06_additional/05_contributors/contributors_region_contacts.xlsx")
