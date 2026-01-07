# 1. Load packages ----

library(tidyverse) # Core tidyverse packages
library(sf)

# 2 Combine and clean model results ----

data_hbm_global <- readRDS(file = "data/13_model-output_hbm/benthic_global.rds")

data_hbm_global <- data_hbm_global %>% 
  select(-name, -data_years, -xgboost) %>% 
  unnest(stan) %>% 
  select(-variable) %>% 
  mutate(across(c(median, lower, upper, lower_80, upper_80), ~.x*100)) %>% 
  rename(year = Year, mean = median, lower_ci_95 = lower, upper_ci_95 = upper,
         upper_ci_80 = upper_80, lower_ci_80 = lower_80) %>% 
  mutate(level = "global")

data_hbm_region <- readRDS(file = "data/13_model-output_hbm/benthic_regions.rds")

data_hbm_region <- data_hbm_region %>% 
  select(-name, -data_years, -xgboost) %>% 
  unnest(stan) %>% 
  select(-variable) %>% 
  mutate(across(c(median, lower, upper, lower_80, upper_80), ~.x*100)) %>% 
  rename(year = Year, mean = median, lower_ci_95 = lower, upper_ci_95 = upper,
         upper_ci_80 = upper_80, lower_ci_80 = lower_80) %>% 
  mutate(level = "region")

data_hbm_subregion <- readRDS(file = "data/13_model-output_hbm/benthic_subregions.rds")

data_hbm_subregion <- data_hbm_subregion %>% 
  select(-name, -data_years, -xgboost) %>% 
  unnest(stan) %>% 
  select(-variable) %>% 
  mutate(across(c(median, lower, upper, lower_80, upper_80), ~.x*100)) %>% 
  rename(year = Year, mean = median, lower_ci_95 = lower, upper_ci_95 = upper,
         upper_ci_80 = upper_80, lower_ci_80 = lower_80) %>% 
  mutate(level = "subregion")

data_hbm_ecoregion <- readRDS(file = "data/13_model-output_hbm/benthic_ecoregions.rds")

data_hbm_ecoregion <- data_hbm_ecoregion %>% 
  select(-name, -data_years, -xgboost, -data) %>% 
  unnest(stan) %>% 
  select(-variable) %>% 
  mutate(across(c(median, lower, upper, lower_80, upper_80), ~.x*100)) %>% 
  rename(year = Year, mean = median, lower_ci_95 = lower, upper_ci_95 = upper,
         upper_ci_80 = upper_80, lower_ci_80 = lower_80) %>% 
  mutate(level = "ecoregion")

data_models <- bind_rows(data_hbm_global, data_hbm_region,
                      data_hbm_subregion, data_hbm_ecoregion) %>% 
  mutate(across(c(mean, lower_ci_95, upper_ci_95, lower_ci_80, upper_ci_80), ~round(.x, 2))) %>% 
  select(category, level, region, subregion, ecoregion,
         year, mean, lower_ci_95, upper_ci_95, lower_ci_80, upper_ci_80)

# 3. Add observed data, first year, and last year variables ----

load("data/11_model-data/data_benthic_prepared.RData")

data_obs <- 
  # ecoregion level
  data_benthic_prepared %>% 
  select(year, category, region, subregion, ecoregion, measurementValue) %>% 
  group_by(year, category, region, subregion, ecoregion) %>% 
  count(name = "data_obs") %>% 
  ungroup() %>% 
  mutate(level = "ecoregion") %>% 
  # subregion level
  bind_rows(., data_benthic_prepared %>% 
              select(year, category, region, subregion, measurementValue) %>% 
              group_by(year, category, region, subregion) %>% 
              count(name = "data_obs") %>% 
              ungroup() %>% 
              mutate(level = "subregion")) %>% 
  # region level
  bind_rows(., data_benthic_prepared %>% 
              select(year, category, region, measurementValue) %>% 
              group_by(year, category, region) %>% 
              count(name = "data_obs") %>% 
              ungroup() %>% 
              mutate(level = "region")) %>% 
  # global level
  bind_rows(., data_benthic_prepared %>% 
              select(year, category, measurementValue) %>% 
              group_by(year, category) %>% 
              count(name = "data_obs") %>% 
              ungroup() %>% 
              mutate(level = "global")) %>% 
  mutate(data_obs = TRUE) %>% 
  # First and last year with data
  group_by(category, level, region, subregion, ecoregion) %>% 
  mutate(first_year = min(year),
         last_year = max(year)) %>% 
  ungroup() %>% 
  # Generate all possible combinations
  complete(year = seq(1970, 2025, 1),
           nesting(category, level, region, subregion, ecoregion, first_year, last_year),
           fill = list(data_obs = FALSE)) 

data_models <- left_join(data_models, data_obs)

# 4. Add subregion names ----

data_models <- st_read("data/01_maps/02_clean/04_subregions/gcrmn_subregions.shp") %>% 
  st_drop_geometry() %>% 
  rename(subregion = subregn, subregion_name = sbrgn_n) %>% 
  select(subregion, subregion_name) %>% 
  distinct() %>% 
  left_join(data_models, .) %>% 
  relocate("subregion_name", .after = "subregion")

# 5. Export the results ----

save(data_models, file = "data/model-results.RData")
