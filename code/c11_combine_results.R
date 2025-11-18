# 1. Load packages ----

library(tidyverse) # Core tidyverse packages

# 2. Source functions ----

source("code/function/combine_model_data.R")

# 3.1 Combine Hierarchical Bayesian Model (HBM) results ----

data_hbm_region <- readRDS(file = "data/13_model-murray/benthic_regions.rds")

data_hbm_region <- data_hbm_region %>% 
  select(-name, -data_years, -xgboost) %>% 
  unnest(stan) %>% 
  select(-variable, -upper_80, -lower_80) %>% 
  mutate(across(c(median, lower, upper), ~.x*100)) %>% 
  rename(year = Year, mean = median, lower_ci_95 = lower, upper_ci_95 = upper) %>% 
  mutate(level = "region", model = "HBM", subregion = NA)

data_hbm_subregion <- readRDS(file = "data/13_model-murray/benthic_subregions.rds")

data_hbm_subregion <- data_hbm_subregion %>% 
  select(-name, -data_years, -xgboost) %>% 
  unnest(stan) %>% 
  select(-variable, -upper_80, -lower_80) %>% 
  mutate(across(c(median, lower, upper), ~.x*100)) %>% 
  rename(year = Year, mean = median, lower_ci_95 = lower, upper_ci_95 = upper) %>% 
  mutate(level = "subregion", model = "HBM")

data_hbm <- bind_rows(data_hbm_region, data_hbm_subregion)

# 3.2 Combine Machine Learning Model (ML) results ----

data_ml_results <- combine_model_data(save_results = FALSE)

data_ml <- data_ml_results$result_trends %>% 
  # Calculate mean and confidence interval
  rename(cover = mean) %>% 
  group_by(category, level, region, subregion, ecoregion, year) %>% 
  summarise(mean = mean(cover),
            lower_ci_95 = quantile(cover, 0.025),
            upper_ci_95 = quantile(cover, 0.975)) %>% 
  ungroup() %>% 
  # Replace negative values by 0
  mutate(across(c(mean, lower_ci_95, upper_ci_95), ~ifelse(.x < 0, 0, .x))) %>% 
  mutate(model = "ML")

# 3.3 Combine data from both models ----

data_models <- bind_rows(data_ml, data_hbm) %>% 
  mutate(across(c(mean, lower_ci_95, upper_ci_95), ~round(.x, 2)))

rm(data_hbm_region, data_hbm_subregion, data_hbm, data_ml)

# 3.4 Add observed data, first year, and last year variables ----

load("data/11_model-data/data_benthic_prepared.RData")

data_obs <- 
  # ecoregion level
  data_benthic %>% 
  select(year, category, region, subregion, ecoregion, measurementValue) %>% 
  group_by(year, category, region, subregion, ecoregion) %>% 
  count(name = "data_obs") %>% 
  ungroup() %>% 
  mutate(level = "ecoregion") %>% 
  # subregion level
  bind_rows(., data_benthic %>% 
              select(year, category, region, subregion, measurementValue) %>% 
              group_by(year, category, region, subregion) %>% 
              count(name = "data_obs") %>% 
              ungroup() %>% 
              mutate(level = "subregion")) %>% 
  # region level
  bind_rows(., data_benthic %>% 
              select(year, category, region, measurementValue) %>% 
              group_by(year, category, region) %>% 
              count(name = "data_obs") %>% 
              ungroup() %>% 
              mutate(level = "region")) %>% 
  # global level
  bind_rows(., data_benthic %>% 
              select(year, category, measurementValue) %>% 
              group_by(year, category) %>% 
              count(name = "data_obs") %>% 
              ungroup() %>% 
              mutate(level = "global")) %>% 
  mutate(data_obs = TRUE) %>% 
  group_by(category, level, region, subregion, ecoregion) %>% 
  # First and last year with data
  mutate(first_year = min(year),
         last_year = max(year)) %>% 
  ungroup() %>% 
  # Generate all possible combinations
  complete(year = seq(1970, 2025, 1), nesting(category, level, region, subregion, ecoregion),
           fill = list(data_obs = FALSE))
  
data_models <- left_join(data_models, data_obs) 
  
# 3.5 Export the results ----
  
save(data_models, file = "data/model-results.RData")
