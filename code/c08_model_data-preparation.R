# 1. Load packages ----

library(tidyverse)
library(googledrive)
library(sf)
sf_use_s2(FALSE)
source("code/function/graphical_par.R")
source("code/function/theme_graph.R")
source("code/function/prepare_benthic_data.R")

# 2. Load data ----

load("data/02_misc/data-benthic.RData")

# 3. Load and combine predictors ----

## 3.1 Add spatial variables to each site ----

### 3.1.1 Region and subregion ----

data_region <- st_read("data/01_maps/02_clean/04_subregions/gcrmn_subregions.shp")

data_predictors <- st_read("data/03_site-coords/global_2024_site-coords_all.shp") %>% 
  st_join(., data_region)

### 3.1.2 Ecoregion ----

data_ecoregion <- st_read("data/01_maps/02_clean/05_ecoregions/gcrmn_ecoregions.shp")

data_predictors <- st_join(data_predictors, data_ecoregion)

### 3.1.3 Country and territory ----

data_eez <- st_read("data/01_maps/01_raw/05_eez/eez_v12.shp") %>% 
  rename(country = SOVEREIGN1, territory = TERRITORY1) %>% 
  select(country, territory)

data_predictors <- st_join(data_predictors, data_eez) %>% 
  bind_cols(., st_coordinates(.)) %>% 
  rename(decimalLatitude = Y, decimalLongitude = X) %>% 
  st_drop_geometry() %>% 
  left_join(data_predictors, .)

rm(data_region, data_ecoregion, data_eez)

## 3.2 Generate all years ----

data_predictors <- data_predictors %>% 
  mutate(decimalLongitude = st_coordinates(.)[,"X"],
         decimalLatitude = st_coordinates(.)[,"Y"]) %>% 
  st_drop_geometry() %>% 
  mutate(site_id = as.numeric(site_id),
         year = 2000) %>% 
  tidyr::complete(year = seq(1980, 2025), nesting(site_id, type, region,
                                                  subregion, ecoregion, country, territory,
                                                  decimalLongitude, decimalLatitude))

## 3.3 Estimate human population for missing years ----

### 3.3.1 Load the data ----

pred_human_pop <- read.csv("data/08_predictors/pred_human-pop.csv") %>% 
  rename(year = system.index) %>% 
  mutate(year = as.numeric(str_split_fixed(year, "_", 9)[,6]))

### 3.3.2 Create the function ----

extract_coeff <- function(data){
  
  model <- lm(pred_population ~ year, data = data)
  
  results <- summary(model)$coefficients
  
  results <- tibble(intercept = results[1, "Estimate"],
                    slope = results[2, "Estimate"])
  
  return(results)
  
}

### 3.3.3 Map over the function ----

pred_human_pop <- pred_human_pop %>% 
  # Extract linear model coefficients
  group_by(site_id, type) %>% 
  group_modify(~extract_coeff(data = .x)) %>% 
  ungroup() %>% 
  left_join(pred_human_pop, .) %>% 
  # Estimate human population for all years between 2000 and 2024
  tidyr::complete(year = seq(2000, 2025), nesting(site_id, type, intercept, slope)) %>% 
  mutate(pred_population = (year*slope)+intercept) %>% 
  select(-intercept, -slope) %>% 
  mutate(pred_population = round(pred_population))

data_predictors <- left_join(data_predictors, pred_human_pop)

## 3.4 Add other predictors ----

data_predictors <- read.csv("data/08_predictors/pred_elevation.csv") %>% 
  mutate(pred_elevation = replace_na(pred_elevation, 0)) %>% 
  left_join(data_predictors, .)

data_predictors <- read.csv("data/08_predictors/pred_land.csv") %>% 
  left_join(data_predictors, .)

data_predictors <- read.csv("data/08_predictors/pred_reef-extent.csv") %>% 
  left_join(data_predictors, .)

data_predictors <- read.csv("data/08_predictors/pred_chla_mean.csv") %>% 
  left_join(data_predictors, .)

data_predictors <- read.csv("data/08_predictors/pred_chla_sd.csv") %>% 
  left_join(data_predictors, .)

data_predictors <- read.csv("data/08_predictors/pred_gravity.csv") %>% 
  left_join(data_predictors, .)

data_predictors <- read.csv("data/08_predictors/pred_enso.csv") %>% 
  left_join(data_predictors, .)

data_predictors <- read.csv("data/08_predictors/pred_sst_mean.csv") %>% 
  arrange(site_id, type, year) %>% 
  group_by(site_id, type) %>% 
  mutate(pred_sst_mean_y1 = lag(pred_sst_mean, n = 1)) %>% 
  left_join(data_predictors, .)

data_predictors <- read.csv("data/08_predictors/pred_sst_max.csv") %>% 
  arrange(site_id, type, year) %>% 
  group_by(site_id, type) %>% 
  mutate(pred_sst_max_y1 = lag(pred_sst_max, n = 1)) %>% 
  left_join(data_predictors, .)

data_predictors <- read.csv("data/08_predictors/pred_sst_min.csv") %>% 
  left_join(data_predictors, .)

data_predictors <- read.csv("data/08_predictors/pred_sst_skewness.csv") %>% 
  left_join(data_predictors, .)

data_predictors <- read.csv("data/08_predictors/pred_sst_sd.csv") %>% 
  left_join(data_predictors, .)

data_predictors <- read.csv("data/08_predictors/pred_dhw_max.csv") %>% 
  arrange(site_id, type, year) %>% 
  group_by(site_id, type) %>% 
  mutate(pred_dhw_max_y1 = lag(pred_dhw_max, n = 1)) %>% 
  left_join(data_predictors, .)

data_predictors <- read.csv("data/08_predictors/pred_ssta_max.csv") %>% 
  arrange(site_id, type, year) %>% 
  group_by(site_id, type) %>% 
  left_join(data_predictors, .)

data_predictors <- read.csv("data/08_predictors/pred_ssta_mean.csv") %>% 
  arrange(site_id, type, year) %>% 
  group_by(site_id, type) %>% 
  left_join(data_predictors, .)

data_predictors <- read.csv("data/08_predictors/pred_cyclones.csv") %>% 
  left_join(data_predictors, .) %>% 
  mutate(across(c("nb_cyclones_y1", "windspeed_y1", "cyclones_freq"), ~ifelse(is.na(.x), 0, .x)))

data_predictors <- read.csv("data/08_predictors/pred_reef-type.csv") %>%
  # See https://developers.google.com/earth-engine/datasets/catalog/ACA_reef_habitat_v2_0
  mutate(reef_type = str_replace_all(reef_type, c("11" = "Shallow Lagoon",
                                                  "12"	= "Deep Lagoon",
                                                  "13"	= "Inner Reef Flat",
                                                  "14"	= "Outer Reef Flat",
                                                  "15"	= "Reef Crest",
                                                  "16"	= "Terrestrial Reef Flat",
                                                  "21"	= "Sheltered Reef Slope",
                                                  "22"	= "Reef Slope",
                                                  "23"	= "Plateau",
                                                  "24"	= "Back Reef Slope",
                                                  "25"	= "Patch Reef"))) %>% 
  left_join(data_predictors, .)

## 3.5 Round values of predictors ----

data_predictors <- data_predictors %>% 
  # Change unit for SST (°C)
  mutate(pred_sst_sd = pred_sst_sd/100) %>%
  # Round to 4 digits
  mutate(across(c(pred_sst_skewness,
                  pred_sst_max, pred_sst_mean,
                  pred_sst_min,
                  pred_dhw_max, pred_dhw_max_y1,
                  pred_sst_max_y1, pred_sst_mean_y1,
                  pred_ssta_max, pred_ssta_mean,
                  pred_elevation, pred_reefextent, pred_land,
                  pred_enso, pred_chla_mean, pred_chla_sd,
                  pred_sst_skewness, pred_elevation, pred_land,
                  pred_reefextent, cyclones_freq, pred_sst_sd),
                ~ round(.x, digits = 3)))

# 4. Feature selection (remove correlated predictors) ----

## 4.1 Find correlation coefficients between predictors ----

data_correlation <- data_predictors %>% 
  select(-site_id, -year, -type, -region, -subregion,
         -ecoregion, -country, -territory, -reef_type) %>% 
  cor(., use = "complete.obs") %>% 
  round(., 2) %>% 
  as_tibble(.) %>% 
  mutate(predictor_a = colnames(.)) %>% 
  pivot_longer(1:ncol(.)-1, names_to = "predictor_b", values_to = "coefficient") %>% 
  filter(predictor_a != predictor_b) %>% 
  arrange(coefficient) %>% 
  # Remove odd row number to remove duplicated pairs of predictors
  filter(row_number(.) %% 2 == 0)

## 4.2 Remove useless predictors based on correlation ----

data_predictors <- data_predictors %>% 
  select(-pred_sst_mean_y1)

# 5. Export predictors for data to predict ----

data_predictors_pred <- data_predictors %>% 
  filter(type == "pred") %>% 
  select(-type, -site_id) %>% 
  mutate(datasetID = NA,
         month = NA,
         verbatimDepth = NA,
         parentEventID = NA)

save(data_predictors_pred, file = "data/11_model-data/data_predictors_pred.RData")

# 6. Summarize data and add predictors ----

data_site_coords_obs <- st_read("data/03_site-coords/global_2024_site-coords_obs.shp") %>% 
  mutate(site_id = as.numeric(site_id),
         decimalLongitude = st_coordinates(.)[,"X"],
         decimalLatitude = st_coordinates(.)[,"Y"]) %>% 
  st_drop_geometry()

data_benthic <- data_benthic %>% 
  # 1. Prepare benthic data
  prepare_benthic_data(data = ., remove_na = FALSE, regenerate_zero = FALSE) %>% 
  # 2. Remove useless variables
  select(-locality, -habitat, -eventDate) %>% 
  # 3. Convert to factors
  mutate(month = as.factor(month)) %>% 
  mutate_if(is.character, factor) %>% 
  # 4. Add site_id and type (to join on step 7)
  left_join(., data_site_coords_obs) %>% 
  # 5. Add predictors
  left_join(., data_predictors %>%
              filter(type == "obs") %>% 
              # Remove lat and long because GEE slightly modify these, which break the join
              select(-decimalLongitude, -decimalLatitude, -region,
                     -subregion, -ecoregion, -country, -territory),
            by = c("site_id", "year", "type")) %>% 
  select(-site_id, -type, -day)

save(data_benthic, file = "data/11_model-data/data_benthic_prepared.RData")

# 7. Check the number of NA per predictors (all) ----

## 7.1 Sites for predictions ----

pred_na_pred <- data_predictors_pred %>% 
  summarise(across(1:ncol(.), ~sum(is.na(.x)))) %>% 
  pivot_longer(1:ncol(.), names_to = "predictor", values_to = "na") %>% 
  mutate(n = nrow(data_predictors_pred),
         percent = (na*100)/n,
         type = "Predictors (Pred.)") %>% 
  select(-n, -na) %>% 
  filter(!(predictor %in% c("measurementValue", "category")))

## 7.2 Sites for observed data ----

pred_na_obs <- data_benthic %>% 
  summarise(across(1:ncol(.), ~sum(is.na(.x)))) %>% 
  pivot_longer(1:ncol(.), names_to = "predictor", values_to = "na") %>% 
  mutate(n = nrow(data_benthic),
         percent = (na*100)/n,
         type = "Predictors (Obs.)") %>% 
  select(-n, -na) %>% 
  filter(!(predictor %in% c("measurementValue", "category")))

## 7.3 Make the plot ----

ggplot(data = bind_rows(pred_na_pred, pred_na_obs),
       aes(x = fct_reorder(predictor, desc(predictor)), y = percent)) +
  geom_bar(stat = "identity", fill = "#c44d56", width = 0.6) +
  coord_flip() +
  lims(y = c(0, 100)) +
  facet_grid(~type) +
  labs(x = NULL, y = "Percentage of NA") +
  theme_graph()

ggsave("figs/06_additional/02_data-exploration/na_predictors_all.png", width = 8, height = 12)

# 8. Check the number of NA per predictors (per year) ----

## 8.1 Sites for predictions ----

pred_na_pred <- data_predictors_pred %>% 
  group_by(year) %>% 
  summarise(across("region":"parentEventID", ~sum(is.na(.x)))) %>% 
  ungroup() %>% 
  pivot_longer(2:ncol(.), names_to = "predictor", values_to = "na") %>% 
  mutate(perc = (na*100)/length(unique(data_predictors %>% filter(type == "pred") %>% select(site_id) %>% pull())),
         type = "Predictors (Pred.)")

## 8.2 Sites for observed data ----

pred_na_obs <- data_benthic %>% 
  select(-category, -measurementValue) %>% 
  distinct() %>% 
  group_by(year) %>% 
  summarise(across("datasetID":"reef_type", ~sum(is.na(.x)))) %>% 
  ungroup() %>% 
  pivot_longer(2:ncol(.), names_to = "predictor", values_to = "na") %>% 
  mutate(perc = (na*100)/length(unique(data_predictors %>% filter(type == "pred") %>% select(site_id) %>% pull())),
         type = "Predictors (Obs.)")

## 8.3 Make the plot ----

ggplot(data = bind_rows(pred_na_obs, pred_na_pred),
       aes(x = year, y = predictor, fill = perc)) +
  geom_tile(color = "white", height = 0.6, linewidth = 0.6) +
  theme_graph() +
  labs(y = NULL, x = "Year", fill = "Percentage of NA") +
  scale_y_discrete(limits = rev) +
  scale_fill_gradientn(colors = c("#74b9ff", "#f1c40f", "#f39c12", "#e74c3c", "#c0392b"),
                       breaks = c(0, 25, 50, 75, 100),
                       limits = c(0, 100)) +
  scale_x_continuous(expand = c(0, 0), limits = c(1979, 2025)) +
  facet_wrap(~type) +
  theme(legend.title.position = "top",
        legend.title = element_text(hjust = 0.5),
        legend.key.width = unit(1.5, "cm"),
        strip.text.x = element_text(face = "bold"))

ggsave("figs/06_additional/02_data-exploration/na_predictors_year.png", width = 8, height = 12)

# 9. Number of obs and pred sites per subregion ----

data_predictors %>% 
  select(site_id, type, subregion) %>% 
  group_by(subregion, type) %>% 
  summarise(n = n_distinct(site_id)) %>% 
  pivot_wider(names_from = type, values_from = n, names_prefix = "n_sites_") %>% 
  bind_rows(., data_predictors %>% 
              select(site_id, type) %>% 
              group_by(type) %>% 
              summarise(n = n_distinct(site_id)) %>% 
              ungroup() %>% 
              pivot_wider(names_from = type, values_from = n, names_prefix = "n_sites_") %>% 
              mutate(subregion = "World")) %>% 
  mutate(across(c("n_sites_obs", "n_sites_pred"), ~replace_na(.x, 0))) %>% 
  openxlsx::write.xlsx(., file = "figs/06_additional/02_data-exploration/nb-sites-obs-pred_area.xlsx")
