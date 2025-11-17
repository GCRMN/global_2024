# 1. Load packages ----

library(tidyverse) # Core tidyverse packages
library(patchwork)
library(glue)
library(ggtext)
library(ggrepel)
library(scales)
library(zoo)
library(Kendall)
library(sf)
sf_use_s2(FALSE)
library(cowplot) # For the function draw_image()

# 2. Source functions ----

source("code/function/graphical_par.R")
source("code/function/theme_graph.R")
source("code/function/add_colors.R")
source("code/function/combine_model_data.R")
source("code/function/plot_trends.R")
source("code/function/extract_mannkendall.R")

# 3. Data preparation ----

## 3.1 Combine model results ----

model_results <- combine_model_data(save_results = FALSE)

model_results$result_trends <- model_results$result_trends %>% 
  # Filter years
  filter(year >= 1980 & year <= 2024) %>% 
  # Filter spatial levels
  filter(level %in% c("global", "region", "subregion", "ecoregion")) %>% 
  # Filter categories
  filter(category %in% c("Hard coral", "Macroalgae", "Turf algae", "Coralline algae"))
  
## 3.2 Confidence intervals ----

raw_trends <- model_results$result_trends %>% 
  # Calculate mean and confidence interval
  rename(cover = mean) %>% 
  group_by(category, level, region, subregion, ecoregion, year, color, text_title) %>% 
  summarise(mean = mean(cover),
            lower_ci_95 = quantile(cover, 0.025),
            upper_ci_95 = quantile(cover, 0.975)) %>% 
  ungroup() %>% 
  # Replace negative values by 0
  mutate(across(c(mean, lower_ci_95, upper_ci_95), ~ifelse(.x < 0, 0, .x)))

## 3.4 Long-term average ----

long_term_average <- raw_trends %>% 
  group_by(category, level, region, subregion, ecoregion) %>% 
  summarise(across(c(mean, lower_ci_95, upper_ci_95), ~mean(.x, na.rm = TRUE))) %>% 
  ungroup()

## 3.5 Long-term trend ----

long_term_trend <- raw_trends %>% 
  group_by(category, level, region, subregion, ecoregion) %>% 
  group_modify(~extract_mannkendall(data = .x, var_y = "mean")) %>% 
  ungroup()

## 3.6 Combine into a list ----

data_trends <- lst(raw_trends, long_term_average, long_term_trend)

rm(raw_trends, long_term_average, long_term_trend)

# 4. Trends ----

## 4.1 Global trends ----

plot_trends(level_i = "region")

## 4.2 Region trends ----

map(setdiff(unique(data_trends$raw_trends$region), NA),
    ~plot_trends(region_i = .x, level_i = "region"))

## 4.3 Subregion trends ----

map(setdiff(unique(data_trends$raw_trends$region), NA),
    ~plot_trends(region_i = .x, level_i = "subregion"))



































































################## TO SORT #######################

# Export model results for Murray

data_xgboost <- data_trends$raw_trends %>% 
  select(-color, -text_title)

save(data_xgboost, file = "data/02_misc/data_xgboost.RData")









# Other

data_trends$raw_trends %>% 
  filter(level == "global") %>% 
  ggplot(data = ., aes(x = year, y = mean, ymin = lower_ci_95,
                       ymax = upper_ci_95)) +
  geom_ribbon(alpha = 0.35, color = NA) +
  scale_fill_manual(values = c("#d63031", "#0984e3")) +
  geom_line() +
  facet_grid(~category) +
  theme_graph() +
  theme(strip.text = element_text(face = "bold"),
        legend.title.position = "top",
        legend.title = element_text(face = "bold", hjust = 0.5)) +
  scale_x_continuous(breaks = c(1980, 2000, 2020)) +
  labs(x = "Year", y = "Benthic cover (%)")


data_trends$raw_trends %>% 
  filter(level == "region") %>% 
  ggplot(data = ., aes(x = year, y = mean, ymin = lower_ci_95,
                       ymax = upper_ci_95)) +
  geom_ribbon(alpha = 0.35, color = NA) +
  scale_fill_manual(values = c("#d63031", "#0984e3")) +
  geom_line() +
  facet_grid(category~region) +
  theme_graph() +
  theme(strip.text = element_text(face = "bold"),
        legend.title.position = "top",
        legend.title = element_text(face = "bold", hjust = 0.5)) +
  scale_x_continuous(breaks = c(1980, 2000, 2020)) +
  labs(x = "Year", y = "Benthic cover (%)")





















## 4.1 Comparison with 2020 data ----

data_2020 <- read.csv("../../2025-08-25_time-series/time_series/data/01_raw-data/ModelledTrends.all.sum.csv") %>% 
  rename(category = Var, region = GCRMN_region, year = Year,
         mean = value, lower_ci_95 = ".lower_0.95", upper_ci_95 = ".upper_0.95") %>% 
  select(category, region, year, mean, lower_ci_95, upper_ci_95) %>% 
  mutate(data = "2020 report",
         region = str_replace_all(region, c("Global" = "World",
                                            "East Asia" = "EAS")),
         category = str_replace_all(category, c("Hard Coral Cover" = "Hard coral",
                                                "Algae Cover" = "Algae")))

data_2025 <- data_trends$raw_trends %>% 
  filter(level == "region") %>% 
  select(category, region, year, mean, lower_ci_95, upper_ci_95) %>% 
  filter(category %in% c("Hard coral", "Algae")) %>% 
  mutate(data = "2025 report",
         region = str_replace_all(region, "All", "World")) %>% 
  drop_na(region)

data_all <- bind_rows(data_2020, data_2025)

plot <- ggplot(data = data_all, aes(x = year, y = mean, ymin = lower_ci_95,
                                    ymax = upper_ci_95, fill = data, color = data)) +
  geom_ribbon(alpha = 0.35, color = NA) +
  scale_fill_manual(values = c("#d63031", "#0984e3")) +
  geom_line() +
  facet_grid(category~region) +
  theme_graph() +
  theme(strip.text = element_text(face = "bold"),
        legend.title.position = "top",
        legend.title = element_text(face = "bold", hjust = 0.5)) +
  scale_x_continuous(breaks = c(1980, 2000, 2020)) +
  labs(x = "Year", y = "Benthic cover (%)")

ggsave("figs/06_additional/04_benthic-trends/comparison-trends_2020-2025_full.png", width = 18, height = 8)

## 4.2 Yearly raw average ----

load("data/11_model-data/data_benthic_prepared.RData")

data_benthic <- data_benthic %>% 
  bind_rows(., data_benthic %>% 
              mutate(region = "All")) %>%
  group_by(year, region, category) %>% 
  summarise(mean = mean(measurementValue),
            sd = sd(measurementValue)) %>% 
  ungroup() %>% 
  mutate(ymin = mean - sd,
         ymin = ifelse(ymin < 0, 0, ymin),
         ymax = mean + sd) %>% 
  bind_rows(., data_benthic %>% 
              group_by(year, category) %>% 
              summarise(mean = mean(measurementValue),
                        sd = sd(measurementValue)) %>% 
              ungroup() %>% 
              mutate(ymin = mean - sd,
                     ymin = ifelse(ymin < 0, 0, ymin),
                     ymax = mean + sd,
                     region = "Caribbean")) %>% 
  complete(year, category, nesting(region), fill = list(mean = NA, sd = NA)) %>% 
  left_join(., data_benthic %>% 
              select(category) %>% 
              distinct()) %>% 
  drop_na(region)
