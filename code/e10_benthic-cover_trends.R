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
source("code/function/extract_mannkendall.R")

# 3. Data preparation ----

## 3.1 Combine model results ----

model_results <- combine_model_data(model = "xgb")

## 3.2 Transform negative predictions to zero ----

model_results$result_trends <- model_results$result_trends %>% 
  mutate(mean = ifelse(mean < 0, 0, mean))

## 3.3 Confidence intervals ----

raw_trends <- model_results$result_trends %>% 
  # Calculate mean and confidence interval
  rename(cover = mean) %>% 
  group_by(category, region, subregion, ecoregion,
           country, territory, year, color, text_title) %>% 
  summarise(mean = mean(cover),
            lower_ci_95 = quantile(cover, 0.05),
            lower_ci_80 = quantile(cover, 0.20),
            upper_ci_95 = quantile(cover, 0.95),
            upper_ci_80 = quantile(cover, 0.80)) %>% 
  ungroup()

## 3.4 Long-term average ----

long_term_average <- raw_trends %>% 
  group_by(category, region, subregion, ecoregion,
           country, territory) %>% 
  summarise(across(c(mean, lower_ci_95, upper_ci_95), ~mean(.x, na.rm = TRUE))) %>% 
  ungroup()

## 3.5 Long-term trend ----

long_term_trend <- raw_trends %>% 
  group_by(category, region, subregion, ecoregion,
           country, territory) %>% 
  group_modify(~extract_mannkendall(data = .x, var_y = "mean")) %>% 
  ungroup()

## 3.6 Add "obs_data" variable ----

load("data/11_model-data/data_benthic_prepared.RData")

data_benthic_obs <- data_benthic %>% 
  select(year, region, subregion, ecoregion,
         country, territory, category) %>% 
  distinct() %>% 
  mutate(region = "Caribbean",
         data_obs = 1)

data_benthic_obs <- data_benthic %>% 
  select(year, category) %>% 
  distinct() %>% 
  mutate(region = "All",
         data_obs = 1) %>% 
  bind_rows(., data_benthic_obs)

raw_trends <- left_join(raw_trends, data_benthic_obs) %>% 
  mutate(data_obs = replace_na(data_obs, 0))

## 3.7 Combine into a list ----

data_trends <- lst(raw_trends, long_term_average, long_term_trend)

rm(raw_trends, long_term_average,
   long_term_trend, data_benthic_obs)

# 4. Trends ----

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
  filter(is.na(subregion) & is.na(territory)) %>% 
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

plot_raw_region <- function(region_i){
  
  plot_i <- data_benthic %>% 
    filter(region == region_i) %>% 
    filter(category %in% c("Hard coral", "Algae", "Coralline algae", "Macroalgae", "Turf algae")) %>% 
    mutate(category = factor(category, levels = c("Hard coral", "Algae", "Coralline algae",
                                                  "Macroalgae", "Turf algae"))) %>% 
    add_colors() %>% 
    ggplot(data = ., aes(x = year, y = mean, ymin = ymin,
                         ymax = ymax, fill = color, color = color)) +
    geom_linerange() +
    geom_point() +
    facet_wrap(~text_title, scales = "free") +
    scale_color_identity() +
    scale_fill_identity() +
    theme_graph() +
    theme(legend.title.position = "top",
          strip.text = element_markdown(hjust = 0, size = 14),
          legend.title = element_text(face = "bold", hjust = 0.5)) +
    scale_x_continuous(breaks = c(1980, 1990, 2000, 2010, 2020), limits = c(1980, 2025)) +
    scale_y_continuous(limits = c(0, NA)) +
    labs(x = "Year", y = "Benthic cover (%)")
  
  ggsave(filename = paste0("figs/06_additional/02_data-exploration/yearly-raw-average_",
                           str_replace_all(str_replace_all(str_to_lower(region_i), " ", "-"), "---", "-"), ".png"),
         plot = plot_i, height = 7, width = 12, dpi = fig_resolution)
  
}

map(unique(data_benthic$region), ~plot_raw_region(region_i = .))

## 4.3 Trends per region ----

plot_trends_region <- function(region_i){
  
  plot_i <- data_trends$raw_trends %>% 
    filter(is.na(subregion) & is.na(territory)) %>% 
    filter(region == region_i) %>% 
    select(category, region, year, mean, lower_ci_95, upper_ci_95) %>% 
    filter(category %in% c("Hard coral", "Algae", "Coralline algae", "Macroalgae", "Turf algae")) %>% 
    mutate(category = factor(category, levels = c("Hard coral", "Algae", "Coralline algae",
                                                  "Macroalgae", "Turf algae"))) %>% 
    add_colors() %>% 
    ggplot(data = ., aes(x = year, y = mean, ymin = lower_ci_95,
                         ymax = upper_ci_95, fill = color, color = color)) +
    geom_ribbon(alpha = 0.35, color = NA) +
    geom_line() +
    facet_wrap(~text_title, scales = "free") +
    scale_color_identity() +
    scale_fill_identity() +
    theme_graph() +
    theme(legend.title.position = "top",
          strip.text = element_markdown(hjust = 0, size = 14),
          legend.title = element_text(face = "bold", hjust = 0.5)) +
    scale_x_continuous(breaks = c(1980, 1990, 2000, 2010, 2020), limits = c(1980, 2025)) +
    scale_y_continuous(limits = c(0, NA)) +
    labs(x = "Year", y = "Benthic cover (%)")

  ggsave(filename = paste0("figs/02_part-2/fig-5/",
                           str_replace_all(str_replace_all(str_to_lower(region_i), " ", "-"), "---", "-"), ".png"),
         plot = plot_i, height = 7, width = 12, dpi = fig_resolution)
  
}

map(unique(data_benthic$region), ~plot_trends_region(region_i = .))

## 4.4 Trends per subregion ----

plot_trends_subregion <- function(region_i){
  
  plot_i <- data_trends$raw_trends %>% 
    filter(!(is.na(region)) & !(is.na(subregion)) & is.na(territory) & is.na(ecoregion)) %>% 
    filter(region == region_i) %>% 
    select(category, region, subregion, year, mean, lower_ci_95, upper_ci_95) %>% 
    filter(category %in% c("Hard coral", "Coralline algae", "Macroalgae", "Turf algae")) %>% 
    mutate(category = factor(category, levels = c("Hard coral", "Coralline algae", "Macroalgae", "Turf algae"))) %>% 
    ggplot(data = ., aes(x = year, y = mean, ymin = lower_ci_95,
                         ymax = upper_ci_95)) +
    geom_ribbon(alpha = 0.35, color = NA) +
    geom_line() +
    facet_grid(subregion~category) +
    theme_graph() +
    theme(strip.text = element_text(face = "bold"),
          legend.title.position = "top",
          legend.title = element_text(face = "bold", hjust = 0.5)) +
    scale_x_continuous(breaks = c(1980, 2000, 2020)) +
    scale_y_continuous(limits = c(0, NA)) +
    labs(x = "Year", y = "Benthic cover (%)")
  
  ggsave(filename = paste0("figs/02_part-2/fig-6/",
                           str_replace_all(str_replace_all(str_to_lower(region_i), " ", "-"), "---", "-"), ".png"),
         plot = plot_i, height = 10, width = 8, dpi = fig_resolution)
  
}

map(unique(data_benthic$region), ~plot_trends_subregion(region_i = .))
