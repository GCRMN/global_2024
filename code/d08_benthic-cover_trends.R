# 1. Load packages ----

library(tidyverse) # Core tidyverse packages
library(patchwork)
library(glue)
library(ggtext)
library(ggrepel)
library(scales)
library(zoo)
library(sf)

# 2. Source functions ----

source("code/function/graphical_par.R")
source("code/function/theme_graph.R")
source("code/function/plot_trends.R")

# 3. Load data ----

load("data/model-results.RData")

# 4. Figures for Part 1 ----

## 4.1 Global - Hard coral and macroalgae ----

plot_trends(level_i = "global", range = "obs")

## 4.2 Global map (for the two regional figures) ----

data_country <- st_read("data/01_maps/01_raw/03_natural-earth/ne_10m_admin_0_countries/ne_10m_admin_0_countries.shp") %>% 
  st_transform(crs = "+proj=eqearth")

data_graticules <- st_read("data/01_maps/01_raw/03_natural-earth/ne_10m_graticules_20/ne_10m_graticules_20.shp") %>% 
  st_transform(crs = "+proj=eqearth")

data_gcrmn_regions <- st_read("data/01_maps/02_clean/03_regions/gcrmn_regions.shp") %>% 
  left_join(., color_regions) %>% 
  st_transform(crs = "+proj=eqearth")

lats <- c(90:-90, -90:90, 90)
longs <- c(rep(c(180, -180), each = 181), 180)

background_map_border <- list(cbind(longs, lats)) %>%
  st_polygon() %>%
  st_sfc(crs = 4326) %>% 
  st_sf() %>%
  st_transform(crs = "+proj=eqearth")

plot <- ggplot() +
  geom_sf(data = background_map_border, fill = "white", color = "grey30", linewidth = 0.25) +
  geom_sf(data = data_graticules, color = "#ecf0f1", linewidth = 0.25) +
  geom_sf(data = background_map_border, fill = NA, color = "grey30", linewidth = 0.25) +
  geom_sf(data = data_gcrmn_regions, aes(fill = color), show.legend = FALSE) +
  scale_fill_identity() +
  geom_sf(data = data_country, color = "#24252a", fill = "#dadfe1") +
  theme(text = element_text(family = "Open Sans"),
        legend.position = "bottom",
        legend.background = element_rect(fill = "transparent", color = NA),
        legend.title = element_blank(),
        panel.background = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        plot.background = element_rect(fill = "transparent", color = NA)) +
  guides(fill = guide_legend(override.aes = list(size = 5, color = NA)))

ggsave(filename = "figs/01_part-1/global_map.png", plot = plot,
       bg = "transparent", height = 5, width = 8, dpi = 300)

rm(background_map_border, data_country, data_gcrmn_regions, data_graticules, lats, longs)

## 4.3 Regional - Hard coral and macroalgae ----

export_subplots <- function(region_i, category_i){
  
  data_i <- data_models %>% 
    group_by(category, level, region, subregion, ecoregion) %>% 
    filter(year >= first_year & year <= last_year) %>% 
    ungroup() %>% 
    filter(category == category_i & level == "region" & model == "HBM") %>% 
    left_join(., color_regions)

  plot_i <- ggplot(data = data_i %>% filter(region == region_i)) +
    geom_ribbon(aes(x = year, ymin = lower_ci_95, ymax = upper_ci_95, fill = color), alpha = 0.35) +
    geom_ribbon(aes(x = year, ymin = lower_ci_80, ymax = upper_ci_80, fill = color), alpha = 0.45) +
    geom_line(aes(x = year, y = mean, color = color)) +
    scale_color_identity() +
    scale_fill_identity() +
    scale_x_continuous(breaks = c(1980, 1985, 1990, 1995, 2000, 2005, 2010, 2015, 2020),
                       limits = c(1979, 2026),
                       labels = c("1980", "", "1990", "", "2000", "", "2010", "", "2020")) +
    lims(y = c(0, max(data_i$upper_ci_95))) +
    labs(x = "Year", y = "Cover (%)", title = case_when(region_i == "EAS" ~ "East Asian Seas",
                                                        region_i == "ETP" ~ "Eastern Tropical Pacific",
                                                        region_i == "WIO" ~ "Western Indian Ocean",
                                                        TRUE ~ region_i)) +
    theme_graph() +
    theme(plot.title = element_text(size = 27, color = "white", face = "bold"),
          axis.title = element_text(size = 18),
          axis.text = element_text(size = 16),
          plot.background = element_rect(fill = "transparent", color = NA))
  
  if(category_i == "Hard coral"){
    
    ggsave(filename = paste0("figs/01_part-1/fig-7_", str_to_lower(region_i), ".png"), plot = plot_i,
           bg = "transparent", height = 5, width = 6, dpi = 300)
    
  }else{
    
    ggsave(filename = paste0("figs/01_part-1/fig-8_", str_to_lower(region_i), ".png"), plot = plot_i,
           bg = "transparent", height = 5, width = 6, dpi = 300)
    
  }
  
}

map(setdiff(unique(data_models$region), NA),
    ~export_subplots(region_i = .x,
                     category_i = "Hard coral"))

map(setdiff(unique(data_models$region), NA),
    ~export_subplots(region_i = .x,
                     category_i = "Macroalgae"))

# 5. Figures for Part 2 ----

## 5.1 Trends (regions) ----

map(setdiff(unique(data_models$region), NA),
    ~plot_trends(region_i = .x,
                 level_i = "region", range = "obs"))

## 5.2 Trends (subregions) ----

map(setdiff(unique(data_models$region), NA),
    ~plot_trends(region_i = .x,
                 level_i = "subregion", category_i = "Hard coral", range = "obs"))

map(setdiff(unique(data_models$region), NA),
    ~plot_trends(region_i = .x,
                 level_i = "subregion", category_i = "Macroalgae", range = "obs"))

## 5.3 Trends for ecoregions ----

map(setdiff(unique(data_models$region), NA),
    ~plot_trends(region_i = .x,
                 level_i = "ecoregion", category_i = "Hard coral", range = "obs"))

map(setdiff(unique(data_models$region), NA),
    ~plot_trends(region_i = .x,
                 level_i = "ecoregion", category_i = "Macroalgae", range = "obs"))

## 5.4 Values per region for writing ----



# 6. Comparison previous trends ----

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

# 7. Average raw values ----

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
