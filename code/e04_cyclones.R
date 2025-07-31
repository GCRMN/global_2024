# 1. Required packages ----

library(tidyverse)
library(ggrepel)
library(glue)
library(sf)
sf_use_s2(FALSE)

# 2. Source functions ----

source("code/function/graphical_par.R")
source("code/function/theme_graph.R")
source("code/function/theme_map.R")

# 3. Maximum wind speed over time ----

## 3.1 Transform data ----

load("data/07_cyclones/02_cyclones_extracted.RData")

data_cyclones <- data_cyclones %>% 
  # Add region from subregion
  mutate(region = gsub('[[:digit:]]+', '', subregion),
         region = str_trim(region)) %>% 
  # Get saffir simpson scale on entire track
  group_by(saffir) %>% 
  mutate(max_saffir = max(saffir)) %>% 
  ungroup() %>% 
  # Filter cyclone (i.e. remove tropical storms)
  filter(max_saffir >= 1) %>% 
  mutate(ts_name = str_to_sentence(ts_name),
         max_saffir = as.factor(max_saffir)) %>%
  # Get the highest wind speed cyclones per region (remove duplicates)
  select(-subregion) %>% 
  group_by(region, ts_id) %>% 
  filter(windspeed == max(windspeed)) %>% 
  ungroup() %>% 
  # Add cyclone position by wind_speed
  arrange(region, desc(windspeed)) %>% 
  group_by(region) %>% 
  mutate(position = row_number())

## 3.2 Create the function ----

cyclone_intensity <- function(region_i){
  
  # 1. Filter
  
  data_cyclones_i <- data_cyclones %>% 
    filter(region == region_i)
  
  # 2. Make the plot 
  
  plot_i <- ggplot(data = data_cyclones_i, aes(x = time, y = windspeed)) +
    geom_point(aes(fill = max_saffir), color = "white", shape = 21, size = 4.5,
               show.legend = c(shape = TRUE)) +
    geom_label_repel(data = data_cyclones_i %>% filter(position %in% 1:15), # Label only the first 15 cyclones
                     aes(label = ts_name, color = max_saffir), fill = "white", alpha = 0.9, size = 3,
                     label.r = unit(0.4, "lines"), show.legend = FALSE, family = font_choose_graph,
                     max.overlaps = getOption("ggrepel.max.overlaps", default = 15)) +
    scale_y_continuous(breaks = c(0, 50 ,100, 150, 200, 250, 300), limits = c(0, 325)) +
    scale_x_date(limits = c(ymd("1980-01-01"), ymd("2025-01-01"))) +
    coord_cartesian(ylim = c(14.25, 310)) +
    scale_fill_manual(breaks = c("1", "2", "3", "4", "5"),
                      labels = c("Cat. 1", "Cat. 2", "Cat. 3", "Cat. 4", "Cat. 5"),
                      values = c(palette_second[2:5], "black"),
                      name = "Saffir-Simpson\ncategory",
                      drop = FALSE) +
    scale_color_manual(breaks = c("1", "2", "3", "4", "5"),
                       labels = c("Cat. 1", "Cat. 2", "Cat. 3", "Cat. 4", "Cat. 5"),
                       values = c(palette_second[2:5], "black"),
                       name = "Saffir-Simpson\ncategory",
                       drop = FALSE) +
    guides(fill = guide_legend(override.aes = list(size = 4))) +
    labs(x = "Year", y = bquote("Wind speed (km."~h^-1*")")) +
    theme_graph() +
    theme(text = element_text(size = 13),
          legend.position = "right",
          legend.direction = "vertical",
          axis.title.x = element_text(size = 14),
          axis.title.y = element_text(size = 14))
  
  # 3. Save the plot
  
  ggsave(filename = paste0("figs/02_part-2/fig-7/",
                           str_replace_all(str_replace_all(str_to_lower(region_i), " ", "-"), "---", "-"), ".png"),
         plot = plot_i, height = 3.5, width = 9, dpi = fig_resolution)
  
}

## 3.3 Map over the function ----

map(unique(data_cyclones$region), ~cyclone_intensity(region_i = .))

# 4. Number of cyclones per year ----

## 4.1 Transform data ----

load("data/07_cyclones/02_cyclones_extracted.RData")

data_cyclones_year <- data_cyclones %>% 
  # Add region from subregion
  mutate(region = gsub('[[:digit:]]+', '', subregion),
         region = str_trim(region)) %>% 
  mutate(year = year(time)) %>% 
  select(region, ts_id, ts_name, year, saffir) %>% 
  distinct() %>% 
  group_by(region, saffir) %>% 
  mutate(max_saffir = max(saffir)) %>% 
  ungroup() %>% 
  filter(max_saffir >= 1) %>% 
  group_by(region, year, max_saffir) %>% 
  count() %>% 
  ungroup() %>% 
  mutate(max_saffir = as.factor(max_saffir))

## 4.2 Create the function ----

cyclone_frequency <- function(region_i){
  
  # 1. Filter
  
  data_cyclones_i <- data_cyclones_year %>% 
    filter(region == region_i)
  
  # 2. Make the plot 
  
  name_cyclone <- case_when(region_i == "Caribbean" ~ "hurricanes",
                            region_i == "EAS" ~ "typhoons",
                            TRUE ~ "cyclones")
  
  plot_i <- ggplot(data = data_cyclones_i, aes(x = year, y = n, fill = max_saffir)) +
    geom_bar(stat = "identity", show.legend = TRUE) +
    theme_graph() +
    labs(x = "Year", y = paste0("Number of ", name_cyclone)) +
    scale_fill_manual(breaks = c("1", "2", "3", "4", "5"),
                      labels = c("Cat. 1", "Cat. 2", "Cat. 3", "Cat. 4", "Cat. 5"),
                      values = c(palette_second[2:5], "black"),
                      name = "Saffir-Simpson category",
                      drop = FALSE) +
    scale_y_continuous(breaks = function(x) unique(floor(pretty(x)))) +
    theme(legend.title.position = "top",
          legend.title = element_text(hjust = 0.5)) +
    lims(x = c(1979, 2026))
  
  # 3. Save the plot
  
  ggsave(filename = paste0("figs/02_part-2/fig-8/",
                           str_replace_all(str_replace_all(str_to_lower(region_i), " ", "-"), "---", "-"), ".png"),
         plot = plot_i, height = 4, width = 8, dpi = fig_resolution)
  
}

## 4.3 Map over the function ----

map(unique(data_cyclones_year$region), ~cyclone_frequency(region_i = .))
