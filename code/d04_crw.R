# 1. Load packages ----

library(tidyverse)
library(ggtext)

# 2. Source functions ----

source("code/function/graphical_par.R")
source("code/function/theme_graph.R")
source("code/function/theme_map.R")
source("code/function/extract_coeff.R")

# 3. Yearly SST anomaly ----

load("data/02_misc/data_ssta_mean_year.RData")

data_ssta_mean_year <- data_ssta_mean_year %>% 
  mutate(color = case_when(ssta_mean < 0 ~ "#2c82c9",
                           ssta_mean > 0 ~ "#d64541"))

## 3.1 Global ----

plot_i <- ggplot(data = data_ssta_mean_year %>% filter(region == "All")) +
  geom_bar(aes(x = year, y = ssta_mean, fill = color), stat = "identity") +
  scale_fill_identity() +
  geom_hline(yintercept = 0) +
  theme(plot.title = element_markdown(size = 17, face = "bold", family = "Open Sans Semibold"),
        plot.subtitle = element_markdown(size = 12)) +
  labs(x = "Year", y = "SST anomaly (°C)") +
  theme_graph() +
  lims(y = c(-1.5, 1.5))

ggsave("figs/01_part-1/fig-10.png", height = 5.3, width = 7.2, dpi = fig_resolution)

## 3.2 Regional ----

plot_ssta <- function(region_i){
  
  plot_i <- ggplot(data = data_ssta_mean_year %>% filter(region == region_i & subregion == "All")) +
    geom_bar(aes(x = year, y = ssta_mean, fill = color), stat = "identity") +
    scale_fill_identity() +
    geom_hline(yintercept = 0) +
    theme(plot.title = element_markdown(size = 17, face = "bold", family = "Open Sans Semibold"),
          plot.subtitle = element_markdown(size = 12)) +
    labs(x = "Year", y = "SST anomaly (°C)") +
    theme_graph()

  ggsave(filename = paste0("figs/02_part-2/fig-3/",
                           str_replace_all(str_replace_all(str_to_lower(region_i), " ", "-"), "---", "-"), ".png"),
         plot = plot_i, height = 5, width = 9, dpi = fig_resolution)
  
}

map(setdiff(unique(data_ssta_mean_year$region), "All"), ~plot_ssta(region_i = .))

# 4. DHW percent ----

load("data/02_misc/data_dhw_freq.RData")

data_dhw_freq <- data_dhw_freq %>% 
  complete(year, dhw, nesting(region, subregion), fill = list(nb_cells = 0)) %>% 
  # See https://coralreefwatch.noaa.gov/product/5km/index_5km_baa-max-7d.php
  mutate(heatstress = case_when(dhw <= 0 ~ "No Stress",
                                #dhw > 0 & dhw < 1 ~ "Bleaching Watch",
                                dhw >= 1 & dhw < 4 ~ "Warning",
                                dhw >= 4 & dhw < 8 ~ "Alert 1",
                                dhw >= 8 & dhw < 12 ~ "Alert 2",
                                dhw >= 12 & dhw < 16 ~ "Alert 3",
                                dhw >= 16 & dhw < 20 ~ "Alert 4",
                                dhw >= 20 ~ "Alert 5")) %>% 
  group_by(year, region, subregion, heatstress) %>% 
  summarise(nb_cells = sum(nb_cells)) %>% 
  ungroup() %>% 
  group_by(year, region, subregion) %>% 
  mutate(total_nb_cells = sum(nb_cells)) %>% 
  ungroup() %>% 
  mutate(freq = (nb_cells*100)/total_nb_cells,
         heatstress = as.factor(heatstress),
         heatstress = factor(heatstress, levels = c("No Stress", "Warning",
                                                    "Alert 1", "Alert 2", "Alert 3", "Alert 4", "Alert 5")))

## 4.1 Global ----

plot_i <- ggplot(data = data_dhw_freq %>% filter(region == "All")) +
  geom_bar(aes(x = year, y = freq, fill = heatstress), stat = "identity") +
  scale_fill_manual(values = c("No Stress" = "lightgrey",
                               "Warning" = palette_second[1],
                               "Alert 1" = palette_second[2],
                               "Alert 2" = palette_second[3],
                               "Alert 3" = palette_second[4],
                               "Alert 4" = palette_second[5],
                               "Alert 5" = "black")) +
  theme_graph() +
  labs(x = "Year", y = "Percentage of coral reefs")

ggsave("figs/01_part-1/fig-11.png", height = 5.3, width = 7.2, dpi = fig_resolution)

## 4.2 Regional ----

plot_dhw <- function(region_i){
  
  # Create and export the plot 
  
  plot_i <- ggplot(data = data_dhw_freq %>% filter(region == region_i & subregion == "All")) +
    geom_bar(aes(x = year, y = freq, fill = heatstress), stat = "identity") +
    scale_fill_manual(values = c("No Stress" = "lightgrey",
                                 "Warning" = palette_second[1],
                                 "Alert 1" = palette_second[2],
                                 "Alert 2" = palette_second[3],
                                 "Alert 3" = palette_second[4],
                                 "Alert 4" = palette_second[5],
                                 "Alert 5" = "black")) +
    theme_graph() +
    labs(x = "Year", y = "Percentage of coral reefs")
  
  ggsave(filename = paste0("figs/02_part-2/fig-4/",
                           str_replace_all(str_replace_all(str_to_lower(region_i), " ", "-"), "---", "-"), ".png"),
         plot = plot_i, height = 5.3, width = 7.2, dpi = fig_resolution)
  
  # Export the data
  
  data_dhw_freq %>% 
    filter(region == region_i) %>%
    select(-nb_cells, -total_nb_cells, -region) %>%
    mutate(freq = round(freq, 2)) %>%
    pivot_wider(names_from = "heatstress", values_from = "freq") %>% 
    select("subregion", "year", "No Stress", "Warning",
           "Alert 1", "Alert 2", "Alert 3", "Alert 4", "Alert 5") %>% 
    arrange(subregion, year) %>% 
    openxlsx::write.xlsx(., paste0("figs/06_additional/06_threats/data-heatstress_",
                                   str_replace_all(str_replace_all(str_to_lower(region_i), " ", "-"),
                                                   "---", "-"), ".xlsx"))
  
}

map(setdiff(unique(data_dhw_freq$region), "All"), ~plot_dhw(region_i = .))

# 5. Daily SST ----

load("data/02_misc/data_sst.RData")

plot_i <- data_sst %>% 
  mutate(date = as_date(date)) %>% 
  filter(subregion == "All" & region != "All") %>% 
  ggplot(data = ., aes(x = date, y = sst)) +
  geom_line() +
  labs(x = "Year", y = "Sea Surface Temperature (°C)") +
  coord_cartesian(clip = "off") +
  theme_graph() +
  facet_wrap(~region, ncol = 2, scales = "free")

ggsave("figs/05_supp-mat/sst.png", width = 7, height = 10, dpi = fig_resolution)

# 6. Long-term SST average and trend ----

load("data/02_misc/data_sst.RData")

## 6.1 Long-term SST average ----

data_sst <- data_sst %>% 
  group_by(region, subregion) %>% 
  mutate(mean_sst = mean(sst, na.rm = TRUE)) %>% 
  ungroup()

## 6.2 Long-term SST trend ----

data_sst <- data_sst %>% 
  # Convert date as numeric
  mutate(date = as.numeric(as_date(date))) %>% 
  # Extract linear model coefficients
  group_by(region, subregion) %>% 
  group_modify(~extract_coeff(data = .x, var_y = "sst", var_x = "date")) %>% 
  ungroup() %>% 
  # Calculate increase in SST over the period
  mutate(min_date = as.numeric(as_date(min(data_sst$date))),
         max_date = as.numeric(as_date(max(data_sst$date)))) %>% 
  mutate(sst_increase = ((max_date)*slope+intercept) - ((min_date)*slope+intercept)) %>% 
  select(-min_date, -max_date) %>% 
  # Calculate the warming rate (°C per year)
  mutate(warming_rate = sst_increase/(year(max(data_sst$date))-year(min(data_sst$date)))) %>% 
  # Add mean_sst for each subregion
  left_join(., data_sst %>% 
              select(region, subregion, mean_sst) %>% 
              distinct()) %>% 
  select(-intercept, -slope)

## 6.3. Export the full table ----

data_sst %>% 
  mutate(across(c(sst_increase, mean_sst), ~format(round(.x, 2))),
         warming_rate = format(round(warming_rate, 3))) %>% 
  openxlsx::write.xlsx(., file = "figs/05_supp-mat/supp-tbl-3_reef-extent_sst-mean-trend.xlsx")

## 6.4. Export the table per region ----

### 6.4.1 Create the function ----

export_descriptors <- function(gcrmn_region){
  
  data_sst %>% 
    filter(region == gcrmn_region) %>% 
    select(-region) %>% 
    write.csv(., file = paste0("figs/02_part-2/tbl-4/",
                               str_replace_all(str_to_lower(gcrmn_region), " ", "-"),
                               ".csv"),
              row.names = FALSE)
  
}

### 6.4.2 Map over the function ----

map(setdiff(unique(data_sst$region), "All"), ~export_descriptors(gcrmn_region = .))
