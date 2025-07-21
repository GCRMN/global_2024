# 1. Load packages ----

library(tidyverse)
library(ggtext)

# 2. Source functions ----

source("code/function/graphical_par.R")
source("code/function/theme_graph.R")
source("code/function/theme_map.R")

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
  theme_graph()

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
         plot = plot_i, height = 3.5, width = 9, dpi = fig_resolution)
  
}

map(setdiff(unique(data_ssta_mean_year$region), "All"), ~plot_ssta(region_i = .))



