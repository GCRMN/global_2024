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

# 5. Temporal trends ----

## 5.1 Load and transform obs data ----

load("data/02_misc/data-benthic.RData")

data_benthic <- data_benthic %>% 
  add_colors()

data_benthic <- data_benthic %>% 
  bind_rows(., data_benthic %>% 
              mutate(area = "All")) %>% 
  group_by(year, area, category, color, text_title) %>% 
  summarise(mean = mean(measurementValue),
            sd = sd(measurementValue)) %>% 
  ungroup() %>% 
  mutate(ymin = mean - sd,
         ymin = ifelse(ymin < 0, 0, ymin),
         ymax = mean + sd) %>% 
  bind_rows(., data_benthic %>% 
              group_by(year, category, color, text_title) %>% 
              summarise(mean = mean(measurementValue),
                        sd = sd(measurementValue)) %>% 
              ungroup() %>% 
              mutate(ymin = mean - sd,
                     ymin = ifelse(ymin < 0, 0, ymin),
                     ymax = mean + sd,
                     area = "Caribbean")) %>% 
  complete(year, category, nesting(area), fill = list(mean = NA, sd = NA)) %>% 
  select(-color, -text_title) %>% 
  left_join(., data_benthic %>% 
              select(category, text_title, color) %>% 
              distinct()) %>% 
  drop_na(area)

## 5.2 Map over the function -----

### 5.2.1 Major categories ----

# Version with only raw data

data_benthic %>% 
  filter(!(area %in% c("All", "Navassa Island", "Caribbean"))) %>% 
  select(area) %>% 
  distinct() %>% 
  pull() %>% 
  map(.,
    ~plot_trends(area_i = .x,
                 categories = c("Hard coral", "Algae", "Other fauna"),
                 icons = TRUE,
                 scales = "fixed",
                 raw_data = TRUE,
                 modelled_data = FALSE))

# Version with modeled and raw data

map(unique(data_trends$raw_trends$area),
    ~plot_trends(area_i = .x,
                 categories = c("Hard coral", "Algae", "Other fauna"),
                 icons = TRUE,
                 scales = "fixed",
                 raw_data = TRUE,
                 modelled_data = TRUE))

### 5.2.2 Algae categories ----

map(unique(data_trends$raw_trends$area),
    ~plot_trends(area_i = .x,
                 categories = c("Coralline algae", "Macroalgae", "Turf algae"),
                 icons = TRUE,
                 scales = "fixed",
                 raw_data = TRUE,
                 modelled_data = TRUE))

### 5.2.3 Hard coral genera ----

map(unique(data_trends$raw_trends$area),
    ~plot_trends(area_i = .x,
                 categories = c("Acropora", "Orbicella", "Porites"),
                 icons = TRUE,
                 scales = "fixed",
                 raw_data = TRUE,
                 modelled_data = TRUE))

# Raw data (for writing)

if(FALSE){
  
  A <- data_trends$raw_trends %>%
    filter(area == "All" & category == "Hard coral") %>%
    select(-upper_ci_95, -lower_ci_95, -text_title, -color)

}

# 6. Generate text to describe models ----

map(unique(data_trends$raw_trends$category), ~model_text(category_i = .x))

# 7. Figure for the Executive Summary ----

## 7.1 Hard coral ----

data_trends$raw_trends %>% 
  filter(category == "Hard coral" & area == "All") %>% 
  filter(year >= 1984) %>% 
  ggplot(data = .) +
  geom_ribbon(aes(x = year, ymin = lower_ci_95, ymax = upper_ci_95, fill = "#42b9bc"), alpha = 0.35) +
  geom_line(aes(x = year, y = mean, color = "#42b9bc"), linewidth = 1) +
  annotate("segment", x = 1970, xend = 1983, y = 50, color = "#42b9bc", linetype = "dashed") +
  annotate("rect", xmin = 1970, xmax = 1983, ymin = 40, ymax = 60, fill = "#42b9bc", alpha = 0.2) +
  scale_fill_identity() +
  scale_color_identity() +
  scale_x_continuous(expand = c(0, 0), limits = c(1970, NA)) +
  scale_y_continuous(limits = c(0, 60)) +
  labs(x = "Year", y = "Benthic cover (%)")

ggsave("figs/00_misc/exe-summ_1_raw.png", height = 5.3, width = 9.2, dpi = fig_resolution)

## 7.2 Macroalgae ----

data_trends$raw_trends %>% 
  filter(category == "Macroalgae" & area == "All") %>% 
  filter(year >= 1984) %>% 
  ggplot(data = .) +
  geom_ribbon(aes(x = year, ymin = lower_ci_95, ymax = upper_ci_95, fill = "#42b9bc"), alpha = 0.35) +
  geom_line(aes(x = year, y = mean, color = "#42b9bc"), linewidth = 1) +
  annotate("segment", x = 1970, xend = 1983, y = 8, color = "#42b9bc", linetype = "dashed") +
  annotate("rect", xmin = 1970, xmax = 1983, ymin = 6, ymax = 10, fill = "#42b9bc", alpha = 0.2) +
  scale_fill_identity() +
  scale_color_identity() +
  scale_x_continuous(expand = c(0, 0), limits = c(1970, NA)) +
  scale_y_continuous(limits = c(0, 50)) +
  labs(x = "Year", y = "Benthic cover (%)")

ggsave("figs/00_misc/exe-summ_2_raw.png", height = 5.3, width = 9.2, dpi = fig_resolution)

# 8. Additional figures ----

## 8.1 Hard coral vs algae ----

### 8.1.1 Transform data ----

data_hc_algae <- data_trends$raw_trends %>% 
  filter(area == "All" & category %in% c("Hard coral", "Algae")) %>% 
  select(year, category, mean) %>% 
  pivot_wider(names_from = "category", values_from = "mean") %>% 
  mutate(ratio = `Hard coral`/`Algae`)

### 8.1.2 Create labels ----

data_labels <- tibble(type = c(1, 1, 2),
                      x = c(30, 10, 50),
                      y = c(10, 30, 50),
                      text = c("**More <span style='color:#16a085'>algae</span>**<br>than <span style='color:#c44d56'>hard corals</span>",
                               "**More <span style='color:#c44d56'>hard corals</span>**<br>than <span style='color:#16a085'>algae</span>",
                               "As much <span style='color:#c44d56'>hard corals</span> than <span style='color:#16a085'>algae</span>"))

### 8.1.3 Make the plot ----

ggplot(data = data_hc_algae, aes(x = Algae, y = `Hard coral`, label = year)) +
  geom_line() +
  geom_text_repel(data = data_hc_algae %>% filter(year %in% c(1985, 2000, 2010, 2023)),
                  aes(x = Algae, y = `Hard coral`, label = year), force = 40,
                  family = font_choose_graph, seed = 27, min.segment.length = unit(10, "cm")) + 
  geom_point(data = data_hc_algae, aes(x = Algae, y = `Hard coral`, fill = ratio),
             size = 2, shape = 21, show.legend = FALSE, color = "black") +
  geom_point(data = data_hc_algae %>% filter(year %in% c(1985, 2000, 2010, 2023)),
             aes(x = Algae, y = `Hard coral`, fill = ratio), size = 4.5, shape = 21,
             color = "black", show.legend = FALSE) +
  geom_abline(slope = 1, linetype = "dashed") +
  scale_fill_gradient2(high = "#c44d56", low = "#16a085", midpoint = 1) +
  labs(x = "Algae cover (%)", y = "Hard coral cover (%)") +
  lims(x = c(0, 60), y = c(0, 60)) +
  theme(axis.line.y = element_line(linewidth = 0.4),
        axis.ticks.y = element_line(linewidth = 0.4, color = "black")) +
  geom_richtext(data = data_labels %>% filter(type == 1), aes(x = x, y = y, label = text),
                label.color = "transparent", fill = "transparent", size = 3) +
  geom_richtext(data = data_labels %>% filter(type == 2), aes(x = x, y = y, label = text),
                label.color = "transparent", fill = "#efeff0", size = 3, angle = 45)

ggsave("figs/06_additional/01_misc/hard-coral-vs-algae.png", width = 6, height = 6, dpi = fig_resolution)

## 8.2 Stacked benthic cover ----

### 8.2.1 Transform data ----

data_cover <- data_trends$raw_trends %>% 
  filter(area == "All" & category %in% c("Hard coral", "Algae", "Other fauna"))

data_cover <- data_cover %>% 
  group_by(year) %>% 
  summarise(mean = sum(mean)) %>% 
  ungroup() %>% 
  # Generate the others category (difference between 100 and sum of all categories)
  mutate(mean = 100 - mean, 
         category = "Others",
         color = "lightgrey") %>% 
  bind_rows(data_cover, .)

data_labels <- tibble(x = c(1995, 1995, 1995, 1995),
                      y = c(82, 52.5, 36, 14),
                      category = c("Algae", "Hard coral", "Other fauna", "Others"),
                      color = c("white", "white", "white", "black"))

### 8.2.2 Make the plot ----

ggplot(data = data_cover, aes(x = year, y = mean, fill = category)) +
  geom_area(show.legend = FALSE, color = "white", outline.type = "full", linewidth = 0.25) +
  scale_fill_manual(values = unique(data_cover$color)) +
  theme_graph() +
  geom_text(data = data_labels, aes(x = x, y = y, label = category, color = color),
            size = 4.5, family = font_choose_graph) +
  scale_color_identity() +
  labs(x = "Year", y = "Benthic cover (%)")

ggsave("figs/06_additional/01_misc/stacked-benthic-cover.png", width = 7, height = 5, dpi = fig_resolution)

## 8.3 Comparison previous trends ----

load("C:/Users/jerem/Desktop/Recherche/03_projects/2025-08-25_time-series/time_series/data/data_trends_litterature.RData")

data_trends_litterature <- data_trends$raw_trends %>%
  filter(area == "All") %>% 
  rename(lower_ci = lower_ci_95, higher_ci = upper_ci_95) %>% 
  select(category, region, year, mean, higher_ci, lower_ci) %>%
  mutate(source = "GCRMN Caribbean 2025") %>% 
  bind_rows(., data_trends_litterature)

data_trends_litterature %>% 
  filter(region == "Caribbean" & is.na(subregion) & category == "Hard coral") %>% 
  ggplot(data = .) +
  geom_ribbon(aes(x = year, ymin = lower_ci, ymax = higher_ci), fill = "lightgrey") +
  geom_point(aes(x = year, y = mean)) +
  geom_line(aes(x = year, y = mean)) +
  facet_wrap(~source, nrow = 1) +
  lims(y = c(0, NA)) +
  theme_graph() +
  theme(strip.background = element_blank(),
        panel.spacing = unit(3, "lines")) +
  labs(x = "Year", y = "Hard coral cover (%)")

ggsave("figs/06_additional/01_misc/trends_litterature.png", width = 15, height = 5)
