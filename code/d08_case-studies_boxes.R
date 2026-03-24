# 1. Load packages ----

library(tidyverse)
library(sf)
sf_use_s2(FALSE)
library(ggspatial) # For annotation_scale function
library(patchwork)
library(ggtext)

# 2. Source functions ----

source("code/function/graphical_par.R")
source("code/function/data_descriptors.R")
source("code/function/theme_map.R")
source("code/function/theme_graph.R")

# 3. ROPME case study ----

## 3.1 Countries shapefile ----

data_countries <- read_sf("data/01_maps/01_raw/03_natural-earth/ne_10m_admin_0_countries/ne_10m_admin_0_countries.shp")

## 3.2 2020 data ----

data_2020 <- read.csv2("data/02_misc/03-merge_all_all_all_benthos_NA.csv") %>% 
  filter(Area == "ROPME Area") %>% 
  select(Longitude, Latitude, Year, Date) %>%
  distinct() %>% 
  drop_na(Longitude, Latitude) %>% 
  group_by(Longitude, Latitude) %>% 
  summarise(nb_surveys = n()) %>%
  ungroup() %>% 
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326) 

## 3.3 2025 data -----

load("data/02_misc/data-benthic.RData")

data_2025 <- data_benthic %>% 
  filter(region == "ROPME") %>% 
  filter(country != "Yemen") %>% 
  select(decimalLatitude, decimalLongitude, year, eventDate, month) %>%
  distinct() %>% 
  group_by(decimalLatitude, decimalLongitude) %>% 
  summarise(nb_surveys = n()) %>%
  ungroup() %>% 
  st_as_sf(coords = c("decimalLongitude", "decimalLatitude"), crs = 4326)

## 3.4 Create the grid ----

poly <- tibble(lat = c(45, 65),
               long = c(15, 32)) %>% 
  st_as_sf(coords = c("lat", "long"), 
           crs = 4326) %>% 
  st_bbox() %>% 
  st_as_sfc()

data_grid <- st_make_grid(poly, cellsize = 0.4, square = FALSE) %>% 
  st_as_sf() %>% 
  mutate(id_poly = row_number())

## 3.5 Spatial join ---- 

data_2025_grid <- st_join(data_grid, data_2025) %>% 
  group_by(id_poly) %>% 
  summarise(nb_surveys_2025 = sum(nb_surveys)) %>% 
  ungroup()

data_2020_grid <- st_join(data_grid, data_2020) %>% 
  group_by(id_poly) %>% 
  summarise(nb_surveys_2020 = sum(nb_surveys)) %>% 
  ungroup()

data_all <- data_2020_grid %>% 
  st_drop_geometry() %>% 
  bind_cols(., data_2025_grid) %>% 
  select(-"id_poly...3") %>% 
  rename("id_poly" = "id_poly...1") %>% 
  mutate(across(c(nb_surveys_2020, nb_surveys_2025), ~replace_na(.x, 0))) %>% 
  filter(!(nb_surveys_2020 == 0 & nb_surveys_2025 == 0)) %>% 
  pivot_longer(nb_surveys_2020:nb_surveys_2025, names_to = "report", values_to = "nb_surveys") %>% 
  mutate(colors = case_when(nb_surveys == 0 ~ "0",
                            nb_surveys > 0 & nb_surveys <= 5 ~ "1-5",
                            nb_surveys > 5 & nb_surveys <= 10 ~ "6-10",
                            nb_surveys > 10 & nb_surveys <= 20 ~ "11-20",
                            nb_surveys > 20 & nb_surveys <= 45 ~ "21-45",
                            nb_surveys > 45 & nb_surveys <= 70 ~ "46-70",
                            nb_surveys > 70 & nb_surveys <= 120 ~ "71-120"),
         colors = as.factor(colors),
         colors = factor(colors, levels = c("0", "1-5", "6-10", "11-20", "21-45", "46-70", "71-120"))) %>% 
  st_as_sf()

rm(data_2020, data_2020_grid, data_2025, data_2025_grid, data_benthic, data_grid, poly)

## 3.6 Make the plots ----

color_scalebar <- "black"

### 3.6.1 2020 data ----

plot_a <- ggplot() +
  geom_sf(data = data_all %>% filter(report == "nb_surveys_2020"),
          aes(fill = colors), color = "#747d8c", show.legend = FALSE) +
  scale_fill_manual(breaks = c("0", "1-5", "6-10", "11-20", "21-45", "46-70", "71-120"),
                    values = c("#ecf0f1", "#82ccdd", "#3498db", "#fa983a", "#e74c3c", "#c0392b", "#B53471")) +
  geom_sf(data = data_countries, fill = "#dadfe1", color = "black", linewidth = 0.15) +
  theme_map() +
  theme(panel.border = element_rect(fill = NA, color = "black"),
        panel.grid = element_blank(),
        axis.text = element_text(family = font_choose_map, color = "black"),
        axis.text.y = element_text(angle = 90, hjust = 0.5),
        axis.text.y.right = element_text(angle = -90, hjust = 0.5),
        legend.frame = element_rect(fill = "white"),
        legend.key = element_rect(fill = "white"),
        legend.text = element_text(family = font_choose_map, size = 6),
        plot.title = element_markdown(hjust = 0),
        panel.background = element_rect(fill = "transparent", colour = NA),
        plot.background = element_rect(fill = "transparent", colour = NA),
        legend.background = element_rect(fill = "transparent", colour = NA)) + 
  coord_sf(xlim = c(45, 65), ylim = c(15, 32),
           label_axes = list(left = "N", bottom = "E")) +
  annotation_scale(location = "br",
                   width_hint = 0.25, text_family = font_choose_map, text_col = color_scalebar,
                   text_cex = 0.7, style = "bar", line_width = 1, height = unit(0.04, "cm"),
                   line_col = color_scalebar, pad_x = unit(0.5, "cm"), pad_y = unit(0.5, "cm"),
                   bar_cols = c(color_scalebar, color_scalebar)) +
  labs(title = "**A.** <span style='color:#2980b9'>2020</span> GCRMN report")

### 3.6.2 2025 data ----

plot_b <- ggplot() +
  geom_sf(data = data_all %>% filter(report == "nb_surveys_2025"),
          aes(fill = colors), color = "#747d8c", show.legend = TRUE) +
  scale_fill_manual(breaks = c("0", "1-5", "6-10", "11-20", "21-45", "46-70", "71-120"),
                    values = c("#ecf0f1", "#82ccdd", "#3498db", "#fa983a", "#e74c3c", "#c0392b", "#B53471"),
                    name = "Number of\nsurveys") +
  geom_sf(data = data_countries, fill = "#dadfe1", color = "black", linewidth = 0.15) +
  theme_map() +
  theme(panel.border = element_rect(fill = NA, color = "black"),
        panel.grid = element_blank(),
        axis.text = element_text(family = font_choose_map, color = "black"),
        axis.text.y = element_text(angle = 90, hjust = 0.5),
        axis.text.y.right = element_text(angle = -90, hjust = 0.5),
        legend.frame = element_rect(fill = "white"),
        legend.key = element_rect(fill = "white"),
        legend.direction = "vertical",
        legend.text = element_text(family = font_choose_map, size = 6),
        plot.title = element_markdown(hjust = 0),
        panel.background = element_rect(fill = "transparent", colour = NA),
        plot.background = element_rect(fill = "transparent", colour = NA),
        legend.background = element_rect(fill = "transparent", colour = NA)) + 
  coord_sf(xlim = c(45, 65), ylim = c(15, 32),
           label_axes = list(right = "N", bottom = "E")) +
  annotation_scale(location = "br",
                   width_hint = 0.25, text_family = font_choose_map, text_col = color_scalebar,
                   text_cex = 0.7, style = "bar", line_width = 1, height = unit(0.04, "cm"),
                   line_col = color_scalebar, pad_x = unit(0.5, "cm"), pad_y = unit(0.5, "cm"),
                   bar_cols = c(color_scalebar, color_scalebar)) +
  labs(title = "**B.** <span style='color:#2980b9'>2025</span> GCRMN report")

### 3.6.3 Combine the plots ----

plot_full <- plot_a + plot_b + plot_layout(guides = "collect") &
  theme(plot.background = element_rect(fill = "transparent", colour = NA),
        panel.background = element_rect(fill = "transparent", colour = NA))

### 3.6.4 Save the plot ----

ggsave("figs/04_case-studies/case-study_ropme.png", height = 5.5, width = 10.5, bg = "transparent", dpi = 300)

# 4. Comparison 2020 vs 2025 trends ----

## 4.1 Load and transform data ----

load("data/model-results.RData")

data_2025 <- data_models %>% 
  filter(level %in% c("global", "region") & category == "Hard coral") %>% 
  mutate(source = "2025 GCRMN report",
         color = "#c44569") %>% 
  select(source, level, region, category, year, mean, lower_ci_80, upper_ci_80, lower_ci_95, upper_ci_95, color)

data_2020 <- read.csv("../../2025-08-25_time-series/time_series/data/gcrmn_global_2021/ModelledTrends.all.sum.csv") %>% 
  rename(category = "Var", region = "GCRMN_region", year = Year, mean = value,
         lower_ci_80 = ".lower_0.8", upper_ci_80 = ".upper_0.8",
         lower_ci_95 = ".lower_0.95", upper_ci_95 = ".upper_0.95") %>% 
  mutate(category = str_replace_all(category, "Hard Coral Cover", "Hard coral"),
         level = case_when(region == "Global" ~ "global",
                           TRUE ~ "region"),
         region = str_replace_all(region, c("East Asia" = "EAS",
                                            "Global" = NA_character_)),
         source = "2020 GCRMN report",
         color = "#2d98da") %>% 
  filter(category == "Hard coral") %>% 
  select(source, level, region, category, year, mean, lower_ci_80, upper_ci_80, lower_ci_95, upper_ci_95, color)

data_trends <- bind_rows(data_2020, data_2025)

## 4.2 Make the plot ----

ggplot(data = data_trends %>% filter(level == "global"), aes(x = year, fill = color, color = color, group  = source)) +
  geom_ribbon(aes(ymin = lower_ci_95, ymax = upper_ci_95), alpha = 0.35, color = NA) +
  geom_ribbon(aes(ymin = lower_ci_80, ymax = upper_ci_80), alpha = 0.45, color = NA) +
  geom_line(aes(y = mean)) +
  scale_fill_identity() +
  scale_color_identity() +
  theme_graph() +
  theme(legend.title.position = "top",
        legend.title = element_text(face = "bold", hjust = 0.5),
        panel.background = element_rect(fill = "transparent", colour = NA),
        plot.background = element_rect(fill = "transparent", colour = NA)) +
  labs(x = "Year", y = "Hard coral cover (%)")

ggsave("figs/04_case-studies/case-study_1_raw.png", width = 7, height = 5)

ggplot(data = data_trends %>% filter(level == "region"), aes(x = year, fill = color, color = color, group  = source)) +
  geom_ribbon(aes(ymin = lower_ci_95, ymax = upper_ci_95), alpha = 0.35, color = NA) +
  geom_ribbon(aes(ymin = lower_ci_80, ymax = upper_ci_80), alpha = 0.45, color = NA) +
  geom_line(aes(y = mean)) +
  scale_fill_identity() +
  scale_color_identity() +
  facet_wrap(~region, ncol = 2) +
  theme_graph() +
  theme(legend.title.position = "top",
        legend.title = element_text(face = "bold", hjust = 0.5),
        panel.background = element_rect(fill = "transparent", colour = NA),
        plot.background = element_rect(fill = "transparent", colour = NA)) +
  labs(x = "Year", y = "Benthic cover (%)")

ggsave("figs/07_additional/comparison_2020-2025_region.png", width = 7, height = 14)

# 5. Box X ----

data_box <- tibble(year = seq(2000, 2010, 1),
                   cover = c(38, 39, 35, 37, 38, 39, 42, 38, 37,
                             41, 19),
                   position = seq(1,11,1)) %>% 
  mutate(color = case_when(position == max(position) ~ "#2d98da",
                           position == max(position-1) ~ "#c44569",
                           TRUE ~ "#bdc3c7"))

plot_a <- ggplot(data = data_box, aes(x = year, y = cover, fill = color, group = 1)) +
  geom_line() +
  geom_point(size = 3.5, shape = 21, color = "white") +
  scale_fill_identity() +
  scale_y_continuous(limits = c(0, 50)) +
  scale_x_continuous(limits = c(2000, 2020)) +
  theme_graph() +
  theme(panel.background = element_rect(fill = "transparent", colour = NA),
        plot.background = element_rect(fill = "transparent", colour = NA),
        plot.title = element_markdown()) +
  labs(x = "Year", y = "Hard coral cover (%)")

(plot_a + labs(title = "**A.** Absolute change")) + 
  (plot_a + labs(title = "**B.** Relative change")) &
  theme(plot.background = element_rect(fill = "transparent", colour = NA),
        panel.background = element_rect(fill = "transparent", colour = NA))

ggsave("figs/04_case-studies/case-study_X_raw.png", width = 8, height = 4, dpi = 300)
ggsave("figs/04_case-studies/case-study_X_raw.pdf", width = 8, height = 4)

# 6. Turf algae case study ----

## 6.1 Figure B ----

data_turf_length <- read.csv("data/14_case-studies/b_TurfLength.csv")

plot_b <- ggplot(data = data_turf_length, aes(x = TurfLength.mm.)) +
  geom_density(fill = "#7393C9", color = "#013C5E") +
  theme_graph() +
  theme(plot.background = element_rect(fill = "transparent", colour = NA),
        panel.background = element_rect(fill = "transparent", colour = NA)) +
  annotate("segment", y = 0, yend = 0.23, x = 20, xend = 20, colour = "black", linetype = "dashed", linewidth = 0.2) +
  annotate("text", x = 20, y = 0.2275, label = "Macroalgae", hjust = -0.175, color = "black") +
  annotate("segment", x = 20.75, y = 0.2175, xend = 24, yend = 0.2175,
           arrow = arrow(length = unit(0.015, "npc")), color = "#576574") +
  annotate("text", x = 20, y = 0.2275, label = "Turf algae", hjust = 1.2, color = "#013C5E") +
  annotate("segment", x = 19.25, y = 0.2175, xend = 16, yend = 0.2175,
           arrow = arrow(length = unit(0.015, "npc")), color = "#576574") +
  annotate("segment", y = 0, yend = 0.21, x = 3, xend = 3, colour = "black", linetype = "dashed", linewidth = 0.2) +
  annotate("text", x = 3, y = 0.2075, label = "LSATs", hjust = -0.2, color = "#013C5E") +
  annotate("segment", x = 3.5, y = 0.20, xend = 5.5, yend = 0.20,
           arrow = arrow(length = unit(0.015, "npc")), color = "#576574") +
  annotate("text", x = 3, y = 0.2075, label = "SPATs", hjust = 1.2, color = "#013C5E") +
  annotate("segment", x = 2.5, y = 0.20, xend = 0.5, yend = 0.20,
           arrow = arrow(length = unit(0.015, "npc")), color = "#576574") +
  labs(x = "Turf length (mm)", y = "Relative frequency density") +
  lims(x = c(0, 25))

ggsave("figs/04_case-studies/case-study_turf_plot-b.png", width = 6, height = 6, dpi = 300)
ggsave("figs/04_case-studies/case-study_turf_plot-b.pdf", width = 6, height = 6)

## 6.2 Figure C ----

data_turf_sed <- read.csv("data/14_case-studies/c_TurfVsSediment.csv") %>% 
  mutate(TurfLength.mm. = log10(TurfLength.mm.),
         SedimentLoad.g_m2. = log10(SedimentLoad.g_m2.))

plot_c <- ggplot(data = data_turf_sed, aes(x = TurfLength.mm., y = SedimentLoad.g_m2.)) +
  geom_point(color = "#7393C9") +
  geom_smooth(method = "lm", se = FALSE, color = "#013C5E") +
  theme_graph() +
  theme(plot.background = element_rect(fill = "transparent", colour = NA),
        panel.background = element_rect(fill = "transparent", colour = NA)) +
  labs(x = bquote("Turf length (mm; " ~ log[10] * ")"),
       y = bquote("Sediment load (g." ~ m^{-2} * ";" ~ log[10] * ")"))

ggsave("figs/04_case-studies/case-study_turf_plot-c.png", width = 6, height = 6, dpi = 300)
ggsave("figs/04_case-studies/case-study_turf_plot-c.pdf", width = 6, height = 6)
