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

# 3. Load data ----

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

# 4. Make the plots ----

color_scalebar <- "black"

## 4.1 2020 data ----

plot_a <- ggplot() +
  geom_sf(data = data_all %>% filter(report == "nb_surveys_2020"),
          aes(fill = colors), color = "#747d8c", show.legend = FALSE) +
  scale_fill_manual(breaks = c("0", "1-5", "6-10", "11-20", "21-45", "46-70", "71-120"),
                    values = c("#ecf0f1", "#82ccdd", "#3498db", "#fa983a", "#e74c3c", "#c0392b", "#B53471")) +
  geom_sf(data = data_countries, fill = "#dadfe1", color = "black", linewidth = 0.15) +
  theme_map() +
  theme(panel.border = element_rect(fill = NA, color = "black"),
        panel.background = element_rect(fill = "white"),
        panel.grid = element_blank(),
        plot.background = element_rect(fill = "transparent", color = NA),
        axis.text = element_text(family = font_choose_map, color = "black"),
        axis.text.y = element_text(angle = 90, hjust = 0.5),
        axis.text.y.right = element_text(angle = -90, hjust = 0.5),
        legend.frame = element_rect(fill = "white"),
        legend.key = element_rect(fill = "white"),
        legend.text = element_text(family = font_choose_map, size = 6),
        legend.background = element_rect(fill = "white", color = "black", linewidth = 0.2),
        plot.title = element_markdown(hjust = 0)) + 
  coord_sf(xlim = c(45, 65), ylim = c(15, 32),
           label_axes = list(left = "N", bottom = "E")) +
  annotation_scale(location = "br",
                   width_hint = 0.25, text_family = font_choose_map, text_col = color_scalebar,
                   text_cex = 0.7, style = "bar", line_width = 1, height = unit(0.04, "cm"),
                   line_col = color_scalebar, pad_x = unit(0.5, "cm"), pad_y = unit(0.5, "cm"),
                   bar_cols = c(color_scalebar, color_scalebar)) +
  labs(title = "**A.** <span style='color:#2980b9'>2020</span> GCRMN report")

## 4.2 2025 data ----

plot_b <- ggplot() +
  geom_sf(data = data_all %>% filter(report == "nb_surveys_2025"),
          aes(fill = nb_surveys), color = "#747d8c", show.legend = TRUE) +
  scale_fill_gradient2(name = "Number of surveys", low = "#fdcb6e", mid = "#e17055", high = "#d63031",
                       limits = c(1, 120), midpoint = 5) +
  geom_sf(data = data_countries, fill = "#dadfe1", color = "black", linewidth = 0.15) +
  theme_map() +
  theme(panel.border = element_rect(fill = NA, color = "black"),
        panel.background = element_rect(fill = "white"),
        panel.grid = element_blank(),
        plot.background = element_rect(fill = "transparent", color = NA),
        axis.text = element_text(family = font_choose_map, color = "black"),
        axis.text.y = element_text(angle = 90, hjust = 0.5),
        axis.text.y.right = element_text(angle = -90, hjust = 0.5),
        legend.frame = element_rect(fill = "white"),
        legend.key = element_rect(fill = "white"),
        legend.direction = "vertical",
        legend.text = element_text(family = font_choose_map, size = 6),
        legend.background = element_rect(fill = "white", color = NA, linewidth = 0),
        plot.title = element_markdown(hjust = 0)) + 
  coord_sf(xlim = c(45, 65), ylim = c(15, 32),
           label_axes = list(right = "N", bottom = "E")) +
  annotation_scale(location = "br",
                   width_hint = 0.25, text_family = font_choose_map, text_col = color_scalebar,
                   text_cex = 0.7, style = "bar", line_width = 1, height = unit(0.04, "cm"),
                   line_col = color_scalebar, pad_x = unit(0.5, "cm"), pad_y = unit(0.5, "cm"),
                   bar_cols = c(color_scalebar, color_scalebar)) +
  labs(title = "**B.** <span style='color:#2980b9'>2025</span> GCRMN report")

## 4.3 Combine the plots ----

plot_full <- plot_a + plot_b + plot_layout(guides = "collect")

## 4.4 Save the plot ----

ggsave("figs/03_case-studies/ropme-raw.png", height = 5.5, width = 10.5, bg = "transparent", dpi = 300)
