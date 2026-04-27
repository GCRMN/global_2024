# 1. Load packages ----

library(tidyverse)
library(sf)
sf_use_s2(FALSE)
library(ggspatial) # For annotation_scale function
library(patchwork)
library(ggtext)
library(ggsflabel)

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
  scale_fill_manual(breaks = rev(c("0", "1-5", "6-10", "11-20", "21-45", "46-70", "71-120")),
                    values = rev(c("#ecf0f1", "#82ccdd", "#3498db", "#fa983a", "#e74c3c", "#c0392b", "#B53471")),
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
        legend.text = element_text(family = font_choose_map, size = 6),
        plot.title = element_markdown(hjust = 0),
        panel.background = element_rect(fill = "transparent", colour = NA),
        plot.background = element_rect(fill = "transparent", colour = NA),
        legend.background = element_rect(fill = "transparent", colour = NA)) + 
  coord_sf(xlim = c(45, 65), ylim = c(15, 32),
           label_axes = list(left = "N", bottom = "E")) +
  annotation_scale(location = "br",
                   width_hint = 0.25, text_family = font_choose_map, text_col = color_scalebar,
                   text_cex = 0.8, style = "bar", line_width = 1, height = unit(0.04, "cm"),
                   line_col = color_scalebar, pad_x = unit(0.5, "cm"), pad_y = unit(0.5, "cm"),
                   bar_cols = c(color_scalebar, color_scalebar)) +
  labs(title = "**A.** <span style='color:#2980b9'>2020</span> GCRMN report")

### 3.6.2 2025 data ----

plot_b <- ggplot() +
  geom_sf(data = data_all %>% filter(report == "nb_surveys_2025"),
          aes(fill = colors), color = "#747d8c", show.legend = TRUE) +
  scale_fill_manual(breaks = rev(c("0", "1-5", "6-10", "11-20", "21-45", "46-70", "71-120")),
                    values = rev(c("#ecf0f1", "#82ccdd", "#3498db", "#fa983a", "#e74c3c", "#c0392b", "#B53471")),
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
                   text_cex = 0.8, style = "bar", line_width = 1, height = unit(0.04, "cm"),
                   line_col = color_scalebar, pad_x = unit(0.5, "cm"), pad_y = unit(0.5, "cm"),
                   bar_cols = c(color_scalebar, color_scalebar)) +
  labs(title = "**B.** <span style='color:#2980b9'>2025</span> GCRMN report")

### 3.6.3 Combine the plots ----

plot_full <- plot_a + plot_b + plot_layout(guides = "collect") &
  theme(plot.background = element_rect(fill = "transparent", colour = NA),
        panel.background = element_rect(fill = "transparent", colour = NA),
        legend.text = element_text(size = 11),
        legend.title = element_text(hjust = 0))

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

plot_a <- ggplot(data = data_trends %>% filter(level == "global"), aes(x = year, fill = color, color = color, group  = source)) +
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
  annotate("label", x = 2002, y = 24, label = "2025 GCRMN report",
           family = font_choose_graph, fill = "#c44569", color = "white") +
  annotate("label", x = 2015, y = 37.5, label = "2020 GCRMN report",
           family = font_choose_graph, fill = "#2d98da", color = "white") +
  labs(x = "Year", y = "Hard coral cover (%)", title = "A")

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

## 4.3 Plots of number of sites per year ----

load("data/02_misc/data-benthic.RData")

data_sites_2025 <- data_benthic %>% 
  select(decimalLatitude, decimalLongitude, year) %>% 
  st_drop_geometry() %>% 
  distinct() %>% 
  group_by(year) %>% 
  count() %>% 
  ungroup() %>% 
  mutate(report = "report 2025")

rm(data_benthic)

data_sites_2020 <- read.csv2("data/02_misc/03-merge_all_all_all_benthos_NA.csv") %>% 
  filter(!(DatasetID %in% c("XLCA1", "XLCA2", "XLCA3", "XLCA4", "XLCA5",
                            "PACN1.1", "PACN1.2", "PACN1.3", "PACN1.4",
                            "TIAH1", "RFCK1"))) %>% # Remove datasets unused by Murray for the analyses
  select(Latitude, Longitude, Year) %>% 
  drop_na(Latitude, Longitude, Year) %>% 
  distinct() %>% 
  group_by(Year) %>% 
  count() %>% 
  ungroup() %>% 
  rename(year = Year) %>% 
  mutate(report = "report 2020")

data_sites <- bind_rows(data_sites_2020, data_sites_2025) %>% 
  mutate(color = case_when(report == "report 2025" ~ "#c44569",
                           report == "report 2020" ~ "#2d98da"))

plot_b <- ggplot(data = data_sites %>% filter(report == "report 2020"),
                 aes(x = year, y = n, fill = color)) +
  geom_bar(stat = "identity", show.legend = FALSE, width = 1, alpha = 0.8) +
  scale_fill_identity() +
  labs(x = "Year", y = "Sites", title = "B") +
  coord_cartesian(clip = "off") +
  theme_graph() +
  theme(axis.title.x = element_blank(),
        panel.background = element_rect(fill = "transparent", colour = NA),
        plot.background = element_rect(fill = "transparent", colour = NA)) +
  scale_x_continuous(expand = c(0, 0), limits = c(1979, 2026),
                     breaks = c(1980, 1990, 2000, 2010, 2020), labels = c("1980", "", "2000", "", "2020")) +
  scale_y_continuous(limits = c(0, 6500), labels = scales::label_number(scale = 1/1000, suffix = "K"))

plot_c <- ggplot(data = data_sites %>% filter(report == "report 2025"),
                 aes(x = year, y = n, fill = color)) +
  geom_bar(stat = "identity", show.legend = FALSE, width = 1, alpha = 0.8) +
  scale_fill_identity() +
  labs(x = "Year", y = "Sites", title = "C") +
  coord_cartesian(clip = "off") +
  theme_graph() +
  theme(panel.background = element_rect(fill = "transparent", colour = NA),
        plot.background = element_rect(fill = "transparent", colour = NA)) +
  scale_x_continuous(expand = c(0, 0), limits = c(1979, 2026),
                     breaks = c(1980, 1990, 2000, 2010, 2020), labels = c("1980", "", "2000", "", "2020")) +
  scale_y_continuous(limits = c(0, 6500), labels = scales::label_number(scale = 1/1000, suffix = "K"))

## 4.4 Combine and export the plot ----

plot_a + (plot_b / plot_c) + plot_layout(widths = c(2.5, 1)) &
  theme(plot.background = element_rect(fill = "transparent", colour = NA),
        panel.background = element_rect(fill = "transparent", colour = NA))

ggsave("figs/04_case-studies/case-study_2020-2025.png", width = 9, height = 5)
ggsave("figs/04_case-studies/case-study_2020-2025.pdf", width = 9, height = 5)

rm(data_models, data_2020, data_2025, data_sites_2020, data_sites_2025, data_sites)

# 5. Box absolute vs relative values ----

data_box <- tibble(year = seq(2000, 2010, 1),
                   cover = c(38, 39, 35, 37, 38, 39, 42, 38, 37,
                             41, 19),
                   position = seq(1,11,1)) %>% 
  mutate(color = case_when(position == max(position) ~ "#013C5E",
                           position == max(position-1) ~ "#C44D56",
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
        plot.title = element_markdown(size = 13),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        axis.text.x = element_text(size = 11),
        axis.text.y = element_text(size = 11),
        panel.grid = element_blank(),
        axis.ticks.y = element_blank()) +
  labs(x = "Year", y = "Hard coral cover (%)")

(plot_a + labs(title = "**A.** Absolute change")) + 
  (plot_a + labs(title = "**B.** Relative change")) &
  theme(plot.background = element_rect(fill = "transparent", colour = NA),
        panel.background = element_rect(fill = "transparent", colour = NA))

ggsave("figs/04_case-studies/case-study_abs-rel.png", width = 8, height = 4, dpi = 300)
ggsave("figs/04_case-studies/case-study_abs-rel.pdf", width = 8, height = 4)

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

ggsave("figs/04_case-studies/case-study_turf_plot-b.png", width = 6, height = 5, dpi = 300)
ggsave("figs/04_case-studies/case-study_turf_plot-b.pdf", width = 6, height = 5)

## 6.2 Figure C ----

data_turf_sed <- read.csv("data/14_case-studies/c_TurfVsSediment.csv")

plot_c <- ggplot(data = data_turf_sed, aes(x = TurfLength.mm., y = SedimentLoad.g_m2.)) +
  geom_point(color = "#7393C9") +
  scale_x_log10() +
  scale_y_log10() +
  geom_smooth(method = "lm", se = FALSE, color = "#013C5E") +
  theme_graph() +
  theme(plot.background = element_rect(fill = "transparent", colour = NA),
        panel.background = element_rect(fill = "transparent", colour = NA)) +
  labs(x = bquote("Turf length (mm; " ~ log[10] * ")"),
       y = bquote("Sediment load (g." ~ m^{-2} * ";" ~ log[10] * ")"))

ggsave("figs/04_case-studies/case-study_turf_plot-c.png", width = 6, height = 5, dpi = 300)
ggsave("figs/04_case-studies/case-study_turf_plot-c.pdf", width = 6, height = 5)

# 7. Beyond hard coral case study ----

## 7.1 Temporal trends ----

data <- tibble(plot = "a",
               year = c(1980, 1985, 1990, 1995, 2000, 2005, 2010, 2015, 2020),
               mean_cover = c(32, 38, 42, 35, 34, 15, 12, 8, 18),
               upper_cover = c(34, 40, 44, 37, 37, 18, 13, 10, 22),
               lower_cover = c(30, 36, 40, 31, 32, 10, 11, 4, 17),
               color = c(rep("black", 6), rep("grey", 3)))

ggplot(data = data) +
  geom_line(aes(x = year, y = mean_cover, color = color)) +
  # Add the grey segment after the last black point
  geom_segment(data = data %>% 
                 arrange(year) %>%
                 slice(6:7) %>%
                 summarise(x = first(year),
                           xend = last(year),
                           y = first(mean_cover),
                           yend = last(mean_cover)),
               aes(x = x, xend = xend, y = y, yend = yend),
               color = "grey") +
  geom_linerange(aes(x = year, ymin = lower_cover, ymax = upper_cover, color = color),
             show.legend = FALSE) +
  geom_point(aes(x = year, y = mean_cover, fill = color),
             shape = 21, color = "white", show.legend = FALSE, size = 3) +
  scale_fill_identity() +
  scale_color_identity() +
  theme_graph() +
  theme(plot.background = element_rect(fill = "transparent", colour = NA),
        panel.background = element_rect(fill = "transparent", colour = NA)) +
  lims(y = c(0, NA)) +
  labs(x = "Year", y = "Hard coral cover (%)")

ggsave("figs/04_case-studies/case-study_beyong-coral.pdf", width = 5, height = 4, bg = "transparent")

## 7.2 Radar chart ----

# 8. WIO case study ----

## 8.1 Maps ----

color_scalebar <- "black"

color_country <- tibble(country = c("Kenya", "Tanzania", "Madagascar"),
                        color = c("#f8a07e", "#ce6693", "#5c53a5"))

data_countries <- read_sf("data/01_maps/01_raw/03_natural-earth/ne_10m_admin_0_countries/ne_10m_admin_0_countries.shp")

data_sites <- readxl::read_xlsx("data/14_case-studies/wio_fig-1.xlsx") %>% 
  drop_na(latitude, longitude) %>% 
  filter(Country != "Mozambique") %>% 
  rename(country = Country, site = `Site Name (English)`) %>%
  select(country, site, latitude, longitude) %>% 
  distinct() %>% 
  left_join(., color_country) %>% 
  mutate(site = str_remove_all(site, c("TsimipaikaBay_|Tsimipaika Bay_|Nosy Be Bay_|Ambaro Bay_")),
         site = str_replace_all(site, "Kanamai- mradi", "Kanamai-Mradi"),
         label = case_when(site %in% c("Wasini", "Munje", "Mapasi", "Jimbo", "Kuruwitu", "Antsatrana", "Kanamai-Mradi",
                                       "Ampondrabe", "Ampanakana", "Ambatozavavy", "Nosy Be Bay", "Djamandjar",
                                       "Chipopo", "Magengeni", "Makoongwe", "Somanga") ~ site,
                           TRUE ~ NA)) %>% 
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326)

data_bboxes <- tibble(country = c("Kenya", "Tanzania", "Madagascar"),
                      xmin = c(38.2, 37.5, 47),
                      xmax = c(42.8, 42.2, 49.35),
                      ymin = c(-4.9, -10.8, -13.95),
                      ymax = c(-1.4, -4.5, -11.6)) %>% 
  left_join(., color_country)

geom <- pmap(list(data_bboxes$xmin, data_bboxes$xmax, data_bboxes$ymin, data_bboxes$ymax),
             \(xmin, xmax, ymin, ymax) {
               st_polygon(list(matrix(
                 c(xmin, ymin,
                   xmax, ymin,
                   xmax, ymax,
                   xmin, ymax,
                   xmin, ymin),
                 ncol = 2,
                 byrow = TRUE)))}) %>%
  st_sfc(crs = 4326)

data_bboxes <- st_sf(data_bboxes, geometry = geom)

rm(geom)

plot_region <- ggplot() +
  geom_sf(data = data_countries) +
  geom_sf(data = data_sites, aes(color = color), show.legend = FALSE) +
  scale_color_identity() +
  geom_sf(data = data_bboxes, fill = NA, linewidth = 0.25, color = "black") +
  theme_map() +
  theme(panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent"),
        axis.text.x = element_text(hjust = 0.5, size = 10),
        axis.text.y = element_text(hjust = 0.5, size = 10)) +
  coord_sf(xlim = c(28, 65), ylim = c(-28, 8),
           label_axes = list(top = "E", left = "N")) +
  scale_x_continuous(breaks = c(30, 40, 50, 60)) +
  scale_y_continuous(breaks = c(-25, -15, -5, 5))

plot_kenya <- ggplot() +
  geom_sf(data = data_countries) +
  geom_sf_text_repel(data = data_sites %>% filter(country == "Kenya" & site %in% c("Chipopo", "Kuruwitu", "Kanamai-Mradi", "Munje")),
                     aes(label = label), family = font_choose_graph, size = 3, nudge_x = 0.75, seed = 1) +
  geom_sf_text_repel(data = data_sites %>% filter(country == "Kenya" & !(site %in% c("Chipopo", "Kuruwitu", "Kanamai-Mradi", "Munje"))),
                     aes(label = label), family = font_choose_graph, size = 3, nudge_x = -0.5, seed = 1) +
  geom_sf(data = data_sites %>% filter(country == "Kenya"), aes(color = color), show.legend = FALSE, size = 3) +
  scale_color_identity() +
  theme_map() +
  theme(panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent"),
        axis.text.x = element_text(hjust = 0.5, size = 10),
        axis.text.y.right = element_text(hjust = 0.5, size = 10, angle = -90)) +
  coord_sf(xlim = c(38.2, 42.8), ylim = c(-4.9, -1.4),
           label_axes = list(top = "E", right = "N")) +
  scale_x_continuous(breaks = c(39, 40, 41, 42)) +
  scale_y_continuous(breaks = c(-4.5, -3.5, -2.5, -1.5)) +
  annotation_scale(location = "br",
                   width_hint = 0.25, text_family = font_choose_map, text_col = color_scalebar,
                   text_cex = 0.8, style = "bar", line_width = 1, height = unit(0.04, "cm"),
                   line_col = color_scalebar, pad_x = unit(0.5, "cm"), pad_y = unit(0.5, "cm"),
                   bar_cols = c(color_scalebar, color_scalebar))  +
  geom_label(data = tibble(x = 38.2, y = -1.4), aes(x = x, y = y, label = "Kenya"),
             family = font_choose_graph, hjust = 0, vjust = 1, size = 4,
             fill = "#f8a07e", color = "white", linewidth = 0, label.r = unit(0, "lines"))

plot_tanzania <- ggplot() +
  geom_sf(data = data_countries) +
  geom_sf_text_repel(data = data_sites %>% filter(country == "Tanzania" & site != "Somanga"),
                     aes(label = label), family = font_choose_graph, size = 3, nudge_x = 1.2, linewidth = 0.1) +
  geom_sf_text_repel(data = data_sites %>% filter(country == "Tanzania" & site == "Somanga"),
                     aes(label = label), family = font_choose_graph, size = 3, nudge_x = -1, linewidth = 0.1) +
  geom_sf(data = data_sites %>% filter(country == "Tanzania"), aes(color = color), show.legend = FALSE, size = 3) +
  scale_color_identity() +
  theme_map() +
  theme(panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent"),
        axis.text.x = element_text(hjust = 0.5, size = 10),
        axis.text.y = element_text(hjust = 0.5, size = 10)) +
  coord_sf(xlim = c(37.5, 42.2), ylim = c(-10.8, -4.5),
           label_axes = list(bottom = "E", left = "N")) +
  scale_x_continuous(breaks = c(38, 40, 42)) +
  scale_y_continuous(breaks = c(-10, -8, -6)) +
  annotation_scale(location = "bl",
                   width_hint = 0.25, text_family = font_choose_map, text_col = color_scalebar,
                   text_cex = 0.8, style = "bar", line_width = 1, height = unit(0.04, "cm"),
                   line_col = color_scalebar, pad_x = unit(0.5, "cm"), pad_y = unit(0.5, "cm"),
                   bar_cols = c(color_scalebar, color_scalebar))  +
  geom_label(data = tibble(x = 42.2, y = -4.5), aes(x = x, y = y, label = "Tanzania"),
             family = font_choose_graph, hjust = 1, vjust = 1, size = 4,
             fill = "#ce6693", color = "white", linewidth = 0, label.r = unit(0, "lines"))

plot_mada <- ggplot() +
  geom_sf(data = data_countries) +
  geom_sf_text_repel(data = data_sites %>% filter(country == "Madagascar" & site == "Djamandjar"),
                     aes(label = label), family = font_choose_graph, size = 3, seed = 5, nudge_x = -0.3) +
  geom_sf_text_repel(data = data_sites %>% filter(country == "Madagascar" & site %in% c("Nosy Be Bay", "Ambatozavavy")),
                     aes(label = label), family = font_choose_graph, size = 3, seed = 5, nudge_y = 0.5) +
  geom_sf_text_repel(data = data_sites %>% filter(country == "Madagascar" & site %in% c("Antsatrana", "Ampanakana")),
                     aes(label = label), family = font_choose_graph, size = 3, seed = 5, nudge_y = -0.3, nudge_x = 0.1) +
  geom_sf_text_repel(data = data_sites %>% filter(country == "Madagascar" & site %in% c("Ampondrabe")),
                     aes(label = label), family = font_choose_graph, size = 3, seed = 5, nudge_y = -0.3, nudge_x = -0.6) +
  geom_sf(data = data_sites %>% filter(country == "Madagascar"), aes(color = color),
          show.legend = FALSE, size = 3) +
  scale_color_identity() +
  theme_map() +
  theme(panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent"),
        axis.text.x = element_text(hjust = 0.5, size = 10),
        axis.text.y.right = element_text(hjust = 0.5, size = 10, angle = -90)) +
  coord_sf(xlim = c(47, 49.3), ylim = c(-13.95, -11.6),
           label_axes = list(bottom = "E", right = "N")) +
  scale_x_continuous(breaks = c(47, 48, 49)) +
  scale_y_continuous(breaks = c(-14, -13, -12)) +
  annotation_scale(location = "tr",
                   width_hint = 0.25, text_family = font_choose_map, text_col = color_scalebar,
                   text_cex = 0.8, style = "bar", line_width = 1, height = unit(0.04, "cm"),
                   line_col = color_scalebar, pad_x = unit(0.5, "cm"), pad_y = unit(0.5, "cm"),
                   bar_cols = c(color_scalebar, color_scalebar)) +
  geom_label(data = tibble(x = 47, y = -11.6), aes(x = x, y = y, label = "Madagascar"),
             family = font_choose_graph, hjust = 0, vjust = 1, size = 4,
             fill = "#5c53a5", color = "white", linewidth = 0, label.r = unit(0, "lines"))

plot_region + plot_kenya + plot_tanzania + plot_mada & 
  theme(plot.background = element_rect(fill = "transparent", colour = NA),
        panel.background = element_rect(fill = "transparent", colour = NA))

ggsave("figs/04_case-studies/case-study_wio-a.png", width = 6.8, height = 7, dpi = 300, bg = "transparent")

## 8.2 Heatmap ----

data_oecm <- readxl::read_xlsx("data/14_case-studies/wio_fig-2.xlsx") %>% 
  mutate(criteria = str_replace_all(criteria, "Criterion ", "C"),
         site = str_remove_all(site, c("TsimipaikaBay_|Tsimipaika Bay_|Nosy Be Bay_|Ambaro Bay_")),
         color = case_when(compliance == 0 ~ "black",
                           TRUE ~ "white"))

ggplot(data = data_oecm, aes(x = criteria, y = site, fill = compliance)) +
  geom_tile(width = 0.9, height = 0.9, show.legend = FALSE) +
  scale_fill_gradient(low = "#E4F1FE", high = "#013C5E") +
  geom_text(aes(label = compliance, color = color), family = font_choose_graph) +
  scale_color_identity() +
  theme_graph() +
  theme(panel.background = element_rect(fill = "transparent", color = NA),
        plot.background = element_rect(fill = "transparent", color = NA),
        panel.grid = element_blank(),
        axis.ticks = element_blank()) +
  scale_y_discrete(limits = rev) +
  theme(axis.ticks.x = element_blank(),
        axis.line.x = element_blank()) +
  labs(x = "Assessment criteria", y = NULL)

ggsave("figs/04_case-studies/case-study_wio-b.png", width = 4.5, height = 7.5, dpi = 300, bg = "transparent")
ggsave("figs/04_case-studies/case-study_wio-b.pdf", width = 4.5, height = 7.5, bg = "transparent")

# 9. Reef maps case study ----

## 9.1 Map ----

data_countries <- read_sf("data/01_maps/01_raw/03_natural-earth/ne_10m_admin_0_countries/ne_10m_admin_0_countries.shp")

data_subregions <- read_sf("data/01_maps/02_clean/04_subregions/gcrmn_subregions.shp") %>% 
  filter(region == "Australia")

data_reefs <- read_sf("data/01_maps/02_clean/02_reefs-buffer/reefs_buffer_20.shp") %>% 
  filter(region == "Australia")

data_reefs <- st_intersection(data_reefs, data_subregions)

plot_i <- ggplot() +
  geom_sf(data = data_reefs, fill = "#ad5fad", color = "#ad5fad") +
  geom_sf(data = data_subregions, color = "lightgrey", fill = NA, linewidth = 0.1) +
  geom_sf(data = data_countries, color = "black", linewidth = 0.15) +
  theme(panel.border = element_rect(fill = NA, color = "black"),
        panel.background = element_rect(fill = "transparent"),
        panel.grid = element_blank(),
        plot.background = element_rect(fill = "transparent", color = NA),
        axis.text = element_text(family = font_choose_map, color = "black"),
        axis.text.y = element_text(angle = 90, hjust = 0.5),
        axis.text.y.right = element_text(angle = -90, hjust = 0.5)) + 
  coord_sf(xlim = c(91, 170), ylim = c(-37, -7),
           label_axes = list(top = "E", left = "N", right = "N")) +
  annotation_scale(location = "bl",
                   width_hint = 0.25, text_family = font_choose_map, text_col = "black",
                   text_cex = 0.7, style = "bar", line_width = 1, height = unit(0.04, "cm"),
                   line_col = "black", pad_x = unit(0.5, "cm"), pad_y = unit(0.5, "cm"),
                   bar_cols = c("black", "black"))

ggsave("figs/04_case-studies/case-study_reef-maps_a.png",
       height = 4.2, width = 8.5, bg = "transparent", dpi = fig_resolution)

## 9.2 Barplot ----

data_extent <- tibble(region = paste0("Australia ", sort(rep(seq(1,7), times = 3))),
                      source = rep(c("WRI", "ACA", "NESP"), 7),
                      color = case_when(source == "WRI" ~ "#ad5fad",
                                        source == "ACA" ~ "#7BA894",
                                        source == "NESP" ~ "#6798C5"),
                      label = case_when(region == "Australia 5" & source == "WRI" ~ "86%",
                                        region == "Australia 5" & source == "ACA" ~ "56%",
                                        region == "Australia 5" & source == "NESP" ~ "44%"),
                      extent = c(500, 500, 500, 800, 1000, 5200, 800, 1000, 5200,
                                 500, 5400, 5400, 25000, 20000, 28000,
                                 800, 600, 10000, 200, 200, 400))

ggplot(data = data_extent, aes(x = source, y = extent, fill = color)) +
  geom_bar(stat = "identity", show.legend = FALSE, width = 0.8) +
  geom_text(aes(label = label), family = font_choose_graph, size = 4, vjust = -1, hjust = 0.5) +
  scale_fill_identity() +
  facet_wrap(~region, nrow = 1, strip.position = "bottom") +
  scale_y_continuous(limits = c(0, 30000)) +
  theme_graph() +
  theme(panel.grid = element_blank(),
        panel.background = element_rect(fill = "transparent", color = NA),
        plot.background = element_rect(fill = "transparent", color = NA),
        strip.text = element_text(family = font_choose_graph),
        axis.line.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank()) +
  labs(x = NULL, y = "Coral reef extent (km²)")

ggsave("figs/04_case-studies/case-study_reef-maps_b.pdf",
       height = 4, width = 11, bg = "transparent")

# 10. Traditional stewardship of coral reefs ----

## 10.1 Load and transform data ----

data_dca <- read_sf("data/14_case-studies/Approved_DCA.shp") %>% 
  st_transform(crs = 4326)

data_countries <- read_sf("data/01_maps/01_raw/03_natural-earth/ne_10m_admin_0_countries/ne_10m_admin_0_countries.shp")

data_calamianes <- read_sf("data/14_case-studies/calamianes_land_boundaries.shp")

color_scalebar <- "black"

data_bboxes <- tibble(plot = c("plot_a", "plot_b"),
                      xmin = c(119.5, 119.86),
                      xmax = c(120.7, 119.96),
                      ymin = c(11.5, 12.26),
                      ymax = c(12.6, 12.34))

geom <- pmap(list(data_bboxes$xmin, data_bboxes$xmax, data_bboxes$ymin, data_bboxes$ymax),
             \(xmin, xmax, ymin, ymax) {
               st_polygon(list(matrix(
                 c(xmin, ymin,
                   xmax, ymin,
                   xmax, ymax,
                   xmin, ymax,
                   xmin, ymin),
                 ncol = 2,
                 byrow = TRUE)))}) %>%
  st_sfc(crs = 4326)

data_bboxes <- st_sf(data_bboxes, geometry = geom)

rm(geom)

## 10.2 Plot a ----

plot_a <- ggplot() +
  geom_sf(data = data_countries) +
  geom_sf(data = data_bboxes %>% filter(plot == "plot_a"), color = "#ce6693", fill = NA, linewidth = 0.5) +
  coord_sf(xlim = c(114.9, 129.8), ylim = c(3.8, 20.2),
           label_axes = list(top = "E", left = "N")) +
  scale_x_continuous(breaks = c(116, 122, 127)) +
  theme_map() +
  theme(panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent"),
        axis.text.x = element_text(hjust = 0.5, size = 10),
        axis.text.y = element_text(hjust = 0.5, size = 10, angle = 90))

## 10.3 Plot b ----

plot_b <- ggplot() +
  geom_sf(data = data_calamianes) +
  geom_sf(data = data_bboxes %>% filter(plot == "plot_b"), color = "#ce6693", fill = NA, linewidth = 0.5) +
  coord_sf(xlim = c(119.5, 120.7), ylim = c(11.5, 12.6),
           label_axes = list(bottom = "E", left = "N")) +
  scale_x_continuous(breaks = c(119.6, 120.1, 120.6)) +
  scale_y_continuous(breaks = c(11.6, 12.1, 12.6)) +
  theme_map() +
  theme(panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent"),
        axis.text.x = element_text(hjust = 0.5, size = 10),
        axis.text.y = element_text(hjust = 0.5, size = 10, angle = 90))

## 10.4 Plot c ----

plot_c <- ggplot() +
  geom_sf(data = data_dca, color = "#013C5E", fill = "#013C5E", alpha = 0.3) +
  geom_sf(data = data_calamianes) +
  annotation_scale(location = "tl",
                   width_hint = 0.25, text_family = font_choose_map, text_col = color_scalebar,
                   text_cex = 0.8, style = "bar", line_width = 1, height = unit(0.04, "cm"),
                   line_col = color_scalebar, pad_x = unit(0.5, "cm"), pad_y = unit(0.5, "cm"),
                   bar_cols = c(color_scalebar, color_scalebar)) +
  coord_sf(xlim = c(119.84, 119.96), ylim = c(12.26, 12.36),
           label_axes = list(bottom = "E", right = "N", top = "E")) +
  scale_x_continuous(breaks = c(119.85, 119.90, 119.95)) +
  scale_y_continuous(breaks = c(12.26, 12.31, 12.36)) +
  theme_map() +
  theme(panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent"),
        axis.text.x = element_text(hjust = 0.5, size = 10),
        axis.text.y.right = element_text(hjust = 0.5, size = 10, angle = -90))

## 10.5 Combine and export ----

((plot_a / plot_b) | plot_c) + plot_layout(widths = c(1, 2.525)) & 
  theme(plot.background = element_rect(fill = "transparent", colour = NA),
        panel.background = element_rect(fill = "transparent", colour = NA))

ggsave("figs/04_case-studies/case-study_traditional.png",
       height = 5, width = 8, bg = "transparent", dpi = fig_resolution)
