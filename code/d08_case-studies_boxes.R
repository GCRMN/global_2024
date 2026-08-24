# 1. Load packages ----

library(tidyverse)
library(sf)
library(s2)
sf_use_s2(FALSE)
library(ggspatial) # For annotation_scale function
library(patchwork)
library(ggtext)
library(ggsflabel)
library(maptiles)
library(tidyterra)

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
                    values = rev(c("#ecf0f1", "#CDF3EE", "#8CCACC", "#83D7F2", "#486D96", "#1A497C", "#0C2F3B")),
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
  labs(title = "A. <span style='color:#1A497C'>2020</span> GCRMN report")

### 3.6.2 2025 data ----

plot_b <- ggplot() +
  geom_sf(data = data_all %>% filter(report == "nb_surveys_2025"),
          aes(fill = colors), color = "#747d8c", show.legend = TRUE) +
  scale_fill_manual(breaks = rev(c("0", "1-5", "6-10", "11-20", "21-45", "46-70", "71-120")),
                    values = rev(c("#ecf0f1", "#CDF3EE", "#8CCACC", "#83D7F2", "#486D96", "#1A497C", "#0C2F3B")),
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
  labs(title = "B. <span style='color:#1A497C'>2025</span> GCRMN report")

### 3.6.3 Combine the plots ----

plot_full <- plot_a + plot_b + plot_layout(guides = "collect") &
  theme(plot.background = element_rect(fill = "transparent", colour = NA),
        panel.background = element_rect(fill = "transparent", colour = NA),
        legend.text = element_text(size = 11),
        legend.title = element_text(hjust = 0))

### 3.6.4 Save the plot ----

ggsave("figs/04_case-studies/case-study_ropme.png", height = 5.5, width = 10.5, bg = "transparent", dpi = 300)
ggsave("figs/04_case-studies/case-study_ropme.pdf", height = 5.5, width = 10.5, bg = "transparent")

# 4. Comparison 2020 vs 2025 trends ----

## 4.1 Load and transform data ----

color_2020 <- "#1A497C"
color_2025 <- "#40A6AA"

load("data/model-results.RData")

data_2025 <- data_models %>% 
  filter(level %in% c("global", "region") & category == "Hard coral") %>% 
  mutate(source = "2025 GCRMN report",
         color = color_2025) %>% 
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
         color = color_2020) %>% 
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
           family = font_choose_graph, fill = color_2025, color = "white") +
  annotate("label", x = 2015, y = 37.5, label = "2020 GCRMN report",
           family = font_choose_graph, fill = color_2020, color = "white") +
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
  mutate(color = case_when(report == "report 2025" ~ color_2025,
                           report == "report 2020" ~ color_2020))

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

rm(data_models, data_2020, data_2025, data_sites_2020, data_sites_2025, data_sites,
   color_2025, color_2020)

# 5. Box absolute vs relative values ----

data_box <- tibble(year = seq(2000, 2010, 1),
                   cover = c(38, 39, 35, 37, 38, 39, 42, 38, 37,
                             41, 19),
                   position = seq(1,11,1)) %>% 
  mutate(color = case_when(position == max(position) ~ "#1A497C",
                           position == max(position-1) ~ "#FF856D",
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

(plot_a + labs(title = "**A**. Absolute change")) + 
  (plot_a + labs(title = "**B**. Relative change")) &
  theme(plot.background = element_rect(fill = "transparent", colour = NA),
        panel.background = element_rect(fill = "transparent", colour = NA))

ggsave("figs/04_case-studies/case-study_abs-rel.png", width = 8, height = 4, dpi = 300)
ggsave("figs/04_case-studies/case-study_abs-rel.pdf", width = 8, height = 4)

# 6. Turf algae case study ----

## 6.1 Figure B ----

data_turf_length <- read.csv("data/14_case-studies/b_TurfLength.csv")

plot_b <- ggplot(data = data_turf_length, aes(x = TurfLength.mm.)) +
  geom_density(fill = "#1A497C", color = "#0C2F3B") +
  theme_graph() +
  theme(plot.background = element_rect(fill = "transparent", colour = NA),
        panel.background = element_rect(fill = "transparent", colour = NA)) +
  annotate("segment", y = 0, yend = 0.23, x = 20, xend = 20,
           colour = "black", linetype = "dashed", linewidth = 0.2) +
  annotate("text", x = 20, y = 0.2275, label = "Macroalgae", hjust = -0.175,
           color = "black", family = font_choose_graph, size = 4) +
  annotate("segment", x = 20.75, y = 0.2175, xend = 24, yend = 0.2175,
           arrow = arrow(length = unit(0.015, "npc")), color = "#576574") +
  annotate("text", x = 20, y = 0.2275, label = "Turf algae", hjust = 1.2,
           color = "#1A497C", family = font_choose_graph, size = 4) +
  annotate("segment", x = 19.25, y = 0.2175, xend = 16, yend = 0.2175,
           arrow = arrow(length = unit(0.015, "npc")), color = "#576574") +
  annotate("segment", y = 0, yend = 0.21, x = 3, xend = 3, colour = "black", linetype = "dashed", linewidth = 0.2) +
  annotate("text", x = 3, y = 0.2075, label = "LSATs", hjust = -0.2,
           color = "#1A497C", family = font_choose_graph, size = 4) +
  annotate("segment", x = 3.5, y = 0.20, xend = 5.5, yend = 0.20,
           arrow = arrow(length = unit(0.015, "npc")), color = "#576574") +
  annotate("text", x = 3, y = 0.2075, label = "SPATs", hjust = 1.2,
           color = "#1A497C", family = font_choose_graph, size = 4) +
  annotate("segment", x = 2.5, y = 0.20, xend = 0.5, yend = 0.20,
           arrow = arrow(length = unit(0.015, "npc")), color = "#576574") +
  labs(x = "Turf length (mm)", y = "Relative frequency density") +
  lims(x = c(0, 25))

ggsave("figs/04_case-studies/case-study_turf_plot-b.png", width = 6, height = 5, dpi = 300)
ggsave("figs/04_case-studies/case-study_turf_plot-b.pdf", width = 6, height = 5)

## 6.2 Figure C ----

data_turf_sed <- read.csv("data/14_case-studies/c_TurfVsSediment.csv")

plot_c <- ggplot(data = data_turf_sed, aes(x = TurfLength.mm., y = SedimentLoad.g_m2.)) +
  geom_point(color = "#1A497C") +
  scale_x_log10() +
  scale_y_log10() +
  geom_smooth(method = "lm", se = FALSE, color = "#0C2F3B") +
  theme_graph() +
  theme(plot.background = element_rect(fill = "transparent", colour = NA),
        panel.background = element_rect(fill = "transparent", colour = NA)) +
  labs(x = bquote("Turf length (mm; " ~ log[10] * ")"),
       y = bquote("Sediment load (g." ~ m^{-2} * ";" ~ log[10] * ")"))

ggsave("figs/04_case-studies/case-study_turf_plot-c.png", width = 6, height = 5, dpi = 300)
ggsave("figs/04_case-studies/case-study_turf_plot-c.pdf", width = 6, height = 5)

# 7. WIO case study ----

## 7.1 Maps ----

color_scalebar <- "black"

color_country <- tibble(country = c("Kenya", "Tanzania", "Madagascar"),
                        color = c("#40A6AA", "#31BDEA", "#1A497C"))

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
             family = font_choose_graph, hjust = 0, vjust = 1, size = 4.5,
             fill = "#40A6AA", color = "white", linewidth = 0, label.r = unit(0, "lines"))

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
             family = font_choose_graph, hjust = 1, vjust = 1, size = 4.5,
             fill = "#31BDEA", color = "white", linewidth = 0, label.r = unit(0, "lines"))

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
             family = font_choose_graph, hjust = 0, vjust = 1, size = 4.5,
             fill = "#1A497C", color = "white", linewidth = 0, label.r = unit(0, "lines"))

plot_region + plot_kenya + plot_tanzania + plot_mada & 
  theme(plot.background = element_rect(fill = "transparent", colour = NA),
        panel.background = element_rect(fill = "transparent", colour = NA))

ggsave("figs/04_case-studies/case-study_wio-a.png", width = 6.8, height = 7, dpi = 300, bg = "transparent")

## 7.2 Heatmap ----

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

# 8. Reef maps case study ----

## 8.1 Map ----

data_countries <- read_sf("data/01_maps/01_raw/03_natural-earth/ne_10m_admin_0_countries/ne_10m_admin_0_countries.shp")

data_subregions <- read_sf("data/01_maps/02_clean/04_subregions/gcrmn_subregions.shp") %>% 
  filter(region == "Australia")

data_reefs <- read_sf("data/01_maps/02_clean/02_reefs-buffer/reefs_buffer_20.shp") %>% 
  filter(region == "Australia")

data_reefs <- st_intersection(data_reefs, data_subregions)

lon_rect <- c(113.34, 114.31)
lat_rect <- c(-29.13, -28.14)

rect_houtman <- st_polygon(list(matrix(
  c(
    min(lon_rect), min(lat_rect),
    max(lon_rect), min(lat_rect),
    max(lon_rect), max(lat_rect),
    min(lon_rect), max(lat_rect),
    min(lon_rect), min(lat_rect)
  ),
  ncol = 2,
  byrow = TRUE
))) %>% 
  st_sfc(crs = 4326) %>% 
  st_sf(geometry = .)

plot_i <- ggplot() +
  geom_sf(data = data_reefs, fill = "#ad5fad", color = "#ad5fad") +
  geom_sf(data = data_subregions, color = "darkgrey", fill = NA, linewidth = 0.25) +
  geom_sf(data = data_countries, color = "black", linewidth = 0.15) +
  geom_sf(data = rect_houtman, fill = NA, color = "black", linewidth = 0.5) +
  theme(panel.border = element_rect(fill = NA, color = "black"),
        panel.background = element_rect(fill = "transparent"),
        panel.grid = element_blank(),
        plot.background = element_rect(fill = "transparent", color = NA),
        axis.text = element_text(family = font_choose_map, color = "black"),
        axis.text.y = element_text(angle = 90, hjust = 0.5),
        axis.text.y.right = element_text(angle = -90, hjust = 0.5)) + 
  coord_sf(xlim = c(91, 170), ylim = c(-37, -7),
           label_axes = list(top = "E", left = "N", right = "N"))

ggsave("figs/04_case-studies/case-study_reef-maps_b_raw.png",
       height = 4.2, width = 8.5, bg = "transparent", dpi = fig_resolution)

## 8.2 Barplot ----

data_extent <- read.csv("data/14_case-studies/emma_kennedy/ReefArea_by_GCRMNecoregion.csv") %>% 
  rename(subregion = X) %>% 
  pivot_longer(2:ncol(.), names_to = "source", values_to = "extent") %>% 
  mutate(source = str_remove_all(source, "_AREA"),
         color = case_when(source == "WRI" ~ "#ad5fad",
                           source == "ACA" ~ "#7BA894",
                           source == "NESP" ~ "#6798C5"),
         source = factor(source, c("WRI", "ACA", "NESP"))) %>% 
  group_by(source) %>% 
  mutate(sum_extent = sum(extent),
         perc_extent = round((extent*100)/sum_extent,0)) %>% 
  ungroup() %>% 
  mutate(perc_extent = case_when(subregion == "Australia 5" ~ paste0(perc_extent, "%"),
                                 TRUE ~ ""))

ggplot(data = data_extent, aes(x = source, y = extent, fill = color)) +
  geom_bar(stat = "identity", show.legend = FALSE, width = 0.8) +
  geom_text(aes(label = perc_extent), family = font_choose_graph, size = 4, vjust = -1, hjust = 0.5) +
  scale_fill_identity() +
  facet_wrap(~subregion, nrow = 1, strip.position = "bottom") +
  scale_y_continuous(limits = c(0, 30000),
                     breaks = seq(0, 30000, by = 5000),
                     labels = scales::label_comma(big.mark = ",")) +
  theme_graph() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.background = element_rect(fill = "transparent", color = NA),
        plot.background = element_rect(fill = "transparent", color = NA),
        strip.text = element_text(family = font_choose_graph),
        axis.line.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank()) +
  labs(x = NULL, y = "Coral reef extent (km²)")

ggsave("figs/04_case-studies/case-study_reef-maps_c.pdf",
       height = 4, width = 11, bg = "transparent")

# 9 Traditional stewardship of coral reefs ----

## 9.1 Load and transform data ----

data_dca <- read_sf("data/14_case-studies/calawit/dca.kml") %>% 
  st_transform(crs = 4326) %>% 
  st_zm(drop = TRUE, what = "ZM") %>% 
  st_make_valid() %>% 
  st_cast("MULTIPOLYGON") %>% 
  mutate(legend = "Dugong Conservation\nAreas")

data_tagbanwa <- read_sf("data/14_case-studies/calawit/doc.kml") %>% 
  st_transform(crs = 4326) %>% 
  st_zm(drop = TRUE, what = "ZM") %>% 
  st_make_valid() %>% 
  st_cast("MULTIPOLYGON") %>% 
  mutate(legend = "Tagbanwa Calawit\nAncestral Domain")

data_wma <- read_sf("data/14_case-studies/calawit/calawit_wma.kml") %>% 
  filter(row_number() == 3) %>% 
  st_transform(crs = 4326) %>% 
  st_zm(drop = TRUE, what = "ZM") %>% 
  st_make_valid() %>% 
  st_cast("MULTIPOLYGON") %>%
  mutate(legend = "Women Managed\nArea")

data_adopt <- read_sf("data/14_case-studies/calawit/adopt a reef.kml") %>% 
  st_transform(crs = 4326) %>% 
  st_zm(drop = TRUE, what = "ZM") %>% 
  st_make_valid() %>% 
  st_cast("MULTIPOLYGON") %>%
  mutate(legend = "Adopt a Reef\nAreas")

data_areas <- bind_rows(data_dca, data_wma, data_tagbanwa, data_adopt) %>% 
  mutate(legend = factor(legend, c("Tagbanwa Calawit\nAncestral Domain",
                                   "Dugong Conservation\nAreas",
                                   "Women Managed\nArea", "Adopt a Reef\nAreas")),
         geometry = st_set_crs(geometry + c(5, 0), 4326))

data_countries <- read_sf("data/01_maps/01_raw/03_natural-earth/ne_10m_admin_0_countries/ne_10m_admin_0_countries.shp")

# data_calamianes shp generated using the GEE script "code/misc/land_boundaries_calawit.js"
data_calamianes <- read_sf("data/14_case-studies/calawit/calamianes_land_boundaries.shp")

color_scalebar <- "black"

data_bboxes <- tibble(plot = c("plot_a", "plot_b"),
                      xmin = c(119.5, 119.75),
                      xmax = c(120.7, 120.15),
                      ymin = c(11.5, 12.23),
                      ymax = c(12.6, 12.52))

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

## 9.2 Plot a ----

plot_a <- ggplot() +
  geom_sf(data = data_countries) +
  geom_sf(data = data_bboxes %>% filter(plot == "plot_a"), color = "#013C5E", fill = NA, linewidth = 0.6) +
  coord_sf(xlim = c(114.9, 129.8), ylim = c(3.8, 20.2),
           label_axes = list(top = "E", left = "N")) +
  scale_x_continuous(breaks = c(116, 122, 127)) +
  theme_map() +
  theme(panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent"),
        axis.text.x = element_text(hjust = 0.5, size = 10),
        axis.text.y = element_text(hjust = 0.5, size = 10, angle = 90))

## 9.3 Plot b ----

plot_b <- ggplot() +
  geom_sf(data = data_calamianes) +
  geom_sf(data = data_bboxes %>% filter(plot == "plot_b"), color = "#013C5E", fill = NA, linewidth = 0.6) +
  coord_sf(xlim = c(119.5, 120.7), ylim = c(11.5, 12.6),
           label_axes = list(bottom = "E", left = "N")) +
  scale_x_continuous(breaks = c(119.6, 120.1, 120.6)) +
  scale_y_continuous(breaks = c(11.6, 12.1, 12.6)) +
  theme_map() +
  theme(panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent"),
        axis.text.x = element_text(hjust = 0.5, size = 10),
        axis.text.y = element_text(hjust = 0.5, size = 10, angle = 90))

## 9.4 Plot c ----

plot_c <- ggplot() +
  # Generate the legend
  geom_sf(data = data_areas, aes(color = legend, fill = legend), alpha = 0.5) +
  scale_fill_manual(name = "Areas", values = c("Dugong Conservation\nAreas" = "#5c53a5",
                                               "Tagbanwa Calawit\nAncestral Domain" = "#3288bd",
                                               "Women Managed\nArea" = "#d53e4f",
                                               "Adopt a Reef\nAreas" = "#feca57")) +
  scale_color_manual(name = "Areas", values = c("Dugong Conservation\nAreas" = "#5c53a5",
                                                "Tagbanwa Calawit\nAncestral Domain" = "#3288bd",
                                                "Women Managed\nArea" = "#d53e4f",
                                                "Adopt a Reef\nAreas" = "#feca57")) +
  # Add the layers
  geom_sf(data = data_tagbanwa, color = "#3288bd", fill = "#3288bd", alpha = 0.3) +
  geom_sf(data = data_dca, color = "#5c53a5", fill = "#5c53a5", alpha = 0.5) +
  geom_sf(data = data_wma, color = "#d53e4f", fill = "#d53e4f", alpha = 0.5) +
  geom_sf(data = data_adopt, color = "#feca57", fill = "#feca57", alpha = 0.5) +
  geom_sf(data = data_calamianes) +
  annotation_scale(location = "tl",
                   width_hint = 0.25, text_family = font_choose_map, text_col = color_scalebar,
                   text_cex = 0.8, style = "bar", line_width = 1, height = unit(0.04, "cm"),
                   line_col = color_scalebar, pad_x = unit(0.5, "cm"), pad_y = unit(0.5, "cm"),
                   bar_cols = c(color_scalebar, color_scalebar)) +
  coord_sf(xlim = c(119.75, 120.15), ylim = c(12.23, 12.52),
           label_axes = list(bottom = "E", right = "N", top = "E")) +
  scale_x_continuous(breaks = c(119.8, 119.9, 120.0, 120.1)) +
  scale_y_continuous(breaks = c(12.5, 12.4, 12.3)) +
  theme_map() +
  theme(panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent"),
        axis.text.x = element_text(hjust = 0.5, size = 10),
        legend.position = c(0.86, 0.37),
        legend.direction = "vertical",
        legend.title = element_text(hjust = 0, size = 9, family = font_choose_graph),
        legend.text = element_text(size = 7, family = font_choose_graph),
        legend.spacing.y = unit(0.4, "cm"),
        legend.background = element_rect(fill = "transparent", color = NA, linewidth = 0),
        legend.key.height = unit(0.5, "cm"),
        legend.key.width = unit(0.5, "cm"),
        legend.key.spacing.y = unit(0.3, "cm"),
        axis.text.y.right = element_text(hjust = 0.5, size = 10, angle = -90))

## 9.5 Combine and export ----

((plot_a / plot_b) | plot_c) + plot_layout(widths = c(1, 2.92)) & 
  theme(plot.background = element_rect(fill = "transparent", colour = NA),
        panel.background = element_rect(fill = "transparent", colour = NA))

ggsave("figs/04_case-studies/case-study_traditional.png",
       height = 5, width = 8, bg = "transparent", dpi = fig_resolution)

# 10. Beyond coral cover ----

## 10.1 Time series ----

load("data/14_case-studies/beyond_coral/GCRMN_CaseStudy_HC_reefs.RData")

plot_timeseries <- function(domain_name_value, event_year, arrow_colour, subplot) {
  
  plot_i <- GCRMN_CaseStudy_HC_reefs %>%
    filter(domain_name == domain_name_value) %>%
    mutate(cond.range = case_when(report_year > event_year ~ "After", .default = "Before"),
           across(c("median", "lower", "upper"), ~.x*100)) %>%
    ggplot() +
      geom_line(aes(y = median, x = report_year), color = "grey", linewidth = 0.65,
              show.legend = FALSE) +
      geom_line(aes(y = median, x = report_year, color = cond.range), linewidth = 0.75,
                show.legend = FALSE) +
      geom_point(aes(y = median, x = report_year, fill = cond.range),
                 size = 3, show.legend = FALSE, shape = 21, color = "white") +
      geom_linerange(aes(x = report_year, ymin = lower, ymax = upper, color = cond.range),
                     linewidth = 0.3, linetype = "dashed", show.legend = FALSE) +
      annotate(geom = "segment", x = event_year, y = 25, xend = event_year, yend = 15,
               linewidth = 1, colour = arrow_colour, lineend = "butt",
               arrow = arrow(length = unit(0.2, "cm"), type = "closed", angle = 30)) +
      scale_x_continuous(limits = c(1993, 2025), breaks = seq(1995, 2025, 5)) +
      scale_y_continuous(limits = c(0, 85)) +
      scale_color_manual(values = c("grey", "black")) +
      scale_fill_manual(values = c("grey", "black")) +
      labs(x = "Year", y = "Hard coral cover (%)") +
      theme_graph() +
      theme(panel.background = element_rect(fill = NA, colour = NA),
            axis.title.x = element_text(size = 18),
            axis.title.y = element_text(size = 18),
            axis.text.x = element_text(size = 16),
            axis.text.y = element_text(size = 16),
            plot.background = element_rect(fill = NA, colour = NA))
  
  ggsave(paste0("figs/04_case-studies/case-study_beyond-coral_plot-", subplot, ".pdf"),
         height = 4, width = 8.25, bg = "transparent")
  
}

plot_timeseries(domain_name_value = "SNAPPER ISLAND", event_year = 2018,
                          arrow_colour = "#FF856D", subplot = "a")

plot_timeseries(domain_name_value = "LADY MUSGRAVE ISLAND", event_year = 2012,
                          arrow_colour = "#1A497C", subplot = "c")

## 10.2 Plot radar chart ----

source("code/function/script_case-study_mgr.R")

# 11. The 4th Global Bleaching Event ----

## 11.1 Figure 1 ----

data_f1 <- read.csv("data/14_case-studies/4gbe/Fig1_data.csv") %>% 
  mutate(Date = as.Date(Date, format = "%d/%m/%Y"),
         ONI = case_when(ONI_value >= 0.5 ~ "El Niño",
                         ONI_value <= -0.5 ~ "La Niña",
                         TRUE ~ "Neutral")) %>% 
  # Define the periods based on Table 1 of Spady et al. (2026)
  mutate(event = case_when(Date < as.Date("1997-09-24") ~ "NGBE1",
                           Date >= as.Date("1997-09-24") & Date <= as.Date("1999-02-25") ~ "GBE1",
                           Date > as.Date("1999-02-25") & Date < as.Date("2009-06-23")~ "NGBE2",
                           Date >= as.Date("2009-06-23") & Date <= as.Date("2011-09-04") ~ "GBE2",
                           Date > as.Date("2011-09-04") & Date < as.Date("2014-08-28") ~ "NGBE3",
                           Date >= as.Date("2014-08-28") & Date <= as.Date("2017-12-15") ~ "GBE3",
                           Date > as.Date("2017-12-15") & Date < as.Date("2018-08-13") ~ "NGBE4",
                           Date >= as.Date("2018-08-13") & Date <= as.Date("2025-12-17") ~ "GBE4"),
         type = case_when(str_sub(event, 1, 1) == "G" ~ "GBE",
                          TRUE ~ "NGBE"))

data_f1_mean <- data_f1 %>% 
  filter(Date >= as.Date("2019-01-01") & Date <= as.Date("2023-12-31")) %>% 
  summarise(Mean_SST = mean(Mean_SST))

data_f1_labels <- data_f1 %>% 
  filter(Date %in% c(as.Date("1998-04-28"), as.Date("2010-04-19"), as.Date("2016-03-18"), as.Date("2024-04-07"))) %>% 
  mutate(label = case_when(Date == as.Date("1998-04-28") ~ "1998\n(GCBE1)",
                           Date == as.Date("2010-04-19") ~ "2010\n(GCBE2)",
                           Date == as.Date("2016-03-18") ~ "2016\n(GCBE3)",
                           Date == as.Date("2024-04-07") ~ "2024\n(GCBE4)"))

plot_a <- ggplot() +
  geom_line(data = data_f1, aes(x = Date, y = Mean_SST, color = type, group = 1)) +
  scale_color_manual(breaks = c("GBE", "NGBE"),
                     labels = c("Global stress period", "Mean SST (daily)"),
                     values = c("#ce6693", "#57606f"),
                     name = NULL) +
  #geom_hline(yintercept = data_f1_mean$Mean_SST, linetype = "dashed") +
  geom_hline(yintercept = 23.2, linetype = "dashed") +
  geom_text(data = data_f1_labels, aes(x = Date, y = Mean_SST+0.35, label = label), family = font_choose_graph) +
  labs(x = "Year", y = "Sea surface\ntemperature (°C)") +
  lims(y = c(22, 24.5)) +
  theme_graph() +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.line.x = element_blank(),
        axis.ticks.x = element_blank(),
        legend.position = c(0.02, 0.86),
        legend.justification = c(0, 0),
        legend.direction = "horizontal",
        legend.background = element_rect(fill = NA, colour = NA))

data_plot <- data_f1 %>%
  drop_na(ONI_value) %>%
  arrange(Date) %>%
  mutate(Date_next = lead(Date),
         ONI_next = lead(ONI_value),
         segment_color = case_when(
           ONI_value >= 0.5 & ONI_next >= 0.5 ~ "El Niño",
           ONI_value <= -0.5 & ONI_next <= -0.5 ~ "La Niña",
           TRUE ~ "Neutral"),
         segment_color = factor(segment_color, c("El Niño", "Neutral", "La Niña"))) %>%
  drop_na(Date_next, ONI_next)

data_plot_labels <- data_f1 |> 
  drop_na(ONI_value) |> 
  filter(Date %in% c("1987-08-01", "1992-01-01", "1997-12-01", "2009-11-30", "2015-12-01", "2023-12-01")) |> 
  mutate(label = sprintf("%.1f °C", ONI_value))

plot_b <- ggplot(data = data_plot) +
  geom_segment(aes(x = Date, xend = Date_next, y = ONI_value,
                   yend = ONI_next, color = segment_color), linewidth = 0.6) +
  scale_color_manual(values = c("El Niño" = "#d64541",
                                "Neutral" = "#57606f",
                                "La Niña" = "#3288bd"),
                     name = NULL) +
  geom_hline(yintercept = 0) +
  geom_hline(yintercept = 1.5, linetype = "dashed", color = "#d64541") +
  geom_text(data = data_plot_labels, aes(x = Date, y = ONI_value+0.4, label = label),
            color = "#d64541", family = font_choose_graph) +
  labs(x = "Year", y = "Oceanic Niño\nIndex (°C)") +
  scale_y_continuous(limits = c(-3, 3),
                     breaks = c(-3, -2, -1, 0, 1, 2, 3),
                     minor_breaks = seq(-3, 3, by = 1),
                     position = "right") +
  theme_graph() +
  theme(panel.background = element_rect(fill = "transparent", colour = NA),
        plot.background = element_rect(fill = "transparent", colour = NA),
        legend.position = c(0.02, 0.02),
        legend.justification = c(0, 0),
        legend.direction = "horizontal",
        legend.background = element_rect(fill = NA, colour = NA),
        plot.margin = margin(t = 0),
        axis.text.y.right = element_text(margin = margin(l = 8)),
        axis.title.y.right = element_text(margin = margin(l = 8)))

plot_i <- plot_a + plot_b +
  plot_layout(ncol = 1, heights = c(2, 1)) & 
  theme(plot.background = element_rect(fill = "transparent", colour = NA),
        panel.background = element_rect(fill = "transparent", colour = NA))

cowplot::ggdraw(plot_i) +
  cowplot::draw_label("'Strong'\nEl Niño",
                      x = 0.11, y = 0.3,
                      hjust = 1, fontfamily = font_choose_graph, size = 10) +
  cowplot::draw_label("Mean SST\n(2019–2023)",
                      #x = 0.91, y = 0.69,
                      x = 0.91, y = 0.68,
                      hjust = 0, fontfamily = font_choose_graph, size = 10) +
  theme(plot.background = element_rect(fill = "transparent", colour = NA),
        panel.background = element_rect(fill = "transparent", colour = NA))

ggsave("figs/04_case-studies/case-study_4gbe_1.pdf", height = 8, width = 11, bg = "transparent")

ggsave("figs/04_case-studies/case-study_4gbe_1.png", height = 8, width = 11, dpi = 300, bg = "transparent")

## 11.2 Figure 2 ----

data_f2 <- read.csv("data/14_case-studies/4gbe/Fig2_data.csv") %>% 
  mutate(Date = as.Date(Date, format = "%d/%m/%Y")) %>% 
  # Define the periods based on Table 1 of Spady et al. (2026)
  mutate(event = case_when(Date < as.Date("1997-09-24") ~ "NGBE1",
                           Date >= as.Date("1997-09-24") & Date <= as.Date("1999-02-25") ~ "GBE1",
                           Date > as.Date("1999-02-25") & Date < as.Date("2009-06-23")~ "NGBE2",
                           Date >= as.Date("2009-06-23") & Date <= as.Date("2011-09-04") ~ "GBE2",
                           Date > as.Date("2011-09-04") & Date < as.Date("2014-08-28") ~ "NGBE3",
                           Date >= as.Date("2014-08-28") & Date <= as.Date("2017-12-15") ~ "GBE3",
                           Date > as.Date("2017-12-15") & Date < as.Date("2018-08-13") ~ "NGBE4",
                           Date >= as.Date("2018-08-13") & Date <= as.Date("2025-12-17") ~ "GBE4"))

ggplot(data = data_f2) +
  geom_ribbon(data = data_f2 %>% filter(str_sub(event, 1, 1) == "G"),
              aes(x = Date, ymin = 0, ymax = Alert_1, group = event,
                  fill = "Global stress periods"), alpha = 0.75) +
  geom_ribbon(data = data_f2 %>% filter(str_sub(event, 1, 1) == "N"),
                  aes(x = Date, ymin = 0, ymax = Alert_1, group = event,
                      fill = "Non-global stress periods"), alpha = 0.75) +
  geom_line(aes(x = Date, y = Alert_1, linetype = "DHW ≥ 4"), linewidth = 0.3) +
  geom_line(aes(x = Date, y = Alert_2, linetype = "DHW ≥ 8"), linewidth = 0.3) +
  scale_fill_manual(name = NULL,
                    values = c("Non-global stress periods" = "#3288bd",
                               "Global stress periods" = "#d64541")) +
  scale_linetype_manual(name = NULL,
                        values = c("DHW ≥ 4" = "solid",
                                   "DHW ≥ 8" = "dashed"),
                        breaks = c("DHW ≥ 4", "DHW ≥ 8"),
                        labels = c("DHW ≥ 4°C-weeks", "DHW ≥ 8°C-weeks")) +
  scale_x_date(date_breaks = "5 years",
               date_minor_breaks = "1 year",
               date_labels = "%Y",
               limits = c(as.Date("1985-01-01"), as.Date("2026-12-31")),
               expand = FALSE) +
  labs(x = "Year",
       y = "Annual Bleaching Stress\nExtent (percent reefs)") +
  theme_graph() +
  theme(panel.background = element_rect(fill = "transparent", colour = NA),
        plot.background = element_rect(fill = "transparent", colour = NA),
        panel.grid.major.x = element_line(colour = "grey85"),
        panel.grid.minor.x = element_line(colour = "grey92"),
        panel.grid.major.y = element_line(colour = "grey85"),
        panel.grid.minor.y = element_blank(),
        legend.position = c(0.02, 0.98),
        legend.justification = c(0, 1),
        legend.direction = "vertical",
        legend.box = "vertical",
        legend.background = element_rect(fill = "transparent", colour = NA),
        legend.box.background = element_rect(fill = "transparent", colour = NA),
        legend.key = element_blank(),
        legend.spacing.y = unit(0.1, "cm"),
        legend.key.height = unit(0.5, "cm"),
        legend.key.width = unit(0.5, "cm")) +
  guides(linetype = guide_legend(order = 1,
                                 ncol = 1,
                                 override.aes = list(fill = NA)),
         fill = guide_legend(order = 2, ncol = 1))

ggsave("figs/04_case-studies/case-study_4gbe_2.pdf", height = 4, width = 11, bg = "transparent")

ggsave("figs/04_case-studies/case-study_4gbe_2.png", height = 4, width = 11, dpi = 300, bg = "transparent")

# 12. MSC case study ----

## 12.1 Sphere map ----

data_land <- read_sf("data/01_maps/01_raw/03_natural-earth/ne_10m_land/ne_10m_land.shp") %>% 
  st_transform(crs = 4326)

data_graticules <- read_sf("data/01_maps/01_raw/03_natural-earth/ne_10m_graticules_15/ne_10m_graticules_15.shp") %>% 
  st_transform(crs = 4326)

longitude <- -70

g <- as_s2_geography(TRUE)
co <- data_land
oc <- s2_difference(g, s2_union_agg(co)) # oceans
b <- s2_buffer_cells(as_s2_geography(paste0("POINT(", longitude," 0)")), 9800000) # visible half
i <- s2_intersection(b, oc) # visible ocean

i <- i %>% 
  st_as_sfc() %>% 
  st_transform(., paste0("+proj=ortho +lat_0=0 +lon_0=", longitude))

b <- b %>% 
  st_as_sfc() %>% 
  st_transform(., paste0("+proj=ortho +lat_0=0 +lon_0=", longitude))

site_cays <- tibble(latitude = 25.51, longitude = -79.27) %>% 
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>% 
  st_as_sfc() %>% 
  st_transform(., paste0("+proj=ortho +lat_0=0 +lon_0=", longitude))

data_graticules <- st_intersection(data_graticules,
                                   i %>% st_transform(crs = 4326) %>% st_make_valid()) %>% 
  st_transform(., paste0("+proj=ortho +lat_0=0 +lon_0=", longitude))

plot_i <- ggplot() +
  geom_sf(data = b, fill = "#0C2F3B", col = "black", linewidth = 0.3) +
  geom_sf(data = i, fill = "#F2F2F2") +
  geom_sf(data = data_graticules, color = "white", linewidth = 1) +
  geom_sf(data = b, fill = NA, col = "#363737", linewidth = 0.4) +
  geom_sf(data = site_cays, color = "white", fill = "#40A6AA", size = 12, shape = 23) +
  theme_minimal() +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        plot.background = element_rect(fill = "transparent", color = NA),
        panel.background = element_rect(fill = "transparent", color = NA))

ggsave("figs/04_case-studies/case-study_msc_map-sphere.png", height = 6, width = 6, bg = "transparent")

## 12.2 Main map ----

data_sites <- tibble(site = c("Lobo Horris", "Bull Run Reef", "Victory Reef", "Tuna Alley", "Welcome to the Jungle"),
                     latitude = c(25.4004, 25.4534, 25.4857, 25.5248, 25.3892),
                     longitude = c(-79.2256, -79.2478, -79.2735, -79.2992, -79.226)) %>% 
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326)

xlim <- c(-79.35, -79.14)

ylim <- c(25.35, 25.56)

bbox <- st_bbox(c(xmin = xlim[1], xmax = xlim[2],
                  ymin = ylim[1], ymax = ylim[2]),
                crs = 4326)

sat <- get_tiles(x = st_as_sfc(bbox), provider = "Esri.WorldImagery",
                 zoom = 15, crop = FALSE)

color_scalebar <- "white"

ggplot() +
  geom_spatraster_rgb(data = sat) +
  geom_sf(data = data_sites, shape = 21, color = "white", fill = "#1A497C", size = 3) +
  geom_sf_label(data = data_sites %>% filter(site %in% c("Victory Reef", "Tuna Alley")),
                aes(label = site), size = 4, nudge_x = 0.005, hjust = 0,
                family = font_choose_graph, fill = "#1A497C", alpha = 0.8, color = "white") +
  geom_sf_label(data = data_sites %>% filter(site %in% c("Welcome to the Jungle", "Lobo Horris",
                                                         "Bull Run Reef")),
                aes(label = site), size = 4, nudge_x = -0.005, hjust = 1,
                family = font_choose_graph, fill = "#1A497C", alpha = 0.8, color = "white") +
  coord_sf(xlim = xlim, ylim = ylim, expand = FALSE, default_crs = st_crs(4326)) +
  annotation_scale(location = "br",
                   width_hint = 0.25, text_family = font_choose_map, text_col = color_scalebar,
                   text_cex = 0.8, style = "bar", line_width = 1, height = unit(0.04, "cm"),
                   line_col = color_scalebar, pad_x = unit(0.5, "cm"), pad_y = unit(0.5, "cm"),
                   bar_cols = c(color_scalebar, color_scalebar)) +
  theme_map() +
  theme(panel.background = element_rect(fill = "transparent", colour = NA),
        plot.background = element_rect(fill = "transparent", colour = NA)) +
  scale_x_continuous(breaks = c(-79.3, -79.2), limits = c(xlim[1], xlim[2])) +
  scale_y_continuous(breaks = c(25.4, 25.5, 25.6), limits = c(ylim[1], ylim[2])) +
  theme(axis.text.y = element_text(hjust = 0.5))

ggsave("figs/04_case-studies/case-study_msc_map.png", height = 6, width = 5.2, bg = "transparent")
