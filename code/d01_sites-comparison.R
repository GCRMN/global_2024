# 1. Load packages ----

library(tidyverse)
library(sf)
sf_use_s2(FALSE)

# 2. Source functions ----

source("code/function/graphical_par.R")
source("code/function/data_descriptors.R")
source("code/function/theme_map.R")
source("code/function/theme_graph.R")

# 3. Plot comparison percentage of number of sites ----

load("data/02_misc/data-benthic.RData")

data_sites_2025 <- data_benthic %>% 
  group_by(region) %>% 
  data_descriptors() %>% 
  # Add locale to avoid having uppercase first (reverse PERSGA and Pacific)
  arrange(region, .locale = "en") %>% 
  bind_rows(., data_descriptors(data_benthic) %>% mutate(region = "Global")) %>% 
  select(region, nb_sites) %>% 
  rename(nb_sites_2025 = nb_sites)

data_sites_2020 <- tibble(region = c(sort(unique(data_benthic$region)), "Global"),
                          nb_sites_2020 = c(372, 35, 3166,2570,352,4050,243,68,389,915,12160))

data_sites <- left_join(data_sites_2025, data_sites_2020) %>% 
  mutate(diff = (nb_sites_2025*100)/nb_sites_2020,
         region = factor(region, c(sort(unique(data_benthic$region)), "Global")))

plot_i <- ggplot(data_sites, aes(x = region, y = diff)) +
  geom_bar(stat = "identity", fill = "#4d8cb3", width = 0.8) +
  geom_hline(yintercept = 100) +
  coord_flip() +
  labs(x = NULL, y = "Percentage of the number of sites included in 2025 report compared to 2020 report") +
  theme_graph() +
  theme(text = element_text(size = 25),
        axis.title.x = element_text(size = 30)) +
  scale_x_discrete(limits = rev)

ggsave("figs/06_additional/02_data-exploration/comp-2020-2025_barplot.png", height = 4.5, width = 9, dpi = 600)

# 4. Map comparison site distribution 2020 vs 2025 ----

## 4.1 Define parameters to center map on the Pacific ----

crs_selected <- st_crs("+proj=eqc +x_0=0 +y_0=0 +lat_0=0 +lon_0=160")

offset <- 180 - 160

polygon <- st_polygon(x = list(rbind(
  c(-0.0001 - offset, 90),
  c(0 - offset, 90),
  c(0 - offset, -90),
  c(-0.0001 - offset, -90),
  c(-0.0001 - offset, 90)))) %>%
  st_sfc() %>%
  st_set_crs(4326)

## 4.2 Transform land ----

data_land <- st_read("data/01_maps/01_raw/03_natural-earth/ne_10m_admin_0_countries/ne_10m_admin_0_countries.shp")

data_land <- st_crop(x = data_land, 
                     y = st_as_sfc(st_bbox(c(xmin = -180, ymin = -48, xmax = 180, ymax = 48), crs = 4326))) %>%
  st_difference(polygon) %>%
  st_transform(crs = crs_selected)

## 4.3 Transform region ----

data_region <- st_read("data/01_maps/02_clean/04_subregions/gcrmn_subregions.shp")

data_region_pac <- data_region %>% 
  filter(region == "Pacific") %>% 
  st_difference(polygon) %>% 
  st_transform(crs = crs_selected)

data_region_pac %<>% # Special pipe from magrittr
  st_buffer(10) %>% # To join polygon (remove vertical line)
  nngeo::st_remove_holes(.)

data_region <- data_region %>% 
  filter(region != "Pacific") %>% 
  st_difference(polygon) %>% 
  st_transform(crs = crs_selected) %>% 
  bind_rows(., data_region_pac)

## 4.4 Tropics ----

data_tropics <- tibble(tropic = c("Cancer", "Cancer", "Equator", "Equator", "Capricorn", "Capricorn"),
                       lat = c(23.4366, 23.4366, 0, 0, -23.4366, -23.4366),
                       long = c(-180, 180, -180, 180, -180, 180)) %>% 
  st_as_sf(coords = c("long", "lat"), crs = 4326) %>%
  group_by(tropic) %>%
  dplyr::summarize(do_union = FALSE) %>%
  st_cast("LINESTRING") %>% 
  st_difference(polygon) %>%
  st_transform(crs = crs_selected)

## 4.5 Transform benthic sites ----

## 4.5.1 2025 GCRMN report benthic cover monitoring sites ----

data_2025 <- data_benthic %>% 
  select(decimalLatitude, decimalLongitude, region) %>% 
  distinct() %>% 
  st_as_sf(coords = c("decimalLongitude", "decimalLatitude"), crs = 4326) %>% 
  mutate(type = "Report 2025")

## 4.5.2 2020 GCRMN report benthic cover monitoring sites ----

data_2020 <- read.csv2("data/02_misc/03-merge_all_all_all_benthos_NA.csv") %>% 
  filter(!(DatasetID %in% c("XLCA1", "XLCA2", "XLCA3", "XLCA4", "XLCA5",
                            "PACN1.1", "PACN1.2", "PACN1.3", "PACN1.4",
                            "TIAH1", "RFCK1"))) %>% # Remove datasets unused by Murray for the analyses
  select(Longitude, Latitude) %>% 
  drop_na(Latitude, Longitude) %>% 
  distinct() %>% 
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326) %>% 
  mutate(type = "Report 2020")

## 4.5.3 Combine ----

data_all <- bind_rows(data_2020, data_2025) %>% 
  mutate(type = as.factor(type)) %>% 
  st_transform(crs = crs_selected)

### 4.5.4 Map (global) ----

plot_i <- ggplot() +
  geom_sf(data = data_tropics, linetype = "dashed", col = "lightgrey") +
  geom_sf(data = data_region, fill = "grey99") +
  geom_sf(data = data_land) +
  geom_sf(data = data_all %>% arrange(type), aes(color = type)) +
  coord_sf(expand = FALSE) +
  theme_map() +
  theme(axis.text.y = element_text(hjust = 0.5),
        axis.text = element_text(size = 16),
        legend.text = element_text(size = 16)) +
  scale_color_manual(values = c("#3EC1EB", "black"),
                     breaks = c("Report 2020", "Report 2025"),
                     drop = FALSE,
                     name = NULL) +
  guides(color = guide_legend(override.aes = list(size = 4)))

ggsave("figs/06_additional/02_data-exploration/comp-2020-2025_global-map.png", height = 4, width = 9, dpi = 600)

### 4.5.5 Map (regional) ----

plot_comparison_region <- function(region_i){
  
  data_region <- data_region %>% 
    filter(region == region_i)
  
  data_bbox <- st_bbox(data_region)
  
  data_benthic_sites_i <- data_all %>% 
    select(-region) %>% 
    st_join(., data_region) %>% 
    filter(region == region_i) %>% 
    st_transform(crs = 4326)
  
  plot_i <- ggplot() +
    geom_sf(data = data_region, fill = NA, color = "grey") +
    geom_sf(data = data_land) +
    geom_sf(data = data_benthic_sites_i %>% arrange(type), aes(color = type)) +
    coord_sf(expand = FALSE) +
    scale_color_manual(values = c("#3EC1EB", "black"),
                       breaks = c("Report 2020", "Report 2025"),
                       drop = FALSE,
                       name = NULL) +
    guides(color = guide_legend(override.aes = list(size = 4))) +
    coord_sf(xlim = c(data_bbox$xmin, data_bbox$xmax), ylim = c(data_bbox$ymin, data_bbox$ymax)) +
    theme_map()
  
  ggsave(paste0("figs/06_additional/02_data-exploration/comp-2020-2025_regional-map_", region_i, ".png"),
         dpi = 300, height = 6, width = 6)
  
}

map(unique(data_region$region), ~plot_comparison_region(region_i = .))
