# 1. Load packages ----

library(tidyverse)
library(sf)
sf_use_s2(FALSE)
library(ggspatial) # For annotation_scale function
library(magrittr) # To use the pipe %<>%
library(ggtext)
library(readxl)

# 2. Source functions ----

source("code/function/graphical_par.R")
source("code/function/data_descriptors.R")
source("code/function/theme_map.R")
source("code/function/theme_graph.R")
source("code/function/map_region_monitoring.R")

# 3. Load data ----

data_land <- st_read("data/01_maps/01_raw/03_natural-earth/ne_10m_admin_0_countries/ne_10m_admin_0_countries.shp")

data_region <- st_read("data/01_maps/02_clean/04_subregions/gcrmn_subregions.shp")

load("data/02_misc/data-benthic.RData")

data_benthic_sites <- data_benthic %>% 
  select(decimalLatitude, decimalLongitude, region, subregion, year) %>% 
  distinct() %>% 
  group_by(decimalLatitude, decimalLongitude, region, subregion) %>% 
  count(name = "nb_years") %>% 
  ungroup() %>% 
  mutate(int_class = cut(nb_years, 
                         breaks = c(-Inf, 1, 5, 10, 15, Inf),
                         labels = c("1 year", "2-5 years", "6-10 years", "11-15 years", ">15 years")),
         int_class = as.factor(int_class)) %>% 
  arrange(int_class) %>% 
  select(-nb_years) %>% 
  st_as_sf(coords = c("decimalLongitude", "decimalLatitude"), crs = 4326)

# 4. Monitoring map (global) ----

## 4.1 Transform data ----

### 4.1.1 Define parameters to center map on the Pacific ----

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

### 4.1.2 Transform land ----

data_land <- st_crop(x = data_land, 
                     y = st_as_sfc(st_bbox(c(xmin = -180, ymin = -48, xmax = 180, ymax = 48), crs = 4326))) %>%
  st_difference(polygon) %>%
  st_transform(crs = crs_selected)

### 4.1.3 Transform region ----

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

### 4.1.4 Transform benthic sites ----

data_benthic_sites <- data_benthic_sites %>% 
  st_transform(crs = crs_selected)

### 4.1.5 Create the tropics ----

data_tropics <- tibble(tropic = c("Cancer", "Cancer", "Equator", "Equator", "Capricorn", "Capricorn"),
                       lat = c(23.4366, 23.4366, 0, 0, -23.4366, -23.4366),
                       long = c(-180, 180, -180, 180, -180, 180)) %>% 
  st_as_sf(coords = c("long", "lat"), crs = 4326) %>%
  group_by(tropic) %>%
  dplyr::summarize(do_union = FALSE) %>%
  st_cast("LINESTRING") %>% 
  st_difference(polygon) %>%
  st_transform(crs = crs_selected)

## 4.2 Make the map ----

ggplot() +
  geom_sf(data = data_tropics, linetype = "dashed", col = "lightgrey") +
  geom_sf(data = data_region, fill = "grey99") +
  geom_sf(data = data_land) +
  geom_sf(data = data_benthic_sites %>% arrange(int_class), aes(color = int_class)) +
  coord_sf(expand = FALSE) +
  scale_color_manual(values = palette_second,
                     breaks = c("1 year", "2-5 years", "6-10 years", "11-15 years", ">15 years"),
                     labels = c("1 year", "2-5 years", "6-10 years", "11-15 years", ">15 years"), 
                     drop = FALSE,
                     name = "Number of years with data") +
  theme_map()+
  theme(axis.text.y = element_text(hjust = 0.5),
        axis.text = element_text(size = 16),
        legend.text = element_text(size = 16),
        legend.title = element_text(size = 17)) +
  guides(color = guide_legend(override.aes = list(size = 4)))
  
ggsave("figs/01_part-1/fig-1.png", dpi = 600, height = 4, width = 9)

# 5. Monitoring map (regions) ----

## 5.1 Load and transform data ----

data_benthic_sites <- data_benthic_sites %>% 
  st_transform(crs = 4326)

data_countries <- read_sf("data/01_maps/01_raw/03_natural-earth/ne_10m_admin_0_countries/ne_10m_admin_0_countries.shp")

data_subregions <- read_sf("data/01_maps/02_clean/04_subregions/gcrmn_subregions.shp")

data_tropics <- tibble(tropic = c("Cancer", "Cancer", "Equator", "Equator", "Capricorn", "Capricorn"),
                       lat = c(23.4366, 23.4366, 0, 0, -23.4366, -23.4366),
                       long = c(-180, 180, -180, 180, -180, 180)) %>% 
  st_as_sf(coords = c("long", "lat"), crs = 4326) %>%
  group_by(tropic) %>%
  dplyr::summarize(do_union = FALSE) %>%
  st_cast("LINESTRING")

sf_use_s2(FALSE)

data_subregions <- st_difference(data_subregions, st_union(data_countries))

data_tropics <- st_difference(data_tropics, st_union(data_countries))

## 5.2 Make the map ----

map(unique(data_subregions$region), ~map_region_monitoring(region_i = .))

## 5.3 Export the legend ----

ggplot(data = tibble(color = palette_second,
                     label = c("1 year", "2-5 years", "6-10 years", "11-15 years", ">15 years"),
                     y = seq(1, 5, 1)),
       aes(y = y)) +
  geom_point(aes(x = 1, color = color), size = 8) +
  geom_text(aes(x = 2, label = label), hjust = 0, size = 6) +
  scale_color_identity() +
  lims(x = c(0, 5), y = c(0.5, 5.5)) +
  theme(panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA),
        panel.grid = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank())

ggsave("figs/02_part-2/fig-2/legend.png", width = 2.5, height = 3, bg = "transparent")

# 6. Monitoring descriptors ----

## 6.1 Global ----

data_benthic %>% 
  group_by(region) %>% 
  data_descriptors() %>% 
  # Add locale to avoid having uppercase first (reverse PERSGA and Pacific)
  arrange(region, .locale = "en") %>% 
  bind_rows(., data_descriptors(data_benthic) %>% mutate(region = "Global")) %>% 
  write.csv(., file = paste0("figs/01_part-1/tbl-1.csv"),
            row.names = FALSE)

## 6.2 Regional (individual table export) ----

### 6.2.1 Make the function to export the descriptors ----

export_descriptors <- function(gcrmn_region){
  
  data_benthic %>% 
    filter(region == gcrmn_region) %>% 
    group_by(subregion) %>% 
    data_descriptors() %>% 
    ungroup() %>% 
    # Add subregion with no data
    complete(subregion = data_region %>% 
               filter(region == gcrmn_region) %>% 
               select(subregion) %>% 
               st_drop_geometry() %>% 
               distinct() %>% 
               pull(),
             fill = list(nb_sites = 0,
                         nb_surveys = 0,
                         nb_datasets = 0,
                         first_year = NA,
                         last_year = NA,
                         surveys_90_perc = NA)) %>% 
    mutate(region = str_trim(str_remove_all(subregion, "[0-9]"))) %>% 
    bind_rows(., data_benthic %>% 
                filter(region == gcrmn_region) %>% 
                data_descriptors() %>% 
                mutate(subregion = "All")) %>% 
    mutate(across(c(nb_sites, nb_surveys), ~format(.x, big.mark = ",", scientific = FALSE))) %>% 
    write.csv(., file = paste0("figs/02_part-2/tbl-1/",
                               str_replace_all(str_to_lower(gcrmn_region), " ", "-"),
                               ".csv"),
              row.names = FALSE)
  
}

### 6.2.2 Map over the function ----

map(unique(data_region$region), ~export_descriptors(gcrmn_region = .))

## 6.3 Regional (full table export) ----

data_benthic %>% 
  group_by(region, subregion) %>% 
  data_descriptors() %>% 
  ungroup() %>% 
  # Add subregion with no data
  complete(subregion = data_region %>% 
             select(subregion) %>% 
             st_drop_geometry() %>% 
             distinct() %>% 
             pull(),
           fill = list(nb_sites = 0,
                       nb_surveys = 0,
                       nb_datasets = 0,
                       first_year = NA,
                       last_year = NA,
                       surveys_90_perc = NA)) %>% 
             mutate(region = str_trim(str_remove_all(subregion, "[0-9]"))) %>% 
  bind_rows(., data_benthic %>% 
              group_by(region) %>% 
              data_descriptors() %>% 
              mutate(subregion = "All")) %>% 
  bind_rows(., data_benthic %>% 
              data_descriptors() %>% 
              mutate(region = "All", subregion = "All")) %>% 
  mutate(across(c(nb_sites, nb_surveys), ~format(.x, big.mark = ",", scientific = FALSE))) %>% 
  relocate(region, .before = subregion) %>% 
  arrange(region, subregion, .locale = "en") %>% 
  openxlsx::write.xlsx(., file = "figs/05_supp-mat/supp-tbl-4_monitoring.xlsx", rowNames = FALSE)

## 6.4 By region and country ----

data_benthic %>% 
  group_by(region, country) %>% 
  data_descriptors() %>% 
  ungroup() %>% 
  mutate(across(c(nb_sites, nb_surveys), ~format(.x, big.mark = ",", scientific = FALSE))) %>% 
  write.csv(., file = "figs/06_additional/02_data-exploration/monitoring_region-country.csv",
            row.names = FALSE)

## 6.5 By country ----

data_benthic %>% 
  group_by(country) %>% 
  data_descriptors() %>% 
  ungroup() %>% 
  mutate(across(c(nb_sites, nb_surveys), ~format(.x, big.mark = ",", scientific = FALSE))) %>% 
  write.csv(., file = "figs/06_additional/02_data-exploration/monitoring_country.csv",
            row.names = FALSE)

# 7. Number of surveys per year ----

## 7.1 Global ----

data_benthic %>% 
  select(decimalLatitude, decimalLongitude, eventDate, year) %>% 
  st_drop_geometry() %>% 
  distinct() %>% 
  group_by(year) %>% 
  count() %>% 
  ungroup() %>% 
  complete(year, fill = list(n = 0)) %>% 
  mutate(percent = n*100/sum(n)) %>% 
  ggplot(data = ., aes(x = year, y = percent)) +
  geom_bar(stat = "identity", show.legend = FALSE, width = 1,
           color = palette_first[4], fill = palette_first[3]) +
  labs(x = "Year", y = "Surveys (%)") +
  coord_cartesian(clip = "off") +
  theme_graph() +
  scale_x_continuous(expand = c(0, 0), limits = c(1979, 2026))

ggsave("figs/01_part-1/fig-2.png", width = 5, height = 4, dpi = fig_resolution)

## 7.2 Regional ----

### 7.2.1 Create the function ----

plot_surveys_year <- function(gcrmn_region){
  
  data_benthic %>% 
    filter(region == gcrmn_region) %>% 
    select(territory, decimalLatitude, decimalLongitude, eventDate, year) %>% 
    st_drop_geometry() %>% 
    distinct() %>% 
    group_by(year) %>% 
    count() %>% 
    ungroup() %>% 
    complete(year, fill = list(n = 0)) %>% 
    mutate(percent = n*100/sum(n)) %>% 
    ggplot(data = ., aes(x = year, y = percent)) +
    geom_bar(stat = "identity", show.legend = FALSE, width = 1,
             color = palette_first[4], fill = palette_first[3]) +
    labs(x = "Year", y = "Surveys (%)") +
    coord_cartesian(clip = "off") +
    theme_graph() +
    scale_x_continuous(expand = c(0, 0), limits = c(1979, 2026))
  
  ggsave(paste0("figs/06_additional/02_data-exploration/surveys-year_",
                str_replace_all(str_to_lower(gcrmn_region), " ", "-"), ".png"),
         width = 5, height = 4, dpi = fig_resolution)
  
}

### 7.2.2 Map over the function ----

map(unique(data_region$region), ~plot_surveys_year(gcrmn_region = .))

## 7.2.3 Subplots ----

plot_i <- data_benthic %>% 
  select(region, decimalLatitude, decimalLongitude, eventDate, year) %>% 
  st_drop_geometry() %>% 
  distinct() %>% 
  group_by(year, region) %>% 
  count() %>% 
  ungroup() %>% 
  group_by(region) %>%
  mutate(percent = n*100/sum(n)) %>% 
  ungroup() %>% 
  ggplot(data = ., aes(x = year, y = percent)) +
  geom_bar(stat = "identity", show.legend = FALSE, width = 1,
           color = palette_first[4], fill = palette_first[3]) +
  labs(x = "Year", y = "Surveys (%)") +
  coord_cartesian(clip = "off") +
  theme_graph() +
  facet_wrap(~region, ncol = 2) +
  scale_x_continuous(expand = c(0, 0), limits = c(1979, 2026))

ggsave("figs/05_supp-mat/surveys-year.png", width = 7, height = 10, dpi = fig_resolution)

# 8. Number of surveys per depth ----

## 8.1 Global ----

data_benthic %>% 
  select(decimalLatitude, decimalLongitude, eventDate, year, verbatimDepth) %>% 
  st_drop_geometry() %>% 
  drop_na(verbatimDepth) %>% 
  distinct() %>% 
  ggplot(data = ., aes(x = verbatimDepth)) +
  geom_histogram(binwidth = 1, aes(y = after_stat(width * density * 100)),
                 color = palette_first[4], fill = palette_first[3]) +
  labs(x = "Depth (m)", y = "Surveys (%)") +
  coord_cartesian(clip = "off") +
  theme_graph() +
  scale_x_continuous(expand = c(0, 0), limits = c(-1, 40))

ggsave("figs/01_part-1/fig-3.png", width = 5, height = 4, dpi = fig_resolution)

## 8.2 Regional ----

### 8.2.1 Create the function ----

plot_surveys_depth <- function(gcrmn_region){
  
  data_benthic %>% 
    filter(region == gcrmn_region) %>% 
    select(territory, decimalLatitude, decimalLongitude, eventDate, year, verbatimDepth) %>% 
    st_drop_geometry() %>% 
    drop_na(verbatimDepth) %>% 
    distinct() %>% 
    ggplot(data = ., aes(x = verbatimDepth)) +
    geom_histogram(binwidth = 1, aes(y = after_stat(width * density * 100)),
                   color = palette_first[4], fill = palette_first[3]) +
    labs(x = "Depth (m)", y = "Surveys (%)") +
    coord_cartesian(clip = "off") +
    theme_graph() +
    scale_x_continuous(expand = c(0, 0), limits = c(-1, 40))
  
  ggsave(paste0("figs/06_additional/02_data-exploration/surveys-depth_",
                str_replace_all(str_to_lower(gcrmn_region), " ", "-"), ".png"),
         width = 5, height = 4, dpi = fig_resolution)
  
}

## 8.2.2 Map over the function ----

map(unique(data_region$region), ~plot_surveys_depth(gcrmn_region = .))

## 8.2.3 Subplots ----

plot_i <- data_benthic %>% 
  select(region, territory, decimalLatitude, decimalLongitude, eventDate, year, verbatimDepth) %>% 
  st_drop_geometry() %>% 
  drop_na(verbatimDepth) %>% 
  distinct() %>% 
  ggplot(data = ., aes(x = verbatimDepth)) +
  geom_histogram(binwidth = 1, aes(y = after_stat(width * density * 100)),
                 color = palette_first[4], fill = palette_first[3]) +
  labs(x = "Depth (m)", y = "Surveys (%)") +
  coord_cartesian(clip = "off") +
  theme_graph() +
  facet_wrap(~region, ncol = 2) +
  scale_x_continuous(expand = c(0, 0), limits = c(-1, 40))

ggsave("figs/05_supp-mat/surveys-depth.png", width = 7, height = 10, dpi = fig_resolution)

# 9. Number of sites per datasetID and year ----

## 9.1 Transform data ----

data_sources <- read_xlsx("C:/Users/jerem/Desktop/Recherche/03_projects/2022-02-10_gcrmndb_benthos/gcrmndb_benthos/data/05_data-sources.xlsx") %>% 
  select(datasetID, rightsHolder) %>% 
  distinct()

data_year_dataset <- data_benthic %>% 
  group_by(datasetID, region, year) %>% 
  data_descriptors() %>% 
  ungroup() %>% 
  select(datasetID, region, year, nb_sites) %>% 
  complete(nesting(datasetID, region),
           year = 1980:2024,
           fill = list(nb_sites = 0)) %>% 
  left_join(., data_sources) %>% 
  mutate(label = paste0("<b>", datasetID,
                        "</b><br><span style = 'font-size:10pt'>(",
                        rightsHolder, ")</span>"))

## 9.2 Create a function to produce the plot ----

plot_year_dataset <- function(region_i){
  
  data_year_dataset_i <- data_year_dataset %>% 
    filter(region == region_i)
  
  nb_datasets_i <- length(unique(data_year_dataset_i$datasetID))
  
  plot_i <- ggplot(data = data_year_dataset_i,
                   aes(x = year, y = label, fill = nb_sites)) +
    geom_tile(color = "white", height = 0.6, linewidth = 0.6) +
    theme_graph() +
    labs(y = NULL, x = "Year") +
    scale_y_discrete(limits = rev) +
    scale_x_continuous(expand = c(0, 0), limits = c(1979, 2025)) +
    theme(legend.title.position = "top",
          legend.title = element_text(size = 10, hjust = 1, face = "bold", color = "#2c3e50"),
          legend.key.width = unit(1.5, "cm"),
          legend.key.height = unit(0.4, "cm"),
          legend.justification = "right",
          axis.text.y = element_markdown())
  
  if(max(data_year_dataset_i$nb_sites) == 1){
    
    plot_i <- plot_i +
      scale_fill_stepsn(breaks = c(0, 1, 2),
                        colors = c("lightgrey", "lightgrey", palette_second[2], palette_second[2]),
                        limits = c(0, 2),
                        values = scales::rescale(c(0, 1, 2)),
                        labels = scales::label_number(accuracy = 1),
                        show.limits = TRUE,
                        right = FALSE,
                        name = "NUMBER OF SITES")
    
  }else if(max(data_year_dataset_i$nb_sites) == 2){
    
    plot_i <- plot_i +
      scale_fill_stepsn(breaks = c(0, 1, 2),
                        colors = c("lightgrey", "lightgrey", palette_second[2], palette_second[2]),
                        limits = c(0, max(data_year_dataset_i$nb_sites)),
                        values = scales::rescale(c(0, 1, 2)),
                        labels = scales::label_number(accuracy = 1),
                        show.limits = TRUE,
                        right = FALSE,
                        name = "NUMBER OF SITES")
    
  }else if(max(data_year_dataset_i$nb_sites) == 3){
    
    plot_i <- plot_i +
      scale_fill_stepsn(breaks = c(0, 1, 2, 3),
                        colors = c("lightgrey", "lightgrey", palette_second[2], palette_second[2], palette_second[4]),
                        limits = c(0, max(data_year_dataset_i$nb_sites)),
                        values = scales::rescale(c(0, 1, 2, 3)),
                        labels = scales::label_number(accuracy = 1),
                        show.limits = TRUE,
                        right = FALSE,
                        name = "NUMBER OF SITES")
    
  }else{
    
    plot_i <- plot_i +
      scale_fill_stepsn(breaks = c(0, round(seq(1, max(data_year_dataset_i$nb_sites), length.out = 6), 0)),
                        colors = c("lightgrey", "lightgrey", palette_second[2], palette_second[2], palette_second[3],
                                   palette_second[4], palette_second[5]),
                        limits = c(0, max(data_year_dataset_i$nb_sites)),
                        values = scales::rescale(c(0, round(seq(1, max(data_year_dataset_i$nb_sites), length.out = 6), 0))),
                        labels = scales::label_number(accuracy = 1),
                        show.limits = TRUE,
                        right = FALSE,
                        name = "NUMBER OF SITES")
    
  }
  
  ggsave(filename = paste0("figs/06_additional/02_data-exploration/nb-sites_year-datasetid_",
                           str_replace_all(str_replace_all(str_to_lower(region_i), " ", "-"), "---", "-"), ".png"),
         plot = plot_i, height = (2 + (3*0.3*nb_datasets_i)), width = 9, dpi = fig_resolution)
  
}

## 9.3 Map over the function ----

map(unique(data_benthic$region), ~plot_year_dataset(region_i = .))

# 10. Number of sites per dataset and year ----

## 10.1 Transform data ----

data_year_dataset <- data_benthic %>% 
  group_by(region, year) %>% 
  data_descriptors() %>% 
  ungroup() %>% 
  select(region, year, nb_sites) %>% 
  complete(nesting(region),
           year = 1980:2024,
           fill = list(nb_sites = 0))

## 10.2 Create a function to produce the plot ----

plot_year_region <- function(region_i){
  
  data_year_dataset_i <- data_year_dataset %>% 
    filter(region == region_i)
  
  plot_i <- ggplot(data = data_year_dataset_i,
                   aes(x = year, y = 1, fill = nb_sites)) +
    geom_tile(color = "white", height = 0.6, linewidth = 0.6) +
    theme_graph() +
    labs(y = NULL, x = "Year") +
    scale_x_continuous(expand = c(0, 0), limits = c(1979, 2025)) +
    theme(legend.title.position = "top",
          legend.title = element_text(size = 10, hjust = 1, face = "bold", color = "#2c3e50"),
          legend.key.width = unit(2, "cm"),
          legend.key.height = unit(0.4, "cm"),
          legend.justification = "right",
          axis.text.y = element_blank(),
          panel.grid = element_blank(),
          axis.ticks.y = element_blank())
  
  if(max(data_year_dataset_i$nb_sites) == 1){
    
    plot_i <- plot_i +
      scale_fill_stepsn(breaks = c(0, 1, 2),
                        colors = c("lightgrey", "lightgrey", palette_second[2], palette_second[2]),
                        limits = c(0, 2),
                        values = scales::rescale(c(0, 1, 2)),
                        labels = scales::label_number(accuracy = 1),
                        show.limits = TRUE,
                        right = FALSE,
                        name = "NUMBER OF SITES")
    
  }else if(max(data_year_dataset_i$nb_sites) == 2){
    
    plot_i <- plot_i +
      scale_fill_stepsn(breaks = c(0, 1, 2),
                        colors = c("lightgrey", "lightgrey", palette_second[2], palette_second[2]),
                        limits = c(0, max(data_year_dataset_i$nb_sites)),
                        values = scales::rescale(c(0, 1, 2)),
                        labels = scales::label_number(accuracy = 1),
                        show.limits = TRUE,
                        right = FALSE,
                        name = "NUMBER OF SITES")
    
  }else if(max(data_year_dataset_i$nb_sites) == 3){
    
    plot_i <- plot_i +
      scale_fill_stepsn(breaks = c(0, 1, 2, 3),
                        colors = c("lightgrey", "lightgrey", palette_second[2], palette_second[2], palette_second[4]),
                        limits = c(0, max(data_year_dataset_i$nb_sites)),
                        values = scales::rescale(c(0, 1, 2, 3)),
                        labels = scales::label_number(accuracy = 1),
                        show.limits = TRUE,
                        right = FALSE,
                        name = "NUMBER OF SITES")
    
  }else{
    
    plot_i <- plot_i +
      scale_fill_stepsn(breaks = c(0, round(seq(1, max(data_year_dataset_i$nb_sites), length.out = 6), 0)),
                        colors = c("lightgrey", "lightgrey", palette_second[2], palette_second[2], palette_second[3],
                                   palette_second[4], palette_second[5]),
                        limits = c(0, max(data_year_dataset_i$nb_sites)),
                        values = scales::rescale(c(0, round(seq(1, max(data_year_dataset_i$nb_sites), length.out = 6), 0))),
                        labels = scales::label_number(accuracy = 1),
                        show.limits = TRUE,
                        right = FALSE,
                        name = "NUMBER OF SITES")
    
  }
  
  ggsave(filename = paste0("figs/02_part-2/fig-6/",
                           str_replace_all(str_replace_all(str_to_lower(region_i), " ", "-"), "---", "-"), ".png"),
         plot = plot_i, height = 3, width = 9, dpi = fig_resolution)
  
}

## 10.3 Map over the function ----

map(unique(data_benthic$region), ~plot_year_region(region_i = .))

# 11. Number of sites per number of monitoring years ----

## 11.1 Transform data ----

load("data/02_misc/data-benthic.RData")

data_benthic_sites <- data_benthic %>% 
  select(decimalLatitude, decimalLongitude, region, year) %>% 
  distinct() %>% 
  group_by(decimalLatitude, decimalLongitude, region) %>% 
  count(name = "nb_years") %>% 
  ungroup() %>% 
  mutate(int_class = cut(nb_years,
                         breaks = c(-Inf, 1, 5, 10, 15, Inf),
                         labels = c("1 year", "2-5 years", "6-10 years", "11-15 years", ">15 years")),
                   int_class = as.factor(int_class)) %>% 
  group_by(int_class, region) %>% 
  count() %>% 
  group_by(region) %>% 
  mutate(perc = (n*100)/sum(n))

## 11.2 Make the plot ----

ggplot(data = data_benthic_sites, aes(x = int_class, y = perc, fill = int_class,
                                      label = paste0(round(perc, 1), "%\n(n = ", n, ")"))) +
  geom_bar(stat = "identity", width = 0.5, show.legend = FALSE) +
  geom_text(vjust = -0.3, size = 2.5, family = font_choose_graph) +
  scale_fill_manual(values = palette_second) +
  labs(y = "Percentage of sites", x = "Number of years surveyed") +
  coord_cartesian(clip = "off") +
  theme_graph() +
  theme(axis.text.x = element_text(size = 8, angle = 45)) +
  facet_wrap(~region, ncol = 2) +
  lims(y = c(0, 100))

ggsave("figs/05_supp-mat/surveys-per-category.png", width = 7, height = 10, dpi = fig_resolution)
