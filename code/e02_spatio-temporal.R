# 1. Load packages ----

library(tidyverse)
library(sf)
sf_use_s2(FALSE)
library(magrittr) # To use the pipe %<>%
library(ggtext)
library(readxl)

# 2. Source functions ----

source("code/function/graphical_par.R")
source("code/function/data_descriptors.R")
source("code/function/theme_map.R")
source("code/function/theme_graph.R")

# 3. Load data ----

data_land <- st_read("data/01_maps/01_raw/03_natural-earth/ne_10m_admin_0_countries/ne_10m_admin_0_countries.shp")

data_region <- st_read("data/01_maps/02_clean/04_subregions/gcrmn_subregions.shp")

load("data/02_misc/data-benthic.RData")

data_benthic_sites <- data_benthic %>% 
  select(decimalLatitude, decimalLongitude, year, region, subregion) %>% 
  distinct() %>% 
  group_by(decimalLatitude, decimalLongitude, region, subregion) %>% 
  summarise(interval_years = max(year, na.rm = TRUE) - min(year, na.rm = TRUE)) %>%
  ungroup() %>% 
  mutate(interval_class = cut(interval_years, 
                              breaks = c(-Inf, 1, 5, 10, 15, Inf),
                              labels = c("1 year", "2-5 years", "6-10 years", "11-15 years", ">15 years")),
         interval_class = as.factor(interval_class)) %>% 
  st_as_sf(coords = c("decimalLongitude", "decimalLatitude"), crs = 4326)

# 4. Global map ----

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

## 4.2 Make the map (monitoring) ----

ggplot() +
  geom_sf(data = data_tropics, linetype = "dashed", col = "lightgrey") +
  geom_sf(data = data_region, fill = "grey99") +
  geom_sf(data = data_land) +
  geom_sf(data = data_benthic_sites %>% arrange(interval_class), aes(color = interval_class)) +
  coord_sf(expand = FALSE) +
  scale_color_manual(values = palette_second,
                     breaks = c("1 year", "2-5 years", "6-10 years", "11-15 years", ">15 years"),
                     labels = c("1 year", "2-5 years", "6-10 years", "11-15 years", ">15 years"), 
                     drop = FALSE,
                     name = "Number of years with data") +
  guides(color = guide_legend(override.aes = list(size = 3.5))) +
  theme_map()
  
ggsave("figs/01_part-1/fig-1.png", dpi = 600, height = 4.5, width = 12)

## 4.3 Make the map (regions) ----

ggplot() +
  geom_sf(data = data_tropics, linetype = "dashed", col = "lightgrey") +
  geom_sf(data = data_region, aes(fill = region)) +
  geom_sf(data = data_land) +
  coord_sf(expand = FALSE) +
  labs(fill = "GCRMN region") +
  theme_map()

ggsave("figs/01_part-1/fig-0.png", dpi = 600, height = 4.5, width = 12)

rm(data_region_pac, polygon)

# 5. Regional maps ----

## 5.1 Make the function to export the maps ----

plot_region <- function(gcrmn_region){
  
  if(gcrmn_region == "Pacific"){
    
    crs_selected <- "+proj=eqc +lat_ts=0 +lat_0=0 +lon_0=160 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs"
    
    data_region <- data_region %>% 
      filter(region == gcrmn_region) %>% 
      st_transform(crs_selected)
    
    data_region %<>% # Special pipe from magrittr
      st_buffer(10) %>% # To join polygon (remove vertical line)
      nngeo::st_remove_holes(.)
    
    data_bbox <- st_bbox(data_region)
    
    data_benthic_sites_i <- data_benthic_sites %>% 
      filter(region == gcrmn_region) %>% 
      st_transform(crs = crs_selected)
    
    data_land <- data_land %>% 
      st_transform(crs_selected)

    plot_i <- ggplot() +
      geom_sf(data = data_region, fill = NA, color = "grey") +
      geom_sf(data = data_benthic_sites_i %>% arrange(interval_class),
              aes(color = interval_class), show.legend = TRUE) +
      scale_color_manual(values = palette_second,
                         breaks = c("1 year", "2-5 years", "6-10 years", "11-15 years", ">15 years"),
                         labels = c("1 year", "2-5 years", "6-10 years", "11-15 years", ">15 years"), 
                         drop = FALSE,
                         name = "Number of years with data") +
      guides(color = guide_legend(override.aes = list(size = 3.5))) +
      geom_sf(data = data_land) +
      coord_sf(xlim = c(data_bbox$xmin, data_bbox$xmax), ylim = c(data_bbox$ymin, data_bbox$ymax)) +
      theme_map() +
      scale_x_continuous(breaks = c(180, 160, 140, -160, -140, -120))
    
  }else{
  
  data_region <- data_region %>% 
    filter(region == gcrmn_region)
  
  data_bbox <- st_bbox(data_region)
  
  data_benthic_sites_i <- data_benthic_sites %>% 
    filter(region == gcrmn_region) %>% 
    st_transform(crs = 4326)

  plot_i <- ggplot() +
    geom_sf(data = data_region, fill = NA, color = "grey") +
    geom_sf(data = data_benthic_sites_i %>% arrange(interval_class),
            aes(color = interval_class), show.legend = TRUE) +
    scale_color_manual(values = palette_second,
                       breaks = c("1 year", "2-5 years", "6-10 years", "11-15 years", ">15 years"),
                       labels = c("1 year", "2-5 years", "6-10 years", "11-15 years", ">15 years"), 
                       drop = FALSE,
                       name = "Number of years with data") +
    guides(color = guide_legend(override.aes = list(size = 3.5))) +
    geom_sf(data = data_land) +
    coord_sf(xlim = c(data_bbox$xmin, data_bbox$xmax), ylim = c(data_bbox$ymin, data_bbox$ymax)) +
    theme_map()
  
  }
  
  ggsave(paste0("figs/02_part-2/fig-2/", str_replace_all(str_to_lower(gcrmn_region), " ", "-"), ".png"),
         dpi = 600)
  
}

## 5.2 Map over the function ----

map(unique(data_region$region), ~plot_region(gcrmn_region = .))

# 6. Monitoring descriptors ----

## 6.1 By region ----

data_benthic %>% 
  group_by(region) %>% 
  data_descriptors() %>% 
  # Add locale to avoid having uppercase first (reverse PERSGA and Pacific)
  arrange(region, .locale = "en") %>% 
  bind_rows(., data_descriptors(data_benthic) %>% mutate(region = "Global")) %>% 
  write.csv(., file = paste0("figs/01_part-1/tbl-1.csv"),
            row.names = FALSE)

## 6.2 By subregion ----

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
                         last_year = NA)) %>% 
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

## 6.3 By country ----

data_benthic %>% 
  group_by(country) %>% 
  data_descriptors() %>% 
  ungroup() %>% 
  bind_rows(., data_benthic %>% 
              data_descriptors() %>% 
              mutate(country = "All")) %>% 
  mutate(across(c(nb_sites, nb_surveys), ~format(.x, big.mark = ",", scientific = FALSE))) %>% 
  write.csv(., file = "figs/06_additional/monitoring_descriptors_country.csv",
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
  
  ggsave(paste0("figs/02_part-2/fig-3/", str_replace_all(str_to_lower(gcrmn_region), " ", "-"), ".png"),
         width = 5, height = 4, dpi = fig_resolution)
  
}

### 7.2.2 Map over the function ----

map(unique(data_region$region), ~plot_surveys_year(gcrmn_region = .))

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
  
  ggsave(paste0("figs/02_part-2/fig-4/", str_replace_all(str_to_lower(gcrmn_region), " ", "-"), ".png"),
         width = 5, height = 4, dpi = fig_resolution)
  
}

## 8.2.2 Map over the function ----

map(unique(data_region$region), ~plot_surveys_depth(gcrmn_region = .))

# 9. Number of sites per datasetID and year ----

## 9.1 Transform data ----

data_sources <- read_xlsx("C:/Users/jwicquart/Desktop/Recherche/03_projects/2022-02-10_gcrmndb_benthos/gcrmndb_benthos/data/05_data-sources.xlsx") %>% 
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
  
  ggsave(filename = paste0("figs/06_additional/nb-sites_year-datasetid_",
                           str_replace_all(str_replace_all(str_to_lower(region_i), " ", "-"), "---", "-"), ".png"),
         plot = plot_i, height = (2 + (3*0.3*nb_datasets_i)), width = 9, dpi = fig_resolution)
  
}

## 9.3 Map over the function ----

map(unique(data_benthic$region), ~plot_year_dataset(region_i = .))
