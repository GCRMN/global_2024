# 1. Load packages ----

library(tidyverse)
library(sf)
sf_use_s2(FALSE)
library(magrittr) # To use the pipe %<>%

# 2. Source functions ----

source("code/function/graphical_par.R")
source("code/function/data_descriptors.R")
source("code/function/theme_map.R")

# 3. Load data ----

data_land <- st_read("data/01_maps/03_land/ne_10m_admin_0_countries/ne_10m_admin_0_countries.shp")

data_region <- st_read("data/01_maps/05_subregions/gcrmn_subregions.shp")

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
      geom_sf(data = data_benthic_sites_i,
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
    filter(region == gcrmn_region)
  
  plot_i <- ggplot() +
    geom_sf(data = data_region, fill = NA, color = "grey") +
    geom_sf(data = data_benthic_sites_i,
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

## 6.1 Make the function to export the descriptors ----

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

## 6.2 Map over the function ----

map(unique(data_region$region), ~export_descriptors(gcrmn_region = .))
