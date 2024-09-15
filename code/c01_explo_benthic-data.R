# 1. Load packages ----

library(tidyverse)
library(sf)
sf_use_s2(FALSE)
library(ggtext)

# 2. Source functions ----

source("code/function/graphical_par.R")
source("code/function/theme_graph.R")

# 3. Load data ----

data_region <- st_read("data/01_maps/02_clean/04_subregions/gcrmn_subregions.shp")

load("data/02_misc/data-benthic.RData")

# 4. Monitoring duration ----

## 4.1 By region ----

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

data_benthic_sites %>% 
  st_drop_geometry() %>% 
  group_by(region, interval_class) %>% 
  count() %>% 
  ungroup() %>% 
  bind_rows(., data_benthic_sites %>% 
              st_drop_geometry() %>% 
              mutate(region = "Global") %>% 
              group_by(region, interval_class) %>% 
              count() %>% 
              ungroup()) %>% 
  complete(region = data_region %>% 
             st_drop_geometry() %>% 
             distinct() %>% 
             pull(),
           interval_class = c("1 year", "2-5 years", "6-10 years", "11-15 years", ">15 years"),
           fill = list(n = 0)) %>% 
  complete(region, interval_class, fill = list(n = 0)) %>% 
  group_by(region) %>% 
  mutate(total = sum(n)) %>% 
  ungroup() %>% 
  mutate(interval_class = factor(interval_class,
                                 c("1 year", "2-5 years", "6-10 years", "11-15 years", ">15 years")),
         perc = (n*100)/total,
         y_label = ifelse(str_detect(region, "Global"),
                          paste0("**", region, "**<br/><span style='color:#6c7a89'>(_n_ = ",
                                 format(total, big.mark = ",", scientific = FALSE), ")</span>"),
                          paste0(region, "<br/><span style='color:#6c7a89'>(_n_ = ",
                                 format(total, big.mark = ",", scientific = FALSE), ")</span>")),
         perc_label = round(perc, 0),
         perc_label = if_else(perc_label < 8, "", paste0(as.character(perc_label), "%")),
         text_color = ifelse(interval_class %in% c("11-15 years", ">15 years"), "white", "black")) %>% 
  ggplot(data = ., aes(x = y_label, y = perc, fill = interval_class, label = perc_label, color = text_color)) +
  geom_bar(stat = "identity", position = position_stack(reverse = TRUE), width = 0.7, color = NA) +
  geom_text(position = position_stack(vjust = 0.5, reverse = TRUE),
            family = font_choose_graph, size = 4) + 
  coord_flip() +
  theme_graph() +
  labs(x = NULL, y = "Percentage of sites") +
  scale_color_identity() +
  scale_fill_manual(values = palette_second,
                    breaks = c("1 year", "2-5 years", "6-10 years", "11-15 years", ">15 years"),
                    labels = c("1 year", "2-5 years", "6-10 years", "11-15 years", ">15 years"), 
                    drop = FALSE,
                    name = "Number of years with data") +
  theme(legend.title.position = "top",
        legend.title = element_text(hjust = 0.5),
        axis.text = element_markdown()) +
  scale_x_discrete(limits = rev)

ggsave(paste0("figs/06_additional/01_benthic-data_explo/monitoring-duration_global.png"),
       width = 8, height = 9, dpi = fig_resolution)

## 4.2 By subregion ----

### 4.2.1 Create the function ----

plot_sites_interval <- function(gcrmn_region){
  
  data_benthic_sites %>% 
    filter(region == gcrmn_region) %>% 
    st_drop_geometry() %>% 
    group_by(region, subregion, interval_class) %>% 
    count() %>% 
    ungroup() %>% 
    bind_rows(., data_benthic_sites %>% 
                filter(region == gcrmn_region) %>% 
                st_drop_geometry() %>% 
                mutate(subregion = paste0(region, " (region)")) %>% 
                group_by(region, subregion, interval_class) %>% 
                count() %>% 
                ungroup()) %>% 
    complete(subregion = data_region %>% 
               st_drop_geometry() %>% 
               filter(region == gcrmn_region) %>% 
               select(subregion) %>% 
               distinct() %>% 
               pull(), 
             region = data_region %>% 
               st_drop_geometry() %>% 
               filter(region == gcrmn_region) %>% 
               distinct() %>% 
               pull(),
             interval_class = c("1 year", "2-5 years", "6-10 years", "11-15 years", ">15 years"),
             fill = list(n = 0)) %>% 
    complete(subregion, region, interval_class, fill = list(n = 0)) %>% 
    group_by(region, subregion) %>% 
    mutate(total = sum(n)) %>% 
    ungroup() %>% 
    mutate(interval_class = factor(interval_class,
                                   c("1 year", "2-5 years", "6-10 years", "11-15 years", ">15 years")),
           perc = (n*100)/total,
           y_label = ifelse(str_detect(subregion, "region"), 
                            paste0("**", subregion, "**<br/><span style='color:#6c7a89'>(_n_ = ",
                                   format(total, big.mark = ",", scientific = FALSE), ")</span>"),
                            paste0(subregion, "<br/><span style='color:#6c7a89'>(_n_ = ",
                                   format(total, big.mark = ",", scientific = FALSE), ")</span>")),
           perc_label = round(perc, 0),
           perc_label = if_else(perc_label < 8, "", paste0(as.character(perc_label), "%")),
           text_color = ifelse(interval_class %in% c("11-15 years", ">15 years"), "white", "black")) %>% 
    ggplot(data = ., aes(x = y_label, y = perc, fill = interval_class, label = perc_label, color = text_color)) +
    geom_bar(stat = "identity", position = position_stack(reverse = TRUE), width = 0.7, color = NA) +
    geom_text(position = position_stack(vjust = 0.5, reverse = TRUE),
              family = font_choose_graph, size = 4) + 
    coord_flip() +
    theme_graph() +
    labs(x = NULL, y = "Percentage of sites") +
    scale_color_identity() +
    scale_fill_manual(values = palette_second,
                      breaks = c("1 year", "2-5 years", "6-10 years", "11-15 years", ">15 years"),
                      labels = c("1 year", "2-5 years", "6-10 years", "11-15 years", ">15 years"), 
                      drop = FALSE,
                      name = "Number of years with data") +
    theme(legend.title.position = "top",
          legend.title = element_text(hjust = 0.5),
          axis.text = element_markdown()) +
    scale_x_discrete(limits = rev)
  
  ggsave(paste0("figs/06_additional/01_benthic-data_explo/monitoring-duration_",
                str_replace_all(str_to_lower(gcrmn_region), " ", "-"), ".png"),
         width = 8, height = 8, dpi = fig_resolution)
  
}

### 4.2.2 Map over the function ----

map(unique(data_region$region), ~plot_sites_interval(gcrmn_region = .))

# 5. Surveys per method ----

## 5.1 Transform data ----

### 5.1.1 Per subregion ----

data_benthic_method_subregion <- data_benthic %>% 
  select(decimalLatitude, decimalLongitude, region, subregion, year, month, day, eventDate, samplingProtocol) %>% 
  distinct() %>% 
  mutate(samplingProtocol = replace_na(samplingProtocol, "Unknown"),
         samplingProtocol = str_split_fixed(samplingProtocol, ",", 2)[,1],
         samplingProtocol = str_replace_all(samplingProtocol, c("Point intersect transect" = "PIT",
                                                                "Line intersect transect" = "LIT"))) %>% 
  distinct() %>% 
  group_by(region, subregion, samplingProtocol) %>% 
  count() %>% 
  ungroup() %>% 
  complete(subregion = data_region %>% 
             st_drop_geometry() %>% 
             select(subregion) %>% 
             distinct() %>% 
             pull(),
           samplingProtocol,
           fill = list(n = 0)) %>%
  group_by(region, subregion) %>% 
  mutate(total = sum(n),
         region = str_squish(str_remove_all(subregion, "[0-9]"))) %>% 
  ungroup()

### 5.1.2 Per region ----

data_benthic_method_region <- data_benthic %>% 
  select(decimalLatitude, decimalLongitude, region, year, month, day, eventDate, samplingProtocol) %>% 
  distinct() %>% 
  mutate(samplingProtocol = replace_na(samplingProtocol, "Unknown"),
         samplingProtocol = str_split_fixed(samplingProtocol, ",", 2)[,1],
         samplingProtocol = str_replace_all(samplingProtocol, c("Point intersect transect" = "PIT",
                                                                "Line intersect transect" = "LIT"))) %>% 
  distinct() %>% 
  group_by(region, samplingProtocol) %>% 
  count() %>% 
  ungroup() %>% 
  complete(region, samplingProtocol, fill = list(n = 0)) %>% 
  group_by(region) %>% 
  mutate(total = sum(n),
         subregion = "All") %>% 
  ungroup()

### 5.1.3 Global ----

data_benthic_method_global <- data_benthic %>% 
  select(decimalLatitude, decimalLongitude, year, month, day, eventDate, samplingProtocol) %>% 
  distinct() %>% 
  mutate(samplingProtocol = replace_na(samplingProtocol, "Unknown"),
         samplingProtocol = str_split_fixed(samplingProtocol, ",", 2)[,1],
         samplingProtocol = str_replace_all(samplingProtocol, c("Point intersect transect" = "PIT",
                                                                "Line intersect transect" = "LIT"))) %>% 
  distinct() %>% 
  group_by(samplingProtocol) %>% 
  count() %>% 
  ungroup() %>% 
  complete(samplingProtocol, fill = list(n = 0)) %>% 
  mutate(total = sum(n),
         region = "Global",
         subregion = "All")

### 5.1.4 Combine ----
  
data_method <- bind_rows(data_benthic_method_subregion, data_benthic_method_region,
                         data_benthic_method_global) %>% 
    mutate(perc = (n*100)/total,
           perc_label = round(perc, 0),
           perc_label = if_else(perc_label < 8, "", paste0(as.character(perc_label), "%")))

rm(data_benthic_method_global, data_benthic_method_region, data_benthic_method_subregion)

## 5.2 Plots -----

### 5.2.1 Per region ----

data_method %>% 
  filter(subregion == "All") %>% 
  mutate(text_color = ifelse(samplingProtocol %in% c("Video transect", "LIT"), "white", "black")) %>% 
  ggplot(data = ., aes(x = region, y = perc, fill = samplingProtocol, label = perc_label, color = text_color)) +
    geom_bar(stat = "identity", position = position_stack(reverse = TRUE), width = 0.7, color = NA) +
    geom_text(position = position_stack(vjust = 0.5, reverse = TRUE),
              family = font_choose_graph, size = 4) + 
    coord_flip() +
    theme_graph() +
    labs(x = NULL, y = "Percentage of surveys") +
    scale_color_identity() +
    scale_fill_manual(values = c(palette_second[1:4], "grey"),
                      breaks = c("Photo-quadrat", "PIT", "LIT", "Video transect", "Unknown"),
                      labels = c("Photo-quadrat", "PIT", "LIT", "Video transect", "Unknown"), 
                      drop = FALSE,
                      name = "Monitoring method") +
    theme(legend.title.position = "top",
          legend.title = element_text(hjust = 0.5),
          axis.text = element_markdown()) +
    scale_x_discrete(limits = rev)

ggsave(paste0("figs/06_additional/01_benthic-data_explo/monitoring-method_global.png"),
       width = 8, height = 9, dpi = fig_resolution)

### 5.2.2 Per subregion ----

plot_survey_method <- function(gcrmn_region){
  
  data_method %>% 
    filter(region == gcrmn_region) %>% 
    mutate(text_color = ifelse(samplingProtocol %in% c("Video transect", "LIT"), "white", "black")) %>% 
    ggplot(data = ., aes(x = subregion, y = perc, fill = samplingProtocol, label = perc_label, color = text_color)) +
    geom_bar(stat = "identity", position = position_stack(reverse = TRUE), width = 0.7, color = NA) +
    geom_text(position = position_stack(vjust = 0.5, reverse = TRUE),
              family = font_choose_graph, size = 4) + 
    coord_flip() +
    theme_graph() +
    labs(x = NULL, y = "Percentage of surveys") +
    scale_color_identity() +
    scale_fill_manual(values = c(palette_second[1:4], "grey"),
                      breaks = c("Photo-quadrat", "PIT", "LIT", "Video transect", "Unknown"),
                      labels = c("Photo-quadrat", "PIT", "LIT", "Video transect", "Unknown"), 
                      drop = FALSE,
                      name = "Monitoring method") +
    theme(legend.title.position = "top",
          legend.title = element_text(hjust = 0.5),
          axis.text = element_markdown()) +
    scale_x_discrete(limits = rev)
  
  ggsave(paste0("figs/06_additional/01_benthic-data_explo/monitoring-method_",
                str_replace_all(str_to_lower(gcrmn_region), " ", "-"), ".png"),
         width = 8, height = 8, dpi = fig_resolution)
   
}
  
map(unique(data_region$region), ~plot_survey_method(gcrmn_region = .))

# 6. Taxonomic level ----

## 6.1 Transform data ----

### 6.1.2 Per subregion ----

data_taxo_subregion <- data_benthic %>% 
  group_by(region, subregion) %>% 
  mutate(tax_lvl = case_when(!(is.na(scientificName)) ~ "Species",
                             !(is.na(genus)) ~ "Genus",
                             !(is.na(family)) ~ "Family",
                             !(is.na(order)) ~ "Order",
                             !(is.na(class)) ~ "Class",
                             !(is.na(phylum)) ~ "Phylum",
                             !(is.na(subcategory)) ~ "Subcategory",
                             !(is.na(category)) ~ "Category")) %>% 
  group_by(region, subregion, tax_lvl) %>% 
  count() %>% 
  ungroup() %>% 
  complete(subregion = data_region %>% 
             st_drop_geometry() %>% 
             select(subregion) %>% 
             distinct() %>% 
             pull(),
           tax_lvl,
           fill = list(n = 0)) %>%
  mutate(rel = (n*100)/sum(n),
         tax_lvl = as_factor(tax_lvl),
         tax_lvl = fct_expand(tax_lvl, "Category", "Subcategory", "Phylum", "Class",
                              "Order", "Family", "Genus", "Species"),
         tax_lvl = fct_relevel(tax_lvl, "Species", "Genus", "Family", "Order", "Class",
                               "Phylum", "Subcategory", "Category"))

### 6.1.2 Per region ----

data_taxo_region <- data_benthic %>% 
  group_by(region) %>% 
  mutate(tax_lvl = case_when(!(is.na(scientificName)) ~ "Species",
                             !(is.na(genus)) ~ "Genus",
                             !(is.na(family)) ~ "Family",
                             !(is.na(order)) ~ "Order",
                             !(is.na(class)) ~ "Class",
                             !(is.na(phylum)) ~ "Phylum",
                             !(is.na(subcategory)) ~ "Subcategory",
                             !(is.na(category)) ~ "Category")) %>% 
  group_by(region, tax_lvl) %>% 
  count() %>% 
  ungroup() %>% 
  complete(region = data_region %>% 
             st_drop_geometry() %>% 
             select(region) %>% 
             distinct() %>% 
             pull(),
           tax_lvl,
           fill = list(n = 0)) %>%
  mutate(rel = (n*100)/sum(n),
         tax_lvl = as_factor(tax_lvl),
         tax_lvl = fct_expand(tax_lvl, "Category", "Subcategory", "Phylum", "Class",
                              "Order", "Family", "Genus", "Species"),
         tax_lvl = fct_relevel(tax_lvl, "Species", "Genus", "Family", "Order", "Class",
                               "Phylum", "Subcategory", "Category"))

### 6.1.3 Global ----

data_benthic_method_global <- data_benthic %>% 
  select(decimalLatitude, decimalLongitude, year, month, day, eventDate, samplingProtocol) %>% 
  distinct() %>% 
  mutate(samplingProtocol = replace_na(samplingProtocol, "Unknown"),
         samplingProtocol = str_split_fixed(samplingProtocol, ",", 2)[,1],
         samplingProtocol = str_replace_all(samplingProtocol, c("Point intersect transect" = "PIT",
                                                                "Line intersect transect" = "LIT"))) %>% 
  distinct() %>% 
  group_by(samplingProtocol) %>% 
  count() %>% 
  ungroup() %>% 
  complete(samplingProtocol, fill = list(n = 0)) %>% 
  mutate(total = sum(n),
         region = "Global",
         subregion = "All")

### 6.1.4 Combine ----

data_method <- bind_rows(data_benthic_method_subregion, data_benthic_method_region,
                         data_benthic_method_global) %>% 
  mutate(perc = (n*100)/total,
         perc_label = round(perc, 0),
         perc_label = if_else(perc_label < 8, "", paste0(as.character(perc_label), "%")))

rm(data_benthic_method_global, data_benthic_method_region, data_benthic_method_subregion)











## 5.2 Plots -----

### 5.2.1 Per region ----

data_taxo %>% 
  filter(subregion == "All") %>% 
  mutate(text_color = ifelse(samplingProtocol %in% c("Video transect", "LIT"), "white", "black")) %>% 
  ggplot(data = ., aes(x = region, y = perc, fill = samplingProtocol, label = perc_label, color = text_color)) +
  geom_bar(stat = "identity", position = position_stack(reverse = TRUE), width = 0.7, color = NA) +
  geom_text(position = position_stack(vjust = 0.5, reverse = TRUE),
            family = font_choose_graph, size = 4) + 
  coord_flip() +
  theme_graph() +
  labs(x = NULL, y = "Percentage of surveys") +
  scale_color_identity() +
  scale_fill_manual(values = c(palette_second[1:4], "grey"),
                    breaks = c("Photo-quadrat", "PIT", "LIT", "Video transect", "Unknown"),
                    labels = c("Photo-quadrat", "PIT", "LIT", "Video transect", "Unknown"), 
                    drop = FALSE,
                    name = "Monitoring method") +
  theme(legend.title.position = "top",
        legend.title = element_text(hjust = 0.5),
        axis.text = element_markdown()) +
  scale_x_discrete(limits = rev)





























### 3.2.2 For the entire dataset ----

A <- data_benthic %>% 
  group_by(region) %>% 
  mutate(tax_lvl = case_when(!(is.na(scientificName)) ~ "Species",
                             !(is.na(genus)) ~ "Genus",
                             !(is.na(family)) ~ "Family",
                             !(is.na(order)) ~ "Order",
                             !(is.na(class)) ~ "Class",
                             !(is.na(phylum)) ~ "Phylum",
                             !(is.na(subcategory)) ~ "Subcategory",
                             !(is.na(category)) ~ "Category")) %>% 
  group_by(region, tax_lvl) %>% 
  summarise(abs = n()) %>% 
  ungroup() %>% 
  mutate(rel = (abs*100)/sum(abs),
         tax_lvl = as_factor(tax_lvl),
         tax_lvl = fct_expand(tax_lvl, "Category", "Subcategory", "Phylum", "Class",
                              "Order", "Family", "Genus", "Species"),
         tax_lvl = fct_relevel(tax_lvl, "Species", "Genus", "Family", "Order", "Class",
                               "Phylum", "Subcategory", "Category"))

ggsave("figs/04_supp/01_data-explo/01_taxonomic-levels_all.png", dpi = 600, height = 4, width = 8)



















######################



data_benthic_cover <- data_benthic %>% 
  # 1. Sum of benthic cover per sampling unit (site, transect, quadrat) and category
  mutate(category = case_when(subcategory == "Macroalgae" ~ "Macroalgae",
                              subcategory == "Turf algae" ~ "Turf algae",
                              subcategory == "Coralline algae" ~ "Coralline algae",
                              TRUE ~ category)) %>% 
  filter(category %in% c("Hard coral", "Macroalgae", "Turf algae", "Coralline algae")) %>% 
  group_by(datasetID, region, subregion, ecoregion, country, territory, locality, habitat, parentEventID, eventID,
           decimalLatitude, decimalLongitude, verbatimDepth, year, month, day, eventDate, category) %>% 
  summarise(measurementValue = sum(measurementValue)) %>% 
  ungroup() %>% 
  # 2. Summarise data at the transect level (i.e. mean of photo-quadrats)
  # This avoid getting semi-quantitative data (e.g. when there is only 10 points per photo-quadrat)
  # This is the case for datasets "0011", "0012", "0013", "0014", and "0043" at least
  group_by(datasetID, region, subregion, ecoregion, country, territory, locality, habitat, parentEventID,
           decimalLatitude, decimalLongitude, verbatimDepth, year, month, day, eventDate, category) %>% 
  summarise(measurementValue = mean(measurementValue)) %>% 
  ungroup()# %>% 
  # 3. Remove values greater than 100 (unlikely but included to avoid any issues later)
  #filter(measurementValue <= 100)





plot_survey_cover <- function(gcrmn_region){
  
  data <- data_benthic_cover %>% 
    filter(region == gcrmn_region)
  
  data <- bind_rows(data, data %>% mutate(subregion = "All"))
  
  ggplot(data = data, aes(x = year, y = measurementValue)) +
    geom_point() +
    geom_smooth() +
    facet_grid(subregion~category) +
    theme_graph() + 
    labs(y = "Percentage cover", x = "Year")
  
  
}

plot_survey_cover(gcrmn_region = "ROPME")




plot_survey_cover_year <- function(gcrmn_region){
  
  data <- data_benthic_cover %>% 
    filter(region == gcrmn_region) %>%
    mutate(color = case_when(category == "Hard coral" ~ palette_second[2],
                             category == "Coralline algae" ~ palette_second[3],
                             category == "Macroalgae" ~ palette_second[4],
                             category == "Turf algae" ~ palette_second[5]))
  
  data <- bind_rows(data, data %>% mutate(subregion = "All"))
  
  ggplot(data = data, aes(x = year, y = measurementValue, color = color)) +
    geom_point(alpha = 0.1, color = "lightgrey") +
    scale_color_identity() +
    geom_smooth() +
    facet_grid(subregion~category) +
    theme_graph() + 
    lims(y = c(-2, 100), x = c(1980, 2025)) +
    labs(y = "Percentage cover", x = "Year")
  
  ggsave(paste0("figs/06_additional/01_benthic-data_explo/benthic-cover_year_",
                str_replace_all(str_to_lower(gcrmn_region), " ", "-"), ".png"),
         width = 12, height = 8, dpi = fig_resolution)
  
}

map(unique(data_region$region), ~plot_survey_cover_year(gcrmn_region = .))





plot_survey_cover_density <- function(gcrmn_region){
  
  data <- data_benthic_cover %>% 
    filter(region == gcrmn_region) %>%
    mutate(color = case_when(category == "Hard coral" ~ palette_second[2],
                             category == "Coralline algae" ~ palette_second[3],
                             category == "Macroalgae" ~ palette_second[4],
                             category == "Turf algae" ~ palette_second[5]))
  
  data <- bind_rows(data, data %>% mutate(subregion = "All"))
  
  ggplot(data = data, aes(x = measurementValue, fill = color)) +
    geom_density() +
    scale_fill_identity() +
    facet_grid(subregion~category, drop = FALSE) +
    theme_graph() + 
    lims(x = c(0, 100)) +
    labs(x = "Percentage cover")
  
  ggsave(paste0("figs/06_additional/01_benthic-data_explo/benthic-cover_density_",
                str_replace_all(str_to_lower(gcrmn_region), " ", "-"), ".png"),
         width = 12, height = 8, dpi = fig_resolution)
  
}

map(unique(data_region$region), ~plot_survey_cover_density(gcrmn_region = .))


##########################











%>% 
  ggplot(data = ., aes(x = tax_lvl, y = rel)) +
  geom_bar(stat = "identity", fill = "#446cb3") +
  lims(y = c(0, 100)) +
  scale_x_discrete(drop = FALSE) +
  coord_flip() +
  labs(x = NULL, y = "Percentage of rows")


### 3.2.3 For hards corals ----

data_benthic %>% 
  filter(category == "Hard coral") %>% 
  taxonomic_levels() +
  labs(title = "Hard coral")

ggsave("figs/04_supp/01_data-explo/01_taxonomic-levels_hard-coral.png", dpi = 600, height = 4, width = 8)

### 3.2.4 For algae ----

data_benthic %>% 
  filter(category == "Algae") %>% 
  taxonomic_levels() +
  labs(title = "Algae")




























# 2. Load benthic cover data ----


# 6. Summarize data ----

## 6.1 Major hard coral families ----

data_benthic_hc <- data_benthic %>% 
  # 1. Sum of benthic cover per sampling unit (site, transect, quadrat) and category
  filter(family %in% c("Acroporidae", "Pocilloporidae", "Poritidae")) %>% 
  mutate(category = family) %>% 
  group_by(datasetID, region, subregion, ecoregion, country, territory, locality, habitat, parentEventID,
           decimalLatitude, decimalLongitude, verbatimDepth, year, month, day, eventDate, eventID, category) %>% 
  summarise(measurementValue = sum(measurementValue)) %>% 
  ungroup() %>% 
  # 2. Summarise data at the transect level (i.e. mean of photo-quadrats)
  # This avoid getting semi-quantitative data (e.g. when there is only 10 points per photo-quadrat)
  # This is the case for datasets "0011", "0012", "0013", "0014", and "0043" at least
  group_by(datasetID, region, subregion, ecoregion, country, territory, locality, habitat, parentEventID,
           decimalLatitude, decimalLongitude, verbatimDepth, year, month, day, eventDate, category) %>% 
  summarise(measurementValue = mean(measurementValue)) %>% 
  ungroup() %>% 
  # 3. Remove values greater than 100 (unlikely but included to avoid any issues later)
  filter(measurementValue <= 100) %>% 
  # 4. Remove useless variables
  #select(-higherGeography, -country, -locality, -habitat, -eventDate) %>% 
  # 5. Convert to factors
  mutate_if(is.character, factor)

## 6.2 Major benthic categories ----

data_benthic <- data_benthic %>% 
  # 1. Sum of benthic cover per sampling unit (site, transect, quadrat) and category
  mutate(category = case_when(subcategory == "Macroalgae" ~ "Macroalgae",
                              subcategory == "Turf algae" ~ "Turf algae",
                              subcategory == "Coralline algae" ~ "Coralline algae",
                              TRUE ~ category)) %>% 
  filter(category %in% c("Hard coral", "Macroalgae", "Turf algae", "Coralline algae")) %>% 
  group_by(datasetID, region, subregion, ecoregion, country, territory, locality, habitat, parentEventID,
           decimalLatitude, decimalLongitude, verbatimDepth, year, month, day, eventDate, category) %>% 
  summarise(measurementValue = sum(measurementValue)) %>% 
  ungroup() %>% 
  # 2. Summarise data at the transect level (i.e. mean of photo-quadrats)
  # This avoid getting semi-quantitative data (e.g. when there is only 10 points per photo-quadrat)
  # This is the case for datasets "0011", "0012", "0013", "0014", and "0043" at least
  group_by(datasetID, region, subregion, ecoregion, country, territory, locality, habitat, parentEventID,
           decimalLatitude, decimalLongitude, verbatimDepth, year, month, day, eventDate, category) %>% 
  summarise(measurementValue = mean(measurementValue)) %>% 
  ungroup() %>% 
  # 3. Remove values greater than 100 (unlikely but included to avoid any issues later)
  filter(measurementValue <= 100) %>% 
  # 4. Remove useless variables
  #select(-higherGeography, -country, -locality, -habitat, -eventDate) %>% 
  # 5. Convert to factors
  mutate_if(is.character, factor) %>% 
  # 6. Add site_id and type (to join on step 7)
  #left_join(., data_site_coords_obs) %>% 
  # 7. Bind with major benthic categories
  bind_rows(., data_benthic_hc)

## 6.3 Export the data ----

save(data_benthic, file = "data/11_model-data/data_benthic_prepared.RData")




load("data/11_model-data/data_benthic_prepared.RData")

# 3. Temporal trends ----

data_benthic <- data_benthic %>% 
  mutate(color = case_when(category == "Hard coral" ~ palette_second[2],
                           category == "Coralline algae" ~ palette_second[3],
                           category == "Macroalgae" ~ palette_second[4],
                           category == "Turf algae" ~ palette_second[5],
                           category == "Acroporidae" ~ palette_first[1],
                           category == "Poritidae" ~ palette_first[2],
                           category == "Pocilloporidae" ~ palette_first[3]))

plot_trends <- ggplot(data = data_benthic, aes(x = year, y = measurementValue, color = color, fill = color)) +
    geom_point(show.legend = FALSE, alpha = 0.1) +
    stat_summary(geom = "point", fun = "mean", col = "black", size = 2, shape = 23) +
    scale_color_identity() +
    scale_fill_identity() +
    labs(x = "Year", y = "Cover (%)") +
    coord_cartesian(clip = "off") +
    facet_grid(region~category, scales = "free") +
    theme_graph() +
    theme(strip.text = element_text(hjust = 0.5),
          axis.text = element_text(size = 12),
          strip.background = element_blank(),
          panel.spacing = unit(1, "lines")) +
    scale_x_continuous(limits = c(1985, 2025)) +
    scale_y_continuous(breaks = c(0, 25, 50, 75, 100), limits = c(0, 100))
  
ggsave(filename = paste0("figs/06_additional/01_benthic-data_explo/trends.png"),
       plot = plot_trends, width = 16, height = 12, dpi = fig_resolution)
