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

## 3.1 Load data ----

data_2020 <- read.csv2("data/02_misc/03-merge_all_all_all_benthos_NA.csv") %>% 
  filter(Area == "ROPME Area") %>% 
  rename(decimalLatitude = Latitude, decimalLongitude = Longitude,
         year = Year, eventDate = Date, datasetID = DatasetID, country = Country) %>% 
  mutate(month = NA) %>% 
  group_by(country) %>% 
  data_descriptors() %>% 
  ungroup() %>% 
  mutate(report = "2020 GCRMN\nreport")

load("data/02_misc/data-benthic.RData")

data_2025 <- data_benthic %>% 
  filter(region == "ROPME") %>% 
  filter(country != "Yemen") %>% 
  group_by(country) %>% 
  data_descriptors() %>% 
  ungroup() %>% 
  mutate(report = "2025 GCRMN\nreport")

data_all <- bind_rows(data_2020, data_2025) %>% 
  select(country, nb_surveys, report) %>% 
  add_row(country = "Iraq", nb_surveys = 0, report = "2020 GCRMN\nreport") %>% 
  add_row(country = "Iraq", nb_surveys = 0, report = "2025 GCRMN\nreport") %>% 
  add_row(country = "Saudi Arabia", nb_surveys = 0, report = "2020 GCRMN\nreport") %>% 
  mutate(country = str_replace_all(country, "United Arab Emirates", "UAE"),
         color = case_when(country == "Oman" ~ "black",
                           country == "UAE" ~ "#FF0000",
                           country == "Saudi Arabia" ~ "#006C35",
                           country == "Iran" ~ "#239F40",
                           country == "Bahrain" ~ "#CE1126",
                           country == "Kuwait" ~ "black",
                           country == "Qatar" ~ "#8D1B3D",
                           country == "Iraq" ~ "black"))

## 3.2 Make the figure ----

ggplot(data = data_all, aes(x = report, y = nb_surveys, fill = color, group = country, color = color)) +
  geom_line(show.legend = FALSE) +
  geom_point(size = 6, shape = 21, color = "white", show.legend = FALSE) +
  geom_text(data = data_all %>% filter(report == "2025 GCRMN\nreport"),
                  aes(x = report, y = nb_surveys, label = country), show.legend = FALSE) +
  theme_graph() +
  scale_fill_identity() +
  scale_color_identity() +
  scale_x_discrete(expand = c(0.1, 0.5)) +
  lims(y = c(0, 300)) +
  labs(y = "Number of surveys", x = NULL)

ggsave("figs/03_case-studies/ropme-raw-text.png", height = 4, width = 6, bg = "transparent", dpi = 300)

ggplot(data = data_all, aes(x = report, y = nb_surveys, fill = color, group = country, color = color)) +
  geom_line(show.legend = FALSE) +
  geom_point(size = 6, shape = 21, color = "white", show.legend = FALSE) +
  theme_graph() +
  scale_fill_identity() +
  scale_color_identity() +
  scale_x_discrete(expand = c(0.1, 0.5)) +
  lims(y = c(0, 300)) +
  labs(y = "Number of surveys", x = NULL)

ggsave("figs/03_case-studies/ropme-raw.png", height = 4, width = 6, bg = "transparent", dpi = 300)
