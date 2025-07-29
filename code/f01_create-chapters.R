# 1. Load packages ----

library(tidyverse)
library(rmarkdown)
library(sf)
library(googledrive)

# 2. Load functions ----

source("code/function/render_qmd.R")

# 3. Create docx files for each area ----

data_region <- st_read("data/01_maps/02_clean/03_regions/gcrmn_regions.shp") %>% 
  st_drop_geometry() %>% 
  mutate(nb = row_number())

map(data_region$region, ~render_qmd(region_i = ., upload_drive = FALSE))

render_qmd(region_i = "WIO", upload_drive = FALSE)
