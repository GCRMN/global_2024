# 1. Load packages ----

library(tidyverse)
library(rmarkdown)
library(sf)
library(googledrive)

# 2. Load the regions to map over ----

data_region <- st_read("data/01_maps/02_clean/03_regions/gcrmn_regions.shp") %>% 
  st_drop_geometry() %>% 
  mutate(nb = row_number())

# 3. Create the function to render the docx documents ----

render_qmd <- function(region_i, upload_drive = FALSE){
  
  require(rmarkdown)
  require(googledrive)
  
  region_name <- str_replace_all(str_replace_all(str_to_lower(region_i), " ", "-"),  "---", "-")
  
  nb_chapter <- data_region %>% filter(region == region_i) %>% select(nb) %>% pull()
  
  file_name <- paste0(str_pad(nb_chapter, width = 2, pad = "0"), "_", region_name, ".docx")
  
  if(file.exists(paste0("doc/", file_name)) == FALSE){
    
    rmarkdown::render(input = "code/function/chapter_docx.Rmd", 
                      output_file = file_name,
                      output_dir = "doc/",
                      quiet = TRUE)
    
  }
  
  if(upload_drive == TRUE){
    
    drive_put(media = paste0("doc/", file_name),
              path = paste0("folder/", file_name))
    
  }
  
}

# 4. Map over the function ----

render_qmd(region_i = "EAS", upload_drive = FALSE)

#map(data_region$region, ~render_qmd(region_i = ., upload_drive = FALSE))
