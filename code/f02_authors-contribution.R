# 1. Load packages ----

library(tidyverse)
library(readxl)
library(googledrive)
library(openxlsx)

# 2. Create the tibble ----

data_contribution <- read_xlsx("figs/06_additional/05_contributors/contributors_contacts.xlsx") %>% 
  drop_na(first_name) %>% # Remove NA (datasets from paper) 
  select(first_name, last_name, email) %>% 
  distinct() %>% 
  mutate(`Data acquisition` = "X") %>% 
  # Add contributors who are not data contributors
  full_join(., read_xlsx("figs/06_additional/05_contributors/non-data-contributors_to-complete.xlsx")) %>% 
  # Create all the columns
  mutate(`Funding acquisition` = NA,
         `Supervision` = NA,
         `Conceptualization` = NA,
         `Facilitation` = NA,
         `Data integration` = NA,
         `Data analysis` = NA,
         `Participation to workshop` = NA,
         `Executive summary` = NA,
         `Synthesis for the region` = NA,
         `Syntheses for count. and terr.` = NA,
         `Case studies` = NA,
         `Materials and Methods` = NA,
         `Supplementary Materials` = NA,
         `Layout` = NA,
         `Communication` = NA) %>% 
  relocate(`Data acquisition`, .before = `Data integration`) %>% 
  arrange(last_name, .locale = "en") %>% 
  mutate(across(c(first_name, last_name), ~str_squish(.x))) %>% 
  # Concatenate email address
  group_by(first_name, last_name) %>% 
  mutate(email = paste0(email, collapse = ", ")) %>% 
  ungroup() %>% 
  distinct()
  
# 3. Export the file ----

## 3.1 Create the function to export the file with Excel formatting ----

export_contributions <- function(data, path = "figs/00_misc/authors_contribution.xlsx"){

  wb <- createWorkbook()
  
  addWorksheet(wb, "Sheet1")
  
  addStyle(wb, "Sheet1",
           cols = 1,
           rows = seq(from = 2, to = nrow(data_contribution)+1, by = 1),
           style = createStyle(textDecoration = "Bold", halign = "right"))
  
  addStyle(wb, "Sheet1",
           cols = 2,
           rows = seq(from = 2, to = nrow(data_contribution)+1, by = 1),
           style = createStyle(textDecoration = "Bold", halign = "left"))
  
  addStyle(wb, "Sheet1",
           cols = seq(from = 3, to = ncol(data_contribution), by = 1),
           rows = seq(from = 2, to = nrow(data_contribution)+1, by = 1),
           style = createStyle(halign = "center"),
           gridExpand = TRUE)
  
  writeData(wb = wb,
            sheet = "Sheet1",
            x = data,
            headerStyle = createStyle(textDecoration = "Bold", textRotation = 90, halign = "center"))
  
  saveWorkbook(wb, path, overwrite = TRUE)

}

## 3.2 Export the file or update it ----

if(file.exists("figs/00_misc/authors_contribution.xlsx") == FALSE){
  
  export_contributions(data = data_contribution)
  
}else{
  
  data_contribution_before <- read_xlsx("figs/00_misc/authors_contribution.xlsx")
  
  data_contribution <- data_contribution %>% 
    select(first_name, last_name, email, `Data acquisition`) %>% 
    left_join(., data_contribution_before) %>% 
    relocate(`Data acquisition`, .before = `Data integration`)
  
  export_contributions(data = data_contribution)
  
}

# ----------------------------------------------------------------------------------- #
# /!\     FILL THE EXPORTED FILE WITH AUTHOR'S CONTRIBUTIONS BUT:                 /!\ #
# /!\        1) ROWS MUST NOT BE ADDED MANUALLY                                   /!\ #
# /!\        2) THE COLUMN DATA ACQUISITION MUST NOT BE CHANGED                   /!\ #
# /!\        3) MODIFICATIONS MUST BE DONE ON LOCAL FILE NOT ON GOOGLE DRIVE      /!\ #
# ----------------------------------------------------------------------------------- #

# 4. Export to Google Drive (without email) ----

data_contribution <- read_xlsx("figs/00_misc/authors_contribution.xlsx")

data_contribution <- data_contribution %>% 
  select(-email)

export_contributions(data = data_contribution, path = "figs/00_misc/authors_contribution_temp.xlsx")

drive_put(media = "figs/00_misc/authors_contribution_temp.xlsx",
          path = "GCRMN Caribbean report/10_authors-contribution.xlsx")

# Select Google account before running the last line of code

file.remove("figs/00_misc/authors_contribution_temp.xlsx")
