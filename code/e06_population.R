# 1. Load packages ----

library(tidyverse)

# 2. Load and transform data ----

data_population <- read.csv("data/02_misc/ind_human-pop_5km_subregion.csv") %>% 
  bind_rows(., read.csv("data/02_misc/ind_human-pop_5km_region.csv")) %>% 
  bind_rows(., read.csv("data/02_misc/ind_human-pop_5km_global.csv")) %>% 
  mutate(across(c("region", "subregion"), ~ifelse(is.na(.x), "All", .x))) %>% 
  distinct() %>% 
  rename(year = date, 
         population = sum) %>% 
  mutate(year = as.numeric(str_sub(year, 1, 4))) %>% 
  pivot_wider(names_from = year, values_from = population, names_prefix = "pop_") %>% 
  mutate(pop_change_abs = pop_2020-pop_2000,
         pop_change_rel = ((pop_2020-pop_2000)/pop_2000)*100,
         pop_change_rel = ifelse(is.nan(pop_change_rel), 0, pop_change_rel)) %>% 
  select(-pop_2005, -pop_2010, -pop_2015) %>% 
  arrange(region)

# 3. Export the full table ----

data_population %>% 
  select(-pop_2000) %>% 
  mutate(across(c(pop_2020, pop_change_abs), ~format(round(.x, 0), big.mark = ",", scientific = FALSE)),
         pop_change_rel = format(round(pop_change_rel, 2))) %>% 
  openxlsx::write.xlsx(., file = "figs/05_supp-mat/supp-tbl-2_population.xlsx")

# 4. Export the table per region ----

## 4.1 Create the function ----

export_descriptors <- function(gcrmn_region){
  
  data_population %>% 
    filter(region == gcrmn_region) %>% 
    select(-region) %>% 
    write.csv(., file = paste0("figs/02_part-2/tbl-2/",
                               str_replace_all(str_to_lower(gcrmn_region), " ", "-"),
                               ".csv"),
              row.names = FALSE)
  
}

## 4.2 Map over the function ----

map(setdiff(unique(data_population$region), "All"), ~export_descriptors(gcrmn_region = .))
