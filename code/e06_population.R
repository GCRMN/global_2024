# 1. Load packages ----

library(tidyverse)

# 2. Load and transform data ----

read.csv("data/02_misc/ind_human-pop_5km_subregion.csv") %>% 
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
  arrange(region) %>% 
  openxlsx::write.xlsx(., file = "figs/06_additional/human_population.xlsx")
