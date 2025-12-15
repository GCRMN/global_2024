# 1. Load packages ----

library(tidyverse)
library(sf)
sf_use_s2(FALSE)

# 2. Load and transform data ----

data_reefs <- st_read("data/01_maps/02_clean/01_reefs/global_2024_reefs.shp")

data_reef_extent <- data_reefs %>% 
  group_by(region, subregion) %>% 
  summarise(reef_extent_abs = sum(st_area(geometry))) %>% 
  ungroup() %>% 
  st_drop_geometry() %>% 
  mutate(reef_extent_abs = as.numeric(reef_extent_abs)*1e-6) %>% 
  group_by(region) %>% 
  arrange(-reef_extent_abs) %>% 
  mutate(pos_region = row_number(),
         reef_extent_rel_region = (100*reef_extent_abs)/sum(reef_extent_abs)) %>% 
  ungroup() %>% 
  mutate(reef_extent_rel_world = (100*reef_extent_abs)/sum(reef_extent_abs))

data_reef_extent <- data_reef_extent %>% 
  group_by(region) %>% 
  summarise(reef_extent_abs = sum(reef_extent_abs)) %>% 
  ungroup() %>% 
  mutate(subregion = "All") %>% 
  arrange(-reef_extent_abs) %>% 
  mutate(pos_world = row_number(),
         reef_extent_rel_world = (100*reef_extent_abs)/sum(reef_extent_abs)) %>% 
  bind_rows(data_reef_extent, .) %>% 
  arrange(region, subregion) %>% 
  bind_rows(., data_reef_extent %>%
              summarise(reef_extent_abs = sum(reef_extent_abs)) %>%
              mutate(region = "All", subregion = "All"))

# 3. Export the table for supplementary materials ----

data_reef_extent %>% 
  select(region, subregion, reef_extent_abs, reef_extent_rel_region, reef_extent_rel_world) %>% 
  mutate(reef_extent_abs = format(round(reef_extent_abs, 0), big.mark = ",", scientific = FALSE),
         across(c(reef_extent_rel_region, reef_extent_rel_world), ~format(round(.x, 2)))) %>% 
  openxlsx::write.xlsx(., file = "figs/06_supp-mat/reef-extent.xlsx")

# 4. Export the table with all variables (additional) ----

openxlsx::write.xlsx(data_reef_extent, file = "figs/07_additional/reef_extent.xlsx")

# 5. Export the table per region ----

write.csv(data_reef_extent,
          file = "figs/08_text-gen/reefs_extent.csv",
          row.names = FALSE)
  