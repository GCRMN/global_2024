# 1. Load packages ----

library(tidyverse)
library(RcppRoll)

# 2. Load and transform SOI data ----

read_table("data/02_misc/nino34.long.anom.data.txt", skip = 1, col_names = FALSE, n_max = 154) %>% 
  rename(year = 1) %>% 
  filter(year >= 1980 & year <= 2025) %>% 
  pivot_longer(2:ncol(.), values_to = "nino", names_to = "month") %>% 
  mutate(month = str_remove_all(month, "X"),
         month = as.numeric(month)-1,
         date = ym(paste(year, month, sep = "-"))) %>% 
  # Mean of SOI value over the last 24 months (i.e. 2 years)
  mutate(enso = roll_mean(x = nino, n = 24, align = "right", fill = NA)) %>% 
  filter(month == "12") %>% 
  select(year, enso) %>% 
  rename(pred_enso = enso) %>% 
  write.csv(., file = "data/08_predictors/pred_enso.csv", row.names = FALSE)
