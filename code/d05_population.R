# 1. Load packages ----

library(tidyverse)

source("code/function/graphical_par.R")
source("code/function/theme_graph.R")

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

data_population <- data_population %>% 
  filter(row_number() != 1) %>% 
  bind_rows(., data_population %>% 
              slice(1))

# 3. Export the full table ----

## 3.1 Export as .xlsx ----

data_population %>% 
  select(-pop_2000) %>% 
  mutate(across(c(pop_2020, pop_change_abs), ~format(round(.x, 0), big.mark = ",", scientific = FALSE)),
         pop_change_rel = format(round(pop_change_rel, 2))) %>% 
  openxlsx::write.xlsx(., file = "figs/06_supp-mat/population.xlsx")

## 3.2 Export as .tex ----

data_population_tex <- data_population %>% 
  select(-pop_2000) %>% 
  mutate(across(c(pop_2020, pop_change_abs), ~format(round(.x, 0), big.mark = ",", scientific = FALSE)),
         pop_change_rel = format(round(pop_change_rel, 2)))

writeLines(c(map(1:nrow(data_population_tex), ~c(paste0(data_population_tex[.x,"region"], " & ",
                                                        data_population_tex[.x,"subregion"], " & ",
                                                        data_population_tex[.x,"pop_2020"], " & ",
                                                        data_population_tex[.x,"pop_change_abs"], " & ",
                                                        data_population_tex[.x,"pop_change_rel"],
                                                        case_when(.x == nrow(data_population_tex) ~ "",
                                                                  TRUE ~ "\\\\")))) %>%
               unlist()),
           "figs/06_supp-mat/population.tex")

# 4. Export the table per region ----

write.csv(data_population, file = "figs/08_text-gen/human_population.csv", row.names = FALSE)

# 5. Human population at the global scale ----

read.csv("data/02_misc/ind_human-pop_5km_global.csv") %>% 
  distinct() %>% 
  rename(year = date, 
         population = sum) %>% 
  mutate(year = as.numeric(str_sub(year, 1, 4)),
         population = round(population*1e-06, 0)) %>% 
  ggplot(data = ., aes(x = year, y = population, label = population)) +
  geom_line(color = "#2C5D96") +
  geom_point(color = "white", shape = 21, fill = "#2C5D96", size = 5) +
  geom_label(family = font_choose_graph, vjust = 2, linewidth = 0,
             fill = "#74b9ff", text.color = "black", size = 4, alpha = 0.6) +
  labs(x = "Year", y = "Inhabitants (millions)") +
  theme_graph() +
  lims(y = c(0, 120))

ggsave("figs/02_part-1/fig-X.png", width = 6, height = 5, dpi = fig_resolution, bg = "transparent")
