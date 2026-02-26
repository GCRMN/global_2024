# 1. Load packages ----

library(tidyverse)
library(ggtext)

# 2. Source functions ----

source("code/function/graphical_par.R")
source("code/function/theme_graph.R")
source("code/function/theme_map.R")
source("code/function/extract_coeff.R")

# 3. Yearly SST anomaly ----

## 3.1 Transform data ----

load("data/02_misc/data_sst.RData")

data_sst_anom <- data_sst %>% 
  filter(subregion == "All") %>% 
  group_by(region) %>% 
  mutate(sst_mean = mean(sst)) %>% 
  ungroup() %>% 
  mutate(year = year(date),
         sst_anom = sst - sst_mean) %>% 
  group_by(region, year) %>% 
  summarise(mean_sst_anom = mean(sst_anom)) %>% 
  ungroup() %>% 
  mutate(color = case_when(mean_sst_anom < 0 ~ "#2c82c9",
                           mean_sst_anom > 0 ~ "#d64541"))

## 3.2 Global ----

plot_i <- ggplot(data = data_sst_anom %>% filter(region == "All")) +
  geom_bar(aes(x = year, y = mean_sst_anom, fill = color), stat = "identity") +
  scale_fill_identity() +
  geom_hline(yintercept = 0) +
  theme(plot.title = element_markdown(size = 17, face = "bold", family = "Open Sans Semibold"),
        plot.subtitle = element_markdown(size = 12)) +
  labs(x = "Year", y = "SST anomaly (°C)") +
  scale_x_continuous(breaks = c(1985, 1990, 1995, 2000, 2005, 2010, 2015, 2020, 2025),
                     limits = c(1984, 2026),
                     labels = c("", "1990", "", "2000", "", "2010", "", "2020", "")) +
  theme_graph() +
  lims(y = c(-1.5, 1.5))

ggsave("figs/02_part-1/fig-10.png", height = 5.3, width = 7.2, dpi = fig_resolution)

## 3.3 Regional ----

plot_ssta <- function(region_i){
  
  plot_i <- ggplot(data = data_sst_anom %>% filter(region == region_i)) +
    geom_bar(aes(x = year, y = mean_sst_anom, fill = color), stat = "identity") +
    scale_fill_identity() +
    geom_hline(yintercept = 0) +
    labs(x = "Year", y = "SST anomaly (°C)") +
    scale_x_continuous(breaks = c(1985, 1990, 1995, 2000, 2005, 2010, 2015, 2020, 2025),
                       limits = c(1984, 2026),
                       labels = c("1985", "1990", "1995", "2000", "2005", "2010", "2015", "2020", "2025")) +
    theme_graph() +
    theme(plot.title = element_markdown(size = 17, face = "bold", family = "Open Sans Semibold"),
          plot.subtitle = element_markdown(size = 12),
          panel.background = element_rect(fill = "transparent", colour = NA),
          plot.background = element_rect(fill = "transparent", colour = NA))

  ggsave(filename = paste0("figs/03_part-2/fig-2/",
                           str_replace_all(str_replace_all(str_to_lower(region_i), " ", "-"), "---", "-"), ".png"),
         plot = plot_i, height = 3.5, width = 9, dpi = fig_resolution)
  
  ggsave(filename = paste0("figs/03_part-2/fig-2/",
                           str_replace_all(str_replace_all(str_to_lower(region_i), " ", "-"), "---", "-"), ".pdf"),
         plot = plot_i, height = 3.5, width = 9, bg = "transparent")
  
}

map(setdiff(unique(data_sst_anom$region), "All"), ~plot_ssta(region_i = .))

# 4. DHW percent ----

load("data/02_misc/data_dhw_freq.RData")

data_dhw_freq <- data_dhw_freq %>% 
  mutate(subregion = case_when(is.na(subregion) ~ subregn,
                               TRUE ~ subregion)) %>% 
  select(-sbrgn_n, -subregn) %>% 
  complete(year, dhw, nesting(region, subregion), fill = list(nb_cells = 0)) %>% 
  # See https://coralreefwatch.noaa.gov/product/5km/index_5km_baa-max-7d.php
  mutate(heatstress = case_when(dhw < 1 ~ "No stress/Watch",
                                dhw >= 1 & dhw < 4 ~ "Warning",
                                dhw >= 4 & dhw < 8 ~ "Alert 1",
                                dhw >= 8 & dhw < 12 ~ "Alert 2",
                                dhw >= 12 & dhw < 16 ~ "Alert 3",
                                dhw >= 16 & dhw < 20 ~ "Alert 4",
                                dhw >= 20 ~ "Alert 5")) %>% 
  group_by(year, region, subregion, heatstress) %>% 
  summarise(nb_cells = sum(nb_cells)) %>% 
  ungroup() %>% 
  group_by(year, region, subregion) %>% 
  mutate(total_nb_cells = sum(nb_cells)) %>% 
  ungroup() %>% 
  mutate(freq = (nb_cells*100)/total_nb_cells,
         heatstress = as.factor(heatstress),
         heatstress = factor(heatstress, levels = c("No stress/Watch", "Warning",
                                                    "Alert 1", "Alert 2", "Alert 3", "Alert 4", "Alert 5")))

## 4.1 Global ----

plot_i <- ggplot(data = data_dhw_freq %>% filter(region == "All")) +
  geom_bar(aes(x = year, y = freq, fill = heatstress), stat = "identity") +
  scale_fill_manual(values = c("No stress/Watch" = "lightgrey",
                               "Warning" = palette_second[1],
                               "Alert 1" = palette_second[2],
                               "Alert 2" = palette_second[3],
                               "Alert 3" = palette_second[4],
                               "Alert 4" = palette_second[5],
                               "Alert 5" = "black"),
                    name = "Heat stress level") +
  scale_x_continuous(breaks = c(1990, 1995, 2000, 2005, 2010, 2015, 2020, 2025),
                     limits = c(1985, 2026),
                     labels = c("1990", "1995", "2000", "2005", "2010", "2015", "2020", "2025")) +
  theme_graph() +
  theme(legend.title.position = "top", legend.title = element_text(hjust = 0.5)) +
  labs(x = "Year", y = "Percentage of coral reefs")

ggsave("figs/02_part-1/fig-11.png", height = 5.3, width = 7.2, dpi = fig_resolution)

## 4.2 Regional ----

plot_dhw <- function(region_i){
  
  # Create and export the plot 
  
  plot_i <- ggplot(data = data_dhw_freq %>% filter(region == region_i & subregion == "All")) +
    geom_bar(aes(x = year, y = freq, fill = heatstress), stat = "identity") +
    scale_fill_manual(values = c("No stress/Watch" = "lightgrey",
                                 "Warning" = palette_second[1],
                                 "Alert 1" = palette_second[2],
                                 "Alert 2" = palette_second[3],
                                 "Alert 3" = palette_second[4],
                                 "Alert 4" = palette_second[5],
                                 "Alert 5" = "black"),
                      name = "Heat stress level") +
    scale_x_continuous(breaks = c(1990, 1995, 2000, 2005, 2010, 2015, 2020, 2025),
                       limits = c(1985, 2026),
                       labels = c("1990", "1995", "2000", "2005", "2010", "2015", "2020", "2025")) +
    theme_graph() +
    theme(legend.position = "right",
          legend.direction = "vertical",
          legend.title = element_text(hjust = 0.5),
          panel.background = element_rect(fill = "transparent", colour = NA),
          plot.background = element_rect(fill = "transparent", colour = NA),
          legend.background = element_rect(fill = "transparent", colour = NA)) +
    labs(x = "Year", y = "Percentage of coral reefs")
  
  ggsave(filename = paste0("figs/03_part-2/fig-3/",
                           str_replace_all(str_replace_all(str_to_lower(region_i), " ", "-"), "---", "-"), ".png"),
         plot = plot_i, height = 4, width = 10, dpi = fig_resolution)
  
  ggsave(filename = paste0("figs/03_part-2/fig-3/",
                           str_replace_all(str_replace_all(str_to_lower(region_i), " ", "-"), "---", "-"), ".pdf"),
         plot = plot_i, height = 4, width = 10)
  
  # Export the data
  
  data_dhw_freq %>% 
    filter(region == region_i) %>%
    select(-nb_cells, -total_nb_cells, -region) %>%
    mutate(freq = round(freq, 2)) %>%
    pivot_wider(names_from = "heatstress", values_from = "freq") %>% 
    select("subregion", "year", "No stress/Watch", "Warning",
           "Alert 1", "Alert 2", "Alert 3", "Alert 4", "Alert 5") %>% 
    arrange(subregion, year) %>% 
    openxlsx::write.xlsx(., paste0("figs/07_additional/06_threats/data-heatstress_",
                                   str_replace_all(str_replace_all(str_to_lower(region_i), " ", "-"),
                                                   "---", "-"), ".xlsx"))
  
}

map(setdiff(unique(data_dhw_freq$region), "All"), ~plot_dhw(region_i = .))

# 5. Daily SST ----

load("data/02_misc/data_sst.RData")

plot_i <- data_sst %>% 
  mutate(date = as_date(date)) %>% 
  filter(subregion == "All" & region != "All") %>% 
  ggplot(data = ., aes(x = date, y = sst)) +
  geom_line() +
  labs(x = "Year", y = "Sea Surface Temperature (°C)") +
  coord_cartesian(clip = "off") +
  theme_graph() +
  facet_wrap(~region, ncol = 2, scales = "free")

#ggsave("figs/06_supp-mat/sst.png", width = 7, height = 10, dpi = fig_resolution)

# 6. Long-term SST average and trend ----

## 6.1 Create the function ---- 

gls_trends <- function(data){
  
  require(nlme)
  
  data_i <- data %>%  
    arrange(date) %>% 
    mutate(t_year = as.numeric(format(date, "%Y")) + ((as.numeric(format(date, "%j"))-1)/365.25),
           t_year = t_year - min(t_year),
           month = factor(format(date, "%m")),
           t_id = row_number(date))
  
  gls_model <- gls(
    model = sst ~ t_year + month,
    data = data_i,
    correlation = corAR1(form = ~ t_id))
  
  results <- data %>% 
    mutate(intercept = as.numeric(gls_model$coefficients["(Intercept)"]),
           slope = as.numeric(gls_model$coefficients["t_year"]),
           slope_se = summary(gls_model)$tTable["t_year", "Std.Error"])

  return(results)
  
}

## 6.2 Map over the function ---- 

load("data/02_misc/data_sst.RData")

data_sst_trends <- data_sst %>% 
  group_by(region, subregion) %>% 
  group_modify(~gls_trends(.x)) %>% 
  ungroup() %>% 
  mutate(warming_rate = slope*10,
         sst_increase = slope*((as.numeric(format(max(date), "%Y")))-(as.numeric(format(min(date), "%Y"))))) %>% 
  group_by(region, subregion) %>% 
  mutate(mean_sst = mean(sst, na.rm = TRUE)) %>% 
  ungroup() %>% 
  select(region, subregion, sst_increase, warming_rate, mean_sst) %>% 
  distinct() %>% 
  mutate(across(c(sst_increase, mean_sst), ~format(round(.x, 2))),
         warming_rate = format(round(warming_rate, 3)))

## 6.3 Export the full table as .xlsx ----

openxlsx::write.xlsx(data_sst_trends, file = "figs/06_supp-mat/sst.xlsx")

## 6.4 Export as .tex ----

writeLines(c(map(1:nrow(data_sst_trends), ~c(paste0(data_sst_trends[.x,"region"], " & ",
                                                    data_sst_trends[.x,"subregion"], " & ",
                                                    data_sst_trends[.x,"sst_increase"], " & ",
                                                    data_sst_trends[.x,"warming_rate"], " & ",
                                                    data_sst_trends[.x,"mean_sst"],
                                                    case_when(.x == nrow(data_sst_trends) ~ "",
                                                              TRUE ~ "\\\\")))) %>%
               unlist()),
           "figs/06_supp-mat/sst.tex")

## 6.5 Export the table per region ----

write.csv(data_sst_trends, file = "figs/08_text-gen/thermal_regime.csv", row.names = FALSE)
