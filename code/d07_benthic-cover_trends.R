# 1. Load packages ----

library(tidyverse) # Core tidyverse packages
library(patchwork)
library(glue)
library(ggtext)
library(ggrepel)
library(scales)
library(zoo)
library(openxlsx)

# 2. Source functions ----

source("code/function/graphical_par.R")
source("code/function/theme_graph.R")
source("code/function/plot_trends_model.R")
source("code/function/transform_ribbons.R")

# 3. Load data ----

load("data/model-results.RData")

# 4. Figures for Part 1 ----

## 4.1 Global - Hard coral and macroalgae ----

plot_trends_model(level_i = "global", range = "full", category_i = "Hard coral")

plot_trends_model(level_i = "global", range = "full", category_i = "Macroalgae")

## 4.2 Regional - Hard coral and macroalgae ----

export_subplots <- function(region_i, category_i){
  
  data_i <- data_models %>% 
    group_by(category, level, region, subregion, ecoregion) %>% 
    filter(year >= first_year & year <= last_year) %>% 
    ungroup() %>% 
    filter(category == category_i & level == "region") %>% 
    left_join(., palette_regions)

  plot_i <- ggplot(data = data_i %>% filter(region == region_i)) +
    geom_ribbon(aes(x = year, ymin = lower_ci_95, ymax = upper_ci_95, fill = color), alpha = 0.35) +
    geom_ribbon(aes(x = year, ymin = lower_ci_80, ymax = upper_ci_80, fill = color), alpha = 0.45) +
    geom_line(aes(x = year, y = mean, color = color)) +
    scale_color_identity() +
    scale_fill_identity() +
    scale_x_continuous(breaks = c(1980, 1985, 1990, 1995, 2000, 2005, 2010, 2015, 2020, 2025),
                       limits = c(1979, 2026),
                       labels = c("1980", "", "1990", "", "2000", "", "2010", "", "2020", "")) +
    lims(y = c(0, max(data_i$upper_ci_95))) +
    labs(x = "Year", y = "Cover (%)", title = case_when(region_i == "EAS" ~ "East Asian Seas",
                                                        region_i == "ETP" ~ "Eastern Tropical Pacific",
                                                        region_i == "WIO" ~ "Western Indian Ocean",
                                                        TRUE ~ region_i)) +
    theme_graph() +
    theme(plot.title = element_blank(),
          axis.title.x = element_text(size = 48),
          axis.title.y = element_text(size = 48),
          axis.text = element_text(size = 40),
          plot.background = element_rect(fill = "transparent", color = NA))
  
  if(category_i == "Hard coral"){
    
    ggsave(filename = paste0("figs/02_part-1/fig-4_", str_to_lower(region_i), ".png"), plot = plot_i,
           bg = "transparent", height = 5, width = 6, dpi = 600)
    
  }else{
    
    ggsave(filename = paste0("figs/02_part-1/fig-6_", str_to_lower(region_i), ".png"), plot = plot_i,
           bg = "transparent", height = 5, width = 6, dpi = 600)
    
  }
  
}

map(setdiff(unique(data_models$region), NA),
    ~export_subplots(region_i = .x,
                     category_i = "Hard coral"))

map(setdiff(unique(data_models$region), NA),
    ~export_subplots(region_i = .x,
                     category_i = "Macroalgae"))

## 4.3 Contrasts per region ----

data_contrasts <- readxl::read_xlsx("data/02_misc/contrasts-global.xlsx") %>% 
  mutate(region = str_replace_all(region, c("EAS" = "East Asian<br>Seas",
                                            "ETP" = "Eastern Tropical<br>Pacific",
                                            "WIO" = "Western Indian<br>Ocean")),
         relative_change = case_when(is.na(relative_change) ~ 0,
                                     TRUE ~ relative_change),
         region = case_when(region == "Global" ~ "**Global**",
                            TRUE ~ region),
         region = fct_reorder(region, desc(relative_change)),
         color = case_when(relative_change < 0 & evidence == "Strong evidence" ~ "#d63031",
                           relative_change < 0 & evidence == "Evidence" ~ "#e17055",
                           relative_change < 0 & evidence == "Weak evidence" ~ "#fab1a0",
                           evidence == "No evidence" ~ NA,
                           relative_change > 0 & evidence == "Weak evidence" ~ "#81ecec",
                           relative_change > 0 & evidence == "Evidence" ~ "#74b9ff",
                           relative_change > 0 & evidence == "Strong evidence" ~ "#0984e3"),
         label = case_when(evidence == "No evidence" ~ NA,
                           relative_change < 0 ~ paste0(relative_change, "%"),
                           relative_change > 0 ~ paste0("+", relative_change, "%")),
         color_label = case_when(relative_change < 0 ~ "#d63031",
                           evidence == "No evidence" ~ NA,
                           relative_change > 0 ~ "#0984e3"))

ggplot(data = data_contrasts) +
  geom_bar(aes(x = region, y = relative_change, fill = color), stat = "identity", width = 0.5) +
  geom_hline(yintercept = 0, linewidth = 0.2) +
  scale_fill_identity() +
  geom_label(aes(x = region, y = 0, label = evidence,
                vjust = ifelse(evidence == "No evidence", 0.5, -2.5),
                hjust = case_when(relative_change < 0 ~ 1.05,
                                  relative_change == 0 ~ 0.5,
                                  relative_change > 0 ~ -0.05)),
            color = "#576574", fontface = "italic", size = 3,
            label.padding = unit(0.1, "lines"), border.color = NA) +
  geom_label(aes(x = region, y = relative_change, label = label,
                 color = color_label, hjust = ifelse(relative_change < 0, 1.2, -0.2)),
                 fontface = "bold", size = 5, label.padding = unit(0.1, "lines"), border.color = NA) +
  scale_color_identity() +
  scale_y_continuous(limits = c(-70, 60),
                     breaks = c(-50, -25, 0, 25, 50),
                     labels = c("-50%", "-25%", "0%", "+25%", "+50%")) +
  coord_flip() +
  theme_graph() +
  theme(axis.text.y = element_markdown(size = 16, lineheight = 0.75),
        axis.text.x = element_markdown(size = 16),
        axis.title.x = element_text(size = 20, lineheight = 0.6, vjust = -2),
        plot.background = element_rect(fill = "transparent", color = NA)) +
  labs(x = NULL, y = "Relative change in hard coral cover\nbetween 1980-2009 and 2020-2024")

ggsave(filename = "figs/02_part-1/fig-5.png", bg = "transparent", height = 7, width = 4, dpi = 600)

ggsave(filename = "figs/02_part-1/fig-5.pdf", bg = "transparent", height = 8, width = 5, dpi = 600)

# 5. Figures for Part 2 ----

## 5.1 Trends (regions) ----

map(setdiff(unique(data_models$region), NA),
    ~plot_trends_model(region_i = .x,
                 level_i = "region", range = "obs"))

## 5.2 Trends (subregions) ----

map(setdiff(unique(data_models$region), NA),
    ~plot_trends_model(region_i = .x,
                 level_i = "subregion", category_i = "Hard coral", range = "obs"))

map(setdiff(unique(data_models$region), NA),
    ~plot_trends_model(region_i = .x,
                 level_i = "subregion", category_i = "Macroalgae", range = "obs"))

map(c("Brazil", "ETP"),
    ~plot_trends_model(region_i = .x,
                       level_i = "subregion", category_i = "Turf algae", range = "obs"))

## 5.3 Trends (ecoregions) ----

map(setdiff(unique(data_models$region), NA),
    ~plot_trends_model(region_i = .x,
                 level_i = "ecoregion", category_i = "Hard coral", range = "obs"))

map(setdiff(unique(data_models$region), NA),
    ~plot_trends_model(region_i = .x,
                 level_i = "ecoregion", category_i = "Macroalgae", range = "obs"))

## 5.4 Modeled values per region for writing ----

### 5.4.1 Create the function ----

export_model_data <- function(region_i, range){
  
  metadata <- tibble(variable = c("category", "region", "subregion",
                                  "year", "mean",
                                  "lower_ci_95",
                                  "upper_ci_95",
                                  "lower_ci_80",
                                  "upper_ci_80",
                                  "data_obs"),
                     description = c("Benthic category", "GCRMN region", "GCRMN subregion",
                                     "Year", "Mean modelled percentage cover",
                                     "Percentage cover for the lower 95% credible interval",
                                     "Percentage cover for the upper 95% credible interval",
                                     "Percentage cover for the lower 80% credible interval",
                                     "Percentage cover for the upper 80% credible interval",
                                     "Observed monitoring data available for the year? Yes (TRUE) or No (FALSE)"))
  
  data_region <- data_models %>% 
    filter(level == "region" & region == region_i & category %in% c("Hard coral", "Macroalgae")) %>% 
    group_by(category) %>% 
    { 
      if (range == "obs") {
        filter(., year >= first_year & year <= last_year)
      } else if(range == "full") {
        .
      }
    } %>% 
    ungroup() %>% 
    select(category, region, year, mean, lower_ci_95, upper_ci_95, lower_ci_80, upper_ci_80, data_obs) %>% 
    arrange(category, region, year) %>% 
    mutate(data_obs = as.character(data_obs)) # To avoid conversion to French by openxlsx package
  
  data_subregion <- data_models %>% 
    filter(level == "subregion" & region == region_i & category %in% c("Hard coral", "Macroalgae")) %>% 
    group_by(category, subregion) %>% 
    { 
      if (range == "obs") {
        filter(., year >= first_year & year <= last_year)
      } else if(range == "full") {
        .
      }
    } %>% 
    ungroup() %>% 
    select(category, subregion, year, mean, lower_ci_95, upper_ci_95, lower_ci_80, upper_ci_80, data_obs) %>% 
    arrange(category, subregion, year) %>% 
    mutate(data_obs = as.character(data_obs)) # To avoid conversion to French by openxlsx package
  
  list_of_datasets <- list("metadata" = metadata, "region" = data_region, "subregion" = data_subregion)
  
  write.xlsx(list_of_datasets, file = paste0("figs/07_additional/08_model-values/", 
                                             str_replace_all(str_replace_all(str_to_lower(region_i), " ", "-"), "---", "-"),
                                             ".xlsx"))
  
}

### 5.4.2 Map over the function ----

map(setdiff(unique(data_models$region), NA),
    ~export_model_data(region_i = .x, range = "obs"))
