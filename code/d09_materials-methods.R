# 1. Load packages ----

library(tidyverse)
library(sf)
sf_use_s2(FALSE)
library(patchwork)

# 2. Source functions ----

source("code/function/graphical_par.R")
source("code/function/theme_map.R")

# 3. Figure voronoi ----

load(file = "data/02_misc/voronoi_figure_data.RData")

plot_a <- ggplot() +
  geom_sf(data = reef_voronoi_areas, color = "grey") +
  geom_sf(data = sampled_grid.10, aes(color = Weight)) +
  scale_color_gradient(name = "Grid cell\nweights", low = "#82ccdd", high = "#084594", limits = c(0, NA)) +
  geom_sf(data = voronois, linewidth = 0.5, fill = NA) +
  geom_sf(data = grid_10, size = 0.03, fill = NA) +
  coord_sf(expand = FALSE)  +
  theme_map() +
  theme(legend.position = c(0.99, 0.99),
        legend.justification = c(1, 1),
        legend.direction = "vertical",
        legend.title = element_text(hjust = 0, margin = margin(b = 10)),
        plot.background = element_rect(fill = "transparent", color = NA),
        panel.background = element_rect(fill = "transparent", color = NA),
        legend.background = element_rect(fill = "transparent", color = NA),
        plot.title = element_text(hjust = 0, face = "bold"),
        axis.text.y = element_text(hjust = 0.5)) +
  labs(title = "A")

ggsave("figs/05_methods/fig-voronoi.png", height = 5.5, width = 6, bg = "transparent", dpi = 300)

# 4. Figure reef extent ----

load(file = "data/02_misc/reef_area_figure_data.RData")

data_annotations <- tibble(label = c("Ecoregion a", "Ecoregion b", "Ecoregion c"),
                           lat = c(-16.81, -16.55, -16.55),
                           long = c(145.4, 145.7, 146.33)) %>% 
  st_as_sf(coords = c("long", "lat"), crs = 4326)

plot_b <- ggplot() +
  geom_sf(data = grid_10, size = 0.03, fill = NA) +
  geom_sf(data = reefs_grid_10, aes(fill = as.numeric(Area / 10000))) +
  scale_fill_gradient(name = "Reef area\n(Ha)", low = "#E4F1FE", high = "#084594", limits = c(0, NA)) +
  geom_sf(data = meow_grid_10, linewidth = 0.4, fill = "transparent", color = "black") +
  geom_sf_text(data = data_annotations, aes(label = label), family = font_choose_graph, color = "#747d8c", size = 3) +
  coord_sf(expand = FALSE) +
  theme_map() +
  scale_x_continuous(breaks = c(145.2, 145.6, 146, 146.4)) +
  scale_y_continuous(breaks = c(-16.8, -16.4, -16)) +
  theme(legend.position = c(0.99, 0.99),
        legend.justification = c(1, 1),
        panel.grid = element_blank(),
        legend.direction = "vertical",
        legend.title = element_text(hjust = 0, margin = margin(b = 10)),
        plot.background = element_rect(fill = "transparent", color = NA),
        panel.background = element_rect(fill = "transparent", color = NA),
        legend.background = element_rect(fill = "transparent", color = NA),
        plot.title = element_text(hjust = 0, face = "bold"),
        axis.text.y = element_text(hjust = 0.5)) +
  labs(title = "B")

ggsave("figs/05_methods/fig-grid.png", height = 5.5, width = 6, bg = "transparent", dpi = 300)

# 5. Combine the plots ----

plot_a + plot_b &
  theme(plot.background = element_rect(fill = "transparent", colour = NA),
        panel.background = element_rect(fill = "transparent", colour = NA))

ggsave("figs/05_methods/fig-voronoi-grid.png", height = 4.5, width = 9.5, bg = "transparent", dpi = 300)
