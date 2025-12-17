map_region_geography <- function(region_i, color_scalebar = "white"){
  
  data_subregions_i <- data_subregions %>% 
    filter(region == region_i)
  
  data_reefs_i <- st_intersection(data_reefs, data_subregions_i)
  
  plot_i <- ggplot() +
    geom_spatraster_rgb(data = data_tif, maxcell = 5e+07) +
    geom_sf(data = data_reefs_i, fill = "#ad5fad", color = "#ad5fad") +
    geom_sf(data = data_subregions_i, color = "white", fill = NA, linewidth = 0.3) +
    geom_sf(data = data_tropics, linetype = "dashed", linewidth = 0.25, color = "#483e37") +
    geom_sf(data = data_countries, fill = "NA", color = "black", linewidth = 0.15) +
    theme(panel.border = element_rect(fill = NA, color = "black"),
          plot.background = element_rect(fill = "transparent", color = NA),
          axis.text = element_text(family = font_choose_map, color = "black"),
          axis.text.y = element_text(angle = 90, hjust = 0.5),
          axis.text.y.right = element_text(angle = -90, hjust = 0.5))
  
  if(region_i == "South Asia"){
    
    plot_i <- plot_i + 
      coord_sf(xlim = c(60, 103), ylim = c(-12, 30),
               label_axes = list(top = "E", left = "N", right = "N")) +
      annotation_scale(location = "br",
                       width_hint = 0.25, text_family = font_choose_map, text_col = color_scalebar,
                       text_cex = 0.7, style = "bar", line_width = 1, height = unit(0.04, "cm"),
                       line_col = color_scalebar, pad_x = unit(0.5, "cm"), pad_y = unit(0.5, "cm"),
                       bar_cols = c(color_scalebar, color_scalebar))
    
    ggsave(paste0("figs/03_part-2/fig-1/", str_replace_all(str_to_lower(region_i), " ", "-"), "_raw.png"),
           height = 5.4, width = 5.5, bg = "transparent", dpi = fig_resolution)
    
  }else if(region_i == "EAS"){
    
    plot_i <- plot_i + 
      coord_sf(xlim = c(88, 145), ylim = c(-13, 35),
               label_axes = list(top = "E", left = "N", right = "N")) +
      annotation_scale(location = "bl",
                       width_hint = 0.25, text_family = font_choose_map, text_col = color_scalebar,
                       text_cex = 0.7, style = "bar", line_width = 1, height = unit(0.04, "cm"),
                       line_col = color_scalebar, pad_x = unit(0.5, "cm"), pad_y = unit(0.5, "cm"),
                       bar_cols = c(color_scalebar, color_scalebar))
    
    ggsave(paste0("figs/03_part-2/fig-1/", str_replace_all(str_to_lower(region_i), " ", "-"), "_raw.png"),
           height = 5.5, width = 6, bg = "transparent", dpi = fig_resolution)
    
  }else if(region_i == "Caribbean"){
    
    plot_i <- plot_i + 
      coord_sf(xlim = c(-100, -55), ylim = c(7.5, 35),
               label_axes = list(top = "E", left = "N", right = "N")) +
      annotation_scale(location = "bl",
                       width_hint = 0.25, text_family = font_choose_map, text_col = color_scalebar,
                       text_cex = 0.7, style = "bar", line_width = 1, height = unit(0.04, "cm"),
                       line_col = color_scalebar, pad_x = unit(0.5, "cm"), pad_y = unit(0.5, "cm"),
                       bar_cols = c(color_scalebar, color_scalebar))
    
    ggsave(paste0("figs/03_part-2/fig-1/", str_replace_all(str_to_lower(region_i), " ", "-"), "_raw.png"),
           height = 5.3, width = 8, bg = "transparent", dpi = fig_resolution)
    
  }else if(region_i == "WIO"){
    
    plot_i <- plot_i + 
      coord_sf(xlim = c(30, 68), ylim = c(11, -32),
               label_axes = list(top = "E", left = "N", right = "N")) +
      scale_x_continuous(breaks = c(35, 45, 55, 65)) +
      annotation_scale(location = "br",
                       width_hint = 0.25, text_family = font_choose_map, text_col = color_scalebar,
                       text_cex = 0.7, style = "bar", line_width = 1, height = unit(0.04, "cm"),
                       line_col = color_scalebar, pad_x = unit(0.5, "cm"), pad_y = unit(0.5, "cm"),
                       bar_cols = c(color_scalebar, color_scalebar))
    
    ggsave(paste0("figs/03_part-2/fig-1/", str_replace_all(str_to_lower(region_i), " ", "-"), "_raw.png"),
           height = 6.1, width = 5.4, bg = "transparent", dpi = fig_resolution)
    
  }else if(region_i == "ROPME"){
    
    plot_i <- plot_i + 
      coord_sf(xlim = c(45, 67), ylim = c(13, 32),
               label_axes = list(top = "E", left = "N", right = "N")) +
      annotation_scale(location = "br",
                       width_hint = 0.25, text_family = font_choose_map, text_col = color_scalebar,
                       text_cex = 0.7, style = "bar", line_width = 1, height = unit(0.04, "cm"),
                       line_col = color_scalebar, pad_x = unit(0.5, "cm"), pad_y = unit(0.5, "cm"),
                       bar_cols = c(color_scalebar, color_scalebar))
    
    ggsave(paste0("figs/03_part-2/fig-1/", str_replace_all(str_to_lower(region_i), " ", "-"), "_raw.png"),
           height = 5.6, width = 5.9, bg = "transparent", dpi = fig_resolution)
    
  }else if(region_i == "PERSGA"){
    
    plot_i <- plot_i + 
      coord_sf(xlim = c(29, 60), ylim = c(6, 34),
               label_axes = list(top = "E", left = "N", right = "N")) +
      scale_y_continuous(breaks = c(10, 15, 20, 25, 30)) +
      annotation_scale(location = "br",
                       width_hint = 0.25, text_family = font_choose_map, text_col = color_scalebar,
                       text_cex = 0.7, style = "bar", line_width = 1, height = unit(0.04, "cm"),
                       line_col = color_scalebar, pad_x = unit(0.5, "cm"), pad_y = unit(0.5, "cm"),
                       bar_cols = c(color_scalebar, color_scalebar))
    
    ggsave(paste0("figs/03_part-2/fig-1/", str_replace_all(str_to_lower(region_i), " ", "-"), "_raw.png"),
           height = 5.9, width = 5.9, bg = "transparent", dpi = fig_resolution)
    
  }else if(region_i == "Australia"){
    
    plot_i <- plot_i + 
      coord_sf(xlim = c(91, 170), ylim = c(-37, -7),
               label_axes = list(top = "E", left = "N", right = "N")) +
      annotation_scale(location = "bl",
                       width_hint = 0.25, text_family = font_choose_map, text_col = color_scalebar,
                       text_cex = 0.7, style = "bar", line_width = 1, height = unit(0.04, "cm"),
                       line_col = color_scalebar, pad_x = unit(0.5, "cm"), pad_y = unit(0.5, "cm"),
                       bar_cols = c(color_scalebar, color_scalebar))
    
    ggsave(paste0("figs/03_part-2/fig-1/", str_replace_all(str_to_lower(region_i), " ", "-"), "_raw.png"),
           height = 4.2, width = 8.5, bg = "transparent", dpi = fig_resolution)
    
  }else if(region_i == "ETP"){
    
    plot_i <- plot_i + 
      coord_sf(xlim = c(-123, -75), ylim = c(-6, 27),
               label_axes = list(top = "E", left = "N", right = "N")) +
      annotation_scale(location = "bl",
                       width_hint = 0.25, text_family = font_choose_map, text_col = color_scalebar,
                       text_cex = 0.7, style = "bar", line_width = 1, height = unit(0.04, "cm"),
                       line_col = color_scalebar, pad_x = unit(0.5, "cm"), pad_y = unit(0.5, "cm"),
                       bar_cols = c(color_scalebar, color_scalebar))
    
    ggsave(paste0("figs/03_part-2/fig-1/", str_replace_all(str_to_lower(region_i), " ", "-"), "_raw.png"),
           height = 4.8, width = 7.2, bg = "transparent", dpi = fig_resolution)
    
  }else if(region_i == "Brazil"){
    
    plot_i <- plot_i + 
      coord_sf(xlim = c(-62, -20), ylim = c(-27, 12),
               label_axes = list(top = "E", left = "N", right = "N")) +
      annotation_scale(location = "tr",
                       width_hint = 0.25, text_family = font_choose_map, text_col = color_scalebar,
                       text_cex = 0.7, style = "bar", line_width = 1, height = unit(0.04, "cm"),
                       line_col = color_scalebar, pad_x = unit(0.5, "cm"), pad_y = unit(0.5, "cm"),
                       bar_cols = c(color_scalebar, color_scalebar))
    
    ggsave(paste0("figs/03_part-2/fig-1/", str_replace_all(str_to_lower(region_i), " ", "-"), "_raw.png"),
           height = 4.9, width = 5.5, bg = "transparent", dpi = fig_resolution)
    
  }else if(region_i == "Pacific"){
    
    sf_use_s2(FALSE)
    
    crs_selected <- "+proj=eqc +lat_ts=0 +lat_0=0 +lon_0=160 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs"
    
    correction_offset <- 180 - 160
    
    correction_polygon <- st_polygon(x = list(rbind(c(-0.0001 - correction_offset, 90),
                                                    c(0 - correction_offset, 90),
                                                    c(0 - correction_offset, -90),
                                                    c(-0.0001 - correction_offset, -90),
                                                    c(-0.0001 - correction_offset, 90)))) %>%
      st_sfc() %>%
      st_set_crs(4326)
    
    data_countries_pacific <- data_countries %>% 
      st_difference(correction_polygon) %>% 
      st_transform(crs_selected)
    
    data_tropics_pacific <- data_tropics %>% 
      st_difference(correction_polygon) %>% 
      st_transform(crs_selected)
    
    data_subregions_i <- data_subregions_i %>% 
      st_difference(correction_polygon) %>% 
      st_transform(crs_selected)
    
    data_subregions_i %<>% # Special pipe from magrittr
      st_buffer(100) # To join polygon (remove vertical line)
    
    data_tif_pacific <- project(data_tif, crs_selected)
    
    data_reefs_pacific <- data_reefs %>% 
      st_difference(correction_polygon) %>% 
      st_transform(crs_selected)
    
    data_reefs_pacific <- st_intersection(data_reefs_pacific, data_subregions_i)
    
    plot_i <- ggplot() +
      geom_spatraster_rgb(data = data_tif_pacific, maxcell = 5e+07) +
      geom_sf(data = data_reefs_pacific, fill = "#ad5fad", color = "#ad5fad") +
      geom_sf(data = data_tropics_pacific, linetype = "dashed", linewidth = 0.25, color = "#483e37") +
      geom_sf(data = data_subregions_i, color = "white", fill = NA, linewidth = 0.3) +
      geom_sf(data = data_countries_pacific, fill = NA, color = "black", linewidth = 0.15) +
      coord_sf(ylim = c(-4000000, 4000000), xlim = c(-3500000, 11000000), expand = FALSE,
               label_axes = list(top = "E", left = "N", right = "N")) +
      scale_x_continuous(breaks = c(180, 160, 140, -160, -140, -120)) +
      theme(panel.border = element_rect(fill = NA, color = "black"),
            plot.background = element_rect(fill = "transparent", color = NA),
            axis.text = element_text(family = font_choose_map, color = "black"),
            axis.text.y = element_text(angle = 90, hjust = 0.5),
            axis.text.y.right = element_text(angle = -90, hjust = 0.5)) +
      annotation_scale(location = "br",
                       width_hint = 0.25, text_family = font_choose_map, text_col = color_scalebar,
                       text_cex = 0.7, style = "bar", line_width = 1, height = unit(0.04, "cm"),
                       line_col = color_scalebar, pad_x = unit(0.5, "cm"), pad_y = unit(0.25, "cm"),
                       bar_cols = c(color_scalebar, color_scalebar))
    
    ggsave(paste0("figs/03_part-2/fig-1/", str_replace_all(str_to_lower(region_i), " ", "-"), "_raw.png"),
           height = 4.5, width = 8, bg = "transparent", dpi = fig_resolution)
    
  }else{
    
    stop("The name of the region is incorrect")
    
  }
  
}
