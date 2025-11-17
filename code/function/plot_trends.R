plot_trends <- function(region_i, level_i){
  
  if(level_i == "region"){
    
    data_i <- data_trends$raw_trends %>% 
      filter(level == "global")
    
    plot_i <- ggplot(data = data_i, aes(x = year, y = mean, ymin = lower_ci_95,
                                        ymax = upper_ci_95, fill = color, color = color)) +
      geom_ribbon(alpha = 0.35, color = NA) +
      geom_line() +
      facet_wrap(~text_title, scales = "free", ncol = 2) +
      scale_color_identity() +
      scale_fill_identity() +
      theme_graph() +
      theme(legend.title.position = "top",
            strip.text = element_markdown(hjust = 0, size = 14),
            legend.title = element_text(face = "bold", hjust = 0.5)) +
      scale_x_continuous(breaks = c(1980, 1990, 2000, 2010, 2020), limits = c(1980, 2025)) +
      scale_y_continuous(limits = c(0, floor(max(data_i$upper_ci_95)/10)*10+10)) +
      labs(x = "Year", y = "Benthic cover (%)")
    
    ggsave(filename = paste0("figs/01_part-1/fig-6.png"),
           plot = plot_i, height = 7, width = 9, dpi = fig_resolution)
    
  }else if(level_i == "region"){
    
    data_i <- data_trends$raw_trends %>% 
      filter(region == region_i & level == "region")
    
    plot_i <- ggplot(data = data_i, aes(x = year, y = mean, ymin = lower_ci_95,
                                        ymax = upper_ci_95, fill = color, color = color)) +
      geom_ribbon(alpha = 0.35, color = NA) +
      geom_line() +
      facet_wrap(~text_title, scales = "free", ncol = 2) +
      scale_color_identity() +
      scale_fill_identity() +
      theme_graph() +
      theme(legend.title.position = "top",
            strip.text = element_markdown(hjust = 0, size = 14),
            legend.title = element_text(face = "bold", hjust = 0.5)) +
      scale_x_continuous(breaks = c(1980, 1990, 2000, 2010, 2020), limits = c(1980, 2025)) +
      scale_y_continuous(limits = c(0, floor(max(data_i$upper_ci_95)/10)*10+10)) +
      labs(x = "Year", y = "Benthic cover (%)")
    
    ggsave(filename = paste0("figs/02_part-2/fig-5/",
                             str_replace_all(str_replace_all(str_to_lower(region_i), " ", "-"), "---", "-"), ".png"),
           plot = plot_i, height = 7, width = 9, dpi = fig_resolution)
    
  }else if(level_i == "subregion"){
    
    data_i <- data_trends$raw_trends %>% 
      filter(region == region_i & level == "subregion")
    
    plot_i <- ggplot(data = data_i, aes(x = year, y = mean, ymin = lower_ci_95,
                                        ymax = upper_ci_95, fill = color, color = color)) +
      geom_ribbon(alpha = 0.35, color = NA) +
      geom_line() +
      facet_grid(subregion~text_title, scales = "free") +
      scale_color_identity() +
      scale_fill_identity() +
      theme_graph() +
      theme(legend.title.position = "top",
            strip.text = element_markdown(hjust = 0, size = 14),
            legend.title = element_text(face = "bold", hjust = 0.5)) +
      scale_x_continuous(breaks = c(1980, 1990, 2000, 2010, 2020), limits = c(1980, 2025)) +
      scale_y_continuous(limits = c(0, floor(max(data_i$upper_ci_95)/10)*10+10)) +
      labs(x = "Year", y = "Benthic cover (%)")
    
    ggsave(filename = paste0("figs/02_part-2/fig-6/",
                             str_replace_all(str_replace_all(str_to_lower(region_i), " ", "-"), "---", "-"), ".png"),
           plot = plot_i, height = 10, width = 8, dpi = fig_resolution)
    
  }else if(level_i == "ecoregion"){
    
    
    
    
  }else{
    
    stop("The level argument must be region, subregion, or ecoregion.")
    
  }
  
}
