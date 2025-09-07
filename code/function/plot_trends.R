plot_trends <- function(region_i, categories, icons = FALSE, raw_data = TRUE,
                        modelled_data = TRUE, scales = "fixed"){
  
  # Filter data
  

  
  data_raw_i <- data_benthic %>% 
    filter(region == region_i) %>% 
    filter(category %in% categories)
  
  # Base plot
  
  plot_trends <- ggplot() +
    {if(raw_data == TRUE)
      geom_pointrange(data = data_raw_i,
                      aes(x = year, y = mean, ymin = ymin,
                          ymax = ymax, color = color), size = 0.25)} +
    {if(modelled_data == TRUE)
      geom_ribbon(data = data_trends_i,
                  aes(x = year, ymin = lower_ci_95, ymax = upper_ci_95,
                      fill = color), alpha = 0.5, show.legend = FALSE)} +
    {if(modelled_data == TRUE)
      geom_line(data = data_trends_i,
                aes(x = year, y = mean, color = color),
                linewidth = 1, show.legend = FALSE)} +
    scale_color_identity() +
    scale_fill_identity() +
    facet_wrap(~text_title, scales = scales) +
    theme(strip.text = element_markdown(hjust = 0, size = rel(1.3)),
          strip.background = element_blank(),
          panel.spacing = unit(2, "lines")) +
    labs(x = "Year", y = "Cover (%)") +
    lims(x = c(1980, 2024), y = c(0, NA))
  
  # Icons
  
  if(icons == FALSE){
    
    plot_results <- plot_trends
    
  }else if(icons == TRUE){
      
    require(cowplot)
    
    data_icons <- tibble(category = c("Hard coral",
                                      "Algae"),
                         path = c("figs/00_misc/icon_coral.png",
                                  "figs/00_misc/icon_algae.png")) %>% 
      filter(category %in% categories)
    
    plot_results <- ggdraw(plot_trends) + 
      draw_image(as.character(data_icons[1, 2]),
                 x = 0.365, y = 0.95, # Position above right
                 hjust = 1, vjust = 1,
                 width = 0.11, height = 0.11) +
      draw_image(as.character(data_icons[2, 2]),
                 x = 0.6835, y = 0.95, # Position above right
                 hjust = 1, vjust = 1,
                 width = 0.11, height = 0.11) + # Relative proportion of the image
      draw_image(as.character(data_icons[3, 2]),
                 x = 1.005, y = 0.95, # Position above right
                 hjust = 1, vjust = 1,
                 width = 0.11, height = 0.11) # Relative proportion of the image
      
  }else{
      
    stop("icons must be TRUE or FALSE")
      
  }
    
 # Save plots
    
  if(all.equal(categories, c("Hard coral", "Algae", "Other fauna")) == TRUE){
      
    if(area_i == "All"){
      
      ggsave(plot = plot_results, filename = "figs/01_part-1/fig-12.png", width = 14, height = 5)
      
    }else{
      
      if(modelled_data == TRUE){
        
        ggsave(plot = plot_results, filename = paste0("figs/02_part-2/fig-5/",
                                                      str_replace_all(str_replace_all(str_to_lower(area_i), " ", "-"),
                                                                      "---", "-"),
                                                      ".png"),
               width = 14, height = 5)
        
      }else{
        
        ggsave(plot = plot_results, filename = paste0("figs/02_part-2/fig-5b/",
                                                      str_replace_all(str_replace_all(str_to_lower(area_i), " ", "-"),
                                                                      "---", "-"),
                                                      ".png"),
               width = 14, height = 5)
        
      }
      
    }
      
  }else if(all.equal(categories, c("Coralline algae", "Macroalgae", "Turf algae")) == TRUE){
      
    if(area_i == "All"){
      
      ggsave(plot = plot_results, filename = "figs/01_part-1/fig-13.png", width = 14, height = 5)
      
    }else{
      
      ggsave(plot = plot_results, filename = paste0("figs/06_additional/04_benthic-trends/algae_",
                                                   str_replace_all(str_replace_all(str_to_lower(area_i), " ", "-"),
                                                                   "---", "-"),
                                                   ".png"),
             width = 14, height = 5)
      
    }
      
  }else if(all.equal(categories, c("Acropora", "Orbicella", "Porites")) == TRUE){
      
    if(area_i == "All"){
      
      ggsave(plot = plot_results, filename = "figs/01_part-1/fig-14.png", width = 14, height = 5)
      
    }else{
      
      ggsave(plot = plot_results, filename = paste0("figs/06_additional/04_benthic-trends/coral_",
                                                   str_replace_all(str_replace_all(str_to_lower(area_i), " ", "-"),
                                                                   "---", "-"),
                                                   ".png"),
             width = 14, height = 5)
      
    }
      
  }else{
      
   stop("Sequence of categories is not valid")
      
  }

}
