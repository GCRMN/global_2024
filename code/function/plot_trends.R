plot_trends <- function(region_i, level_i, category_i = NA, range = NA){

  # range: if 'full' then all years are plotted,
  # if 'obs' only years where observed data were available are plotted
  # (to be exact all years between first and last year with data)
  
  data_models <- data_models %>% 
    mutate(color = case_when(category == "Hard coral" ~ "#c44d56",
                             category == "Algae" ~ "#16a085",
                             category == "Other fauna" ~ "#714d69",
                             category == "Macroalgae" ~ "#03a678",
                             category == "Turf algae" ~ "#26a65b",
                             category == "Coralline algae" ~ "#C5987D"),
           text_title = case_when(category == "Hard coral" ~ 
                                    glue("**A.**<span style='color:{color}'> {category}</span>"),
                                  category == "Macroalgae" ~ 
                                    glue("**B.**<span style='color:{color}'> {category}</span>"),
                                  category == "Turf algae" ~ 
                                    glue("**C.**<span style='color:{color}'> {category}</span>"),
                                  category == "Coralline algae" ~ 
                                    glue("**D.**<span style='color:{color}'> {category}</span>"),
                                  category == "Other fauna" ~ 
                                    glue("**E.**<span style='color:{color}'> {category}</span>")))
  
  # Global
  
  if(level_i == "global"){
    
    data_i <- data_models %>% 
      filter(level == "global" & category %in% c("Hard coral", "Macroalgae") & model == "HBM") %>% 
      group_by(category) %>% 
      { 
        if (range == "obs") {
          filter(., year >= first_year & year <= last_year)
        } else if(range == "full") {
          .
        }
      } %>% 
      ungroup()
    
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
      scale_x_continuous(breaks = c(1980, 1990, 2000, 2010, 2020), limits = c(1979, 2026)) +
      scale_y_continuous(limits = c(0, floor(max(data_i$upper_ci_95)/10)*10+10)) +
      labs(x = "Year", y = "Benthic cover (%)")
    
    ggsave(filename = paste0("figs/01_part-1/fig-6.png"),
           plot = plot_i, height = 4, width = 8.5, dpi = fig_resolution)
    
  # Region
    
  }else if(level_i == "region"){
    
    data_i <- data_models %>% 
      filter(level == "region" & model == "HBM") %>% 
      group_by(category, region, subregion, ecoregion) %>% 
      { 
        if (range == "obs") {
          filter(., year >= first_year & year <= last_year)
        } else if(range == "full") {
          .
        }
      } %>% 
      ungroup() %>% 
      filter(category %in% c("Hard coral", "Macroalgae") & region == region_i)
    
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
      scale_x_continuous(breaks = c(1980, 1990, 2000, 2010, 2020), limits = c(1979, 2026)) +
      scale_y_continuous(limits = c(0, floor(max(data_i$upper_ci_95)/10)*10+10)) +
      labs(x = "Year", y = "Benthic cover (%)")
    
    ggsave(filename = paste0("figs/02_part-2/fig-5/",
                             str_replace_all(str_replace_all(str_to_lower(region_i), " ", "-"), "---", "-"), ".png"),
           plot = plot_i, height = 4, width = 8.5, dpi = fig_resolution)
    
  # Subregion
    
  }else if(level_i == "subregion"){
    
    data_i <- data_models %>% 
      filter(level == "subregion" & model == "HBM") %>% 
      group_by(category, region) %>% 
      { 
        if (range == "obs") {
          filter(., year >= first_year & year <= last_year)
        } else if(range == "full") {
          .
        }
      } %>%
      ungroup() %>% 
      filter(category == category_i & region == region_i)
    
    nb_subregions <- length(unique(data_i$subregion))
    
    plot_i <- ggplot(data = data_i, aes(x = year, y = mean, ymin = lower_ci_95,
                                        ymax = upper_ci_95, color = color, fill = color)) +
      geom_ribbon(alpha = 0.35, color = NA) +
      geom_line() +
      facet_wrap(~subregion, scales = "free", ncol = case_when(nb_subregions == 3 ~ 3,
                                                               nb_subregions == 4 ~ 2,
                                                               nb_subregions == 5 ~ 3,
                                                               nb_subregions == 6 ~ 3,
                                                               nb_subregions >= 7 ~ 4)) +
      scale_color_identity() +
      scale_fill_identity() +
      theme_graph() +
      theme(legend.title.position = "top",
            strip.text = element_markdown(hjust = 0, size = 12, face = "bold"),
            legend.title = element_text(face = "bold", hjust = 0.5)) +
      scale_x_continuous(breaks = c(1980, 2000, 2020), limits = c(1979, 2026)) +
      scale_y_continuous(limits = c(0, floor(max(data_i$upper_ci_95)/10)*10+10)) +
      labs(x = "Year", y = "Benthic cover (%)")
    
    if(category_i == "Hard coral"){
      
      ggsave(filename = paste0("figs/02_part-2/fig-6/",
                               str_replace_all(str_replace_all(str_to_lower(region_i), " ", "-"), "---", "-"), ".png"),
             plot = plot_i,
             height = case_when(nb_subregions == 3 ~ 3.5,
                                nb_subregions == 4 ~ 5.5,
                                nb_subregions == 5 ~ 5.5,
                                nb_subregions == 6 ~ 5.5,
                                nb_subregions >= 7 ~ 7),
             width = case_when(nb_subregions == 3 ~ 8,
                               nb_subregions == 4 ~ 6.5,
                               nb_subregions == 5 ~ 9,
                               nb_subregions == 6 ~ 9,
                               nb_subregions >= 7 ~ 11),
             dpi = fig_resolution)
      
    }else if(category_i == "Macroalgae"){
      
      ggsave(filename = paste0("figs/02_part-2/fig-6b/",
                               str_replace_all(str_replace_all(str_to_lower(region_i), " ", "-"), "---", "-"), ".png"),
             plot = plot_i,
             height = case_when(nb_subregions == 3 ~ 3.5,
                                nb_subregions == 4 ~ 5.5,
                                nb_subregions == 5 ~ 5.5,
                                nb_subregions == 6 ~ 5.5,
                                nb_subregions >= 7 ~ 7),
             width = case_when(nb_subregions == 3 ~ 8,
                               nb_subregions == 4 ~ 6.5,
                               nb_subregions == 5 ~ 9,
                               nb_subregions == 6 ~ 9,
                               nb_subregions >= 7 ~ 11),
             dpi = fig_resolution)
      
    }
    
  # Ecoregion
    
  }else if(level_i == "ecoregion"){
    
    data_i <- data_models %>% 
      filter(level == "ecoregion" & model == "HBM") %>% 
      group_by(category, region) %>% 
      { 
        if (range == "obs") {
          filter(., year >= first_year & year <= last_year)
        } else if(range == "full") {
          .
        }
      } %>%
      ungroup() %>% 
      filter(category == category_i & region == region_i)
    
    # Split text to plot it on two lines
    data_i <- data_i %>% 
      mutate(length_more_20 = str_length(ecoregion) > 20, slash_present = str_detect(ecoregion, "/")) %>% 
      mutate(ecoregion = case_when(ecoregion == "Arnhem Coast to Gulf of Carpenteria" ~
                                     "Arnhem Coast to\nGulf of Carpenteria",
                                   ecoregion == "Fernando de Naronha and Atoll das Rocas" ~
                                     "Fernando de Naronha\nand Atoll das Rocas",
                                   ecoregion == "Lord Howe and Norfolk Islands" ~
                                     "Lord Howe and\nNorfolk Islands",
                                   ecoregion == "Phoenix/Tokelau/Northern Cook Islands" ~
                                     "Phoenix/Tokelau\nNorthern Cook Islands",
                                   ecoregion == "South China Sea Oceanic Islands" ~
                                     "South China Sea\nOceanic Islands",
                                   ecoregion == "Torres Strait Northern Great Barrier Reef" ~
                                     "Torres Strait Northern\nGreat Barrier Reef",
                                   ecoregion == "Central and Southern Great Barrier Reef" ~
                                     "Central and Southern\nGreat Barrier Reef",
                                   length_more_20 == TRUE & slash_present == TRUE ~ 
                                     gsub("/", "\n", ecoregion),
                                   length_more_20 == TRUE & slash_present == FALSE ~ 
                                     paste0(str_split_fixed(ecoregion, " ", 3)[,1], " ",
                                            str_split_fixed(ecoregion, " ", 3)[,2], "\n",
                                            str_split_fixed(ecoregion, " ", 3)[,3]),
                                   TRUE ~ ecoregion)) %>% 
      select(-length_more_20, -slash_present)
    
    plot_i <- ggplot(data = data_i, aes(x = year, y = mean, ymin = lower_ci_95,
                                        ymax = upper_ci_95, color = color, fill = color)) +
      geom_ribbon(alpha = 0.35, color = NA) +
      geom_line() +
      facet_wrap(~ecoregion, scales = "free", ncol = 4) +
      scale_color_identity() +
      scale_fill_identity() +
      theme_graph() +
      theme(legend.title.position = "top",
            strip.text = element_text(face = "bold", hjust = 0),
            legend.title = element_text(face = "bold", hjust = 0.5)) +
      scale_x_continuous(breaks = c(1980, 2000, 2020), limits = c(1979, 2026)) +
      scale_y_continuous(limits = c(0, floor(max(data_i$upper_ci_95)/10)*10+10)) +
      labs(x = "Year", y = "Benthic cover (%)")
    
    ggsave(filename = paste0("figs/06_additional/04_benthic-trends/ecoregion_",
                             str_replace_all(str_replace_all(str_to_lower(category_i), " ", "-"), "---", "-"),
                             "_",
                             str_replace_all(str_replace_all(str_to_lower(region_i), " ", "-"), "---", "-"),
                             ".png"),
           plot = plot_i, height = (2.5*length(unique(data_i$ecoregion))/3),
           width = 10, dpi = fig_resolution)
    
  }else{
    
    stop("The level_i argument must be region, subregion, or ecoregion.")
    
  }
  
}
