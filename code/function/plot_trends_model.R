plot_trends_model <- function(region_i, level_i, category_i = NA, range = NA){

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
      filter(level == "global" & category %in% c("Hard coral", "Macroalgae")) %>% 
      group_by(category) %>% 
      { 
        if (range == "obs") {
          filter(., year >= first_year & year <= last_year)
        } else if(range == "full") {
          .
        }
      } %>% 
      ungroup()
    
    plot_i <- ggplot(data = data_i, aes(x = year, fill = color, color = color)) +
      geom_ribbon(aes(ymin = lower_ci_95, ymax = upper_ci_95), alpha = 0.35, color = NA) +
      geom_ribbon(aes(ymin = lower_ci_80, ymax = upper_ci_80), alpha = 0.45, color = NA) +
      geom_line(aes(y = mean)) +
      facet_wrap(~text_title, scales = "free", ncol = 2) +
      scale_color_identity() +
      scale_fill_identity() +
      theme_graph() +
      theme(legend.title.position = "top",
            strip.text = element_markdown(hjust = 0, size = 14),
            legend.title = element_text(face = "bold", hjust = 0.5),
            panel.background = element_rect(fill = "transparent", colour = NA),
            plot.background = element_rect(fill = "transparent", colour = NA)) +
      scale_x_continuous(breaks = c(1980, 1985, 1990, 1995, 2000, 2005, 2010, 2015, 2020, 2025),
                         limits = c(1979, 2026),
                         labels = c("1980", "", "1990", "", "2000", "", "2010", "", "2020", "")) +
      scale_y_continuous(limits = c(0, floor(max(data_i$upper_ci_95)/10)*10+10)) +
      labs(x = "Year", y = "Benthic cover (%)")
    
    ggsave(filename = paste0("figs/02_part-1/fig-3.png"),
           plot = plot_i, height = 4, width = 8.5, dpi = fig_resolution)
    
    ggsave(filename = paste0("figs/02_part-1/fig-3.pdf"),
           plot = plot_i, height = 4, width = 8.5, bg = "transparent")
    
  # Region
    
  }else if(level_i == "region"){
    
    data_i <- data_models %>% 
      filter(level == "region") %>% 
      group_by(category, region, subregion, ecoregion) %>% 
      { 
        if (range == "obs") {
          filter(., year >= first_year & year <= last_year)
        } else if(range == "full") {
          .
        }
      } %>% 
      ungroup() %>% 
      filter(category %in% c("Hard coral", "Macroalgae", "Turf algae") & region == region_i) %>% 
      filter(!(category == "Turf algae" & region %in% c("PERSGA", "South Asia"))) %>% 
      group_by(category) %>% 
      transform_ribbons() %>% 
      ungroup()
    
    plot_i <- ggplot(data = data_i) +
      geom_ribbon(aes(x = year, ymin = lower_ci_95, ymax = upper_ci_95, fill = color, group = group), 
                  alpha = 0.25, show.legend = FALSE) +
      geom_ribbon(aes(x = year, ymin = lower_ci_80, ymax = upper_ci_80, fill = color, group = group), 
                  alpha = 0.5, show.legend = FALSE) +
      geom_line(aes(x = year, y = mean, color = color, group = group), show.legend = FALSE) +
      facet_wrap(~text_title, scales = "free", ncol = 3) +
      scale_color_identity() +
      scale_fill_identity() +
      theme_graph() +
      theme(legend.title.position = "top",
            strip.text = element_markdown(hjust = 0, size = 14),
            legend.title = element_text(face = "bold", hjust = 0.5),
            panel.background = element_rect(fill = "transparent", colour = NA),
            plot.background = element_rect(fill = "transparent", colour = NA)) +
      scale_x_continuous(breaks = c(1980, 1985, 1990, 1995, 2000, 2005, 2010, 2015, 2020, 2025),
                         limits = c(1979, 2026),
                         labels = c("1980", "", "1990", "", "2000", "", "2010", "", "2020", "")) +
      scale_y_continuous(limits = c(0, floor(max(data_i$upper_ci_95)/10)*10+10)) +
      labs(x = "Year", y = "Benthic cover (%)")
    
    if(region_i %in% c("PERSGA", "South Asia")){
      
      ggsave(filename = paste0("figs/03_part-2/fig-6/",
                               str_replace_all(str_replace_all(str_to_lower(region_i), " ", "-"), "---", "-"), ".png"),
             plot = plot_i, height = 4, width = 8.5, dpi = fig_resolution)
      
      ggsave(filename = paste0("figs/03_part-2/fig-6/",
                               str_replace_all(str_replace_all(str_to_lower(region_i), " ", "-"), "---", "-"), ".pdf"),
             plot = plot_i, height = 4, width = 8.5, bg = "transparent")
      
    }else{
      
      ggsave(filename = paste0("figs/03_part-2/fig-6/",
                               str_replace_all(str_replace_all(str_to_lower(region_i), " ", "-"), "---", "-"), ".png"),
             plot = plot_i, height = 4, width = 11, dpi = fig_resolution)
      
      ggsave(filename = paste0("figs/03_part-2/fig-6/",
                               str_replace_all(str_replace_all(str_to_lower(region_i), " ", "-"), "---", "-"), ".pdf"),
             plot = plot_i, height = 4, width = 11, bg = "transparent")
      
    }
    
  # Subregion
    
  }else if(level_i == "subregion"){
    
    data_i <- data_models %>% 
      filter(level == "subregion") %>% 
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
    
    data_i <- tibble(subregion = sort(unique(data_i$subregion)),
                     letter = LETTERS[seq(from = 1, to = length(unique(data_i$subregion)))]) %>% 
      left_join(data_i, .) %>% 
      mutate(subregion_name = case_when(subregion_name == "Eastern Australian shelf / Great Barrier Reef" ~
                                          "Eastern Australian shelf /<br>Great Barrier Reef",
                                        subregion_name == "Tropical Southwestern Pacific" ~
                                          "Tropical Southwestern<br>Pacific",
                                        subregion_name == "Central West Australian Shelf" ~
                                          "Central West<br>Australian Shelf",
                                        subregion_name == "Tropical Northwestern Pacific" ~
                                          "Tropical Northwestern<br>Pacific",
                                        
                                        subregion_name == "Lord Howe and Norfolk Islands" ~ 
                                          "Lord Howe and<br>Norfolk Islands",
                                        subregion_name == "Fernando de Noronha and Rocas Atoll" ~ 
                                          "Fernando de Noronha<br>and Rocas Atoll",
                                        subregion_name == "Marshall, Gilbert, and Ellis Islands" ~ 
                                          "Marshall, Gilbert,<br>and Ellis Islands",
                                        subregion_name == "Cocos Keeling and Christmas Island" ~ 
                                          "Cocos Keeling and<br>Christmas Island",
                                        TRUE ~ paste0(subregion_name, "<br>")),
             text_title = glue("**{letter}.** {subregion}<br><span style='color:#636e72; font-size:12px'>{subregion_name}</span>")) %>% 
      group_by(category, subregion) %>% 
      transform_ribbons() %>% 
      ungroup()
    
    plot_i <- ggplot(data = data_i) +
      geom_ribbon(aes(x = year, ymin = lower_ci_95, ymax = upper_ci_95, fill = color, group = group), 
                  alpha = 0.25, show.legend = FALSE) +
      geom_ribbon(aes(x = year, ymin = lower_ci_80, ymax = upper_ci_80, fill = color, group = group), 
                  alpha = 0.5, show.legend = FALSE) +
      geom_line(aes(x = year, y = mean, color = color, group = group), show.legend = FALSE) +
      facet_wrap(~text_title, scales = "free", ncol = case_when(nb_subregions == 3 ~ 3,
                                                               nb_subregions == 4 ~ 2,
                                                               nb_subregions == 5 ~ 3,
                                                               nb_subregions == 6 ~ 3,
                                                               nb_subregions >= 7 ~ 4)) +
      scale_color_identity() +
      scale_fill_identity() +
      theme_graph() +
      theme(legend.title.position = "top",
            strip.text = element_markdown(hjust = 0, size = 12),
            legend.title = element_text(face = "bold", hjust = 0.5),
            panel.background = element_rect(fill = "transparent", colour = NA),
            plot.background = element_rect(fill = "transparent", colour = NA)) +
      scale_x_continuous(breaks = c(1980, 1985, 1990, 1995, 2000, 2005, 2010, 2015, 2020, 2025),
                         limits = c(1979, 2026),
                         labels = c("1980", "", "", "", "2000", "", "", "", "2020", "")) +
      scale_y_continuous(limits = c(0, floor(max(data_i$upper_ci_95)/10)*10+10)) +
      labs(x = "Year", y = case_when(category_i == "Hard coral" ~ "Hard coral cover (%)",
                                     category_i == "Macroalgae" ~ "Macroalgae cover (%)"))
    
    fig_i <- case_when(category_i == "Hard coral" ~ "fig-7",
                       category_i == "Macroalgae" ~ "fig-8")
    
    ggsave(filename = paste0("figs/03_part-2/", fig_i, "/",
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
    
    ggsave(filename = paste0("figs/03_part-2/", fig_i, "/",
                             str_replace_all(str_replace_all(str_to_lower(region_i), " ", "-"), "---", "-"), ".pdf"),
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
                             nb_subregions >= 7 ~ 11))
      
  # Ecoregion
    
  }else if(level_i == "ecoregion"){
    
    data_i <- data_models %>% 
      filter(level == "ecoregion") %>% 
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
                                   ecoregion == "Southwestern Caribbean" ~
                                     "Southwestern\nCaribbean",
                                   ecoregion == "Torres Strait Northern Great Barrier Reef" ~
                                     "Torres Strait Northern\nGreat Barrier Reef",
                                   length_more_20 == TRUE & slash_present == TRUE ~ 
                                     gsub("/", "\n", ecoregion),
                                   length_more_20 == TRUE & slash_present == FALSE ~ 
                                     paste0(str_split_fixed(ecoregion, " ", 3)[,1], " ",
                                            str_split_fixed(ecoregion, " ", 3)[,2], "\n",
                                            str_split_fixed(ecoregion, " ", 3)[,3]),
                                   TRUE ~ ecoregion)) %>% 
      select(-length_more_20, -slash_present)
    
    plot_i <- ggplot(data = data_i, aes(x = year, fill = color, color = color)) +
      geom_ribbon(aes(ymin = lower_ci_95, ymax = upper_ci_95), alpha = 0.35, color = NA) +
      geom_ribbon(aes(ymin = lower_ci_80, ymax = upper_ci_80), alpha = 0.45, color = NA) +
      geom_line(aes(y = mean)) +
      facet_wrap(~ecoregion, scales = "free", ncol = 4) +
      scale_color_identity() +
      scale_fill_identity() +
      theme_graph() +
      theme(legend.title.position = "top",
            strip.text = element_text(face = "bold", hjust = 0, size = 10),
            legend.title = element_text(face = "bold", hjust = 0.5)) +
      scale_x_continuous(breaks = c(1980, 1985, 1990, 1995, 2000, 2005, 2010, 2015, 2020, 2025),
                         limits = c(1979, 2026),
                         labels = c("1980", "", "", "", "2000", "", "", "", "2020", "")) +
      scale_y_continuous(limits = c(0, floor(max(data_i$upper_ci_95)/10)*10+10)) +
      labs(x = "Year", y = "Benthic cover (%)")
    
    ggsave(filename = paste0("figs/07_additional/ecoregions/model_",
                             str_replace_all(str_replace_all(str_to_lower(category_i), " ", "-"), "---", "-"), "_",
                             str_replace_all(str_replace_all(str_to_lower(region_i), " ", "-"), "---", "-"), ".png"),
           plot = plot_i, height = (2.5*length(unique(data_i$ecoregion))/3),
           width = 10, dpi = fig_resolution)
    
  }else{
    
    stop("The level_i argument must be region, subregion, or ecoregion.")
    
  }
  
}
