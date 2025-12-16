plot_trends_raw <- function(region_i, level_i, category_i){
  
  data_benthic_cover <- data_benthic_cover %>% 
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
                                    glue("**E.**<span style='color:{color}'> {category}</span>"))) %>%
    filter(category %in% c("Hard coral", "Macroalgae"))
  
  # Global
  
  if(level_i == "global"){
    
    data_i <- data_benthic_cover %>% 
      group_by(year, category, color, text_title) %>% 
      summarise(mean = mean(measurementValue)) %>% 
      ungroup()
    
    ggplot(data = data_i) +
      geom_point(aes(x = year, y = mean, color = color)) +
      facet_wrap(~text_title, scales = "free", ncol = 2) +
      scale_color_identity() +
      scale_fill_identity() +
      theme_graph() +
      theme(legend.title.position = "top",
            strip.text = element_markdown(hjust = 0, size = 14),
            legend.title = element_text(face = "bold", hjust = 0.5)) +
      scale_x_continuous(breaks = c(1980, 1985, 1990, 1995, 2000, 2005, 2010, 2015, 2020, 2025),
                         limits = c(1979, 2026),
                         labels = c("1980", "", "1990", "", "2000", "", "2010", "", "2020", "")) +
      labs(x = "Year", y = "Benthic cover (%)")
    
    # Region    
    
  }else if(level_i == "region"){
    
    # 5 first regions
    
    data_i <- data_benthic_cover %>% 
      filter(category %in% c("Hard coral", "Macroalgae")) %>% 
      filter(region %in% c("Australia", "Brazil", "Caribbean", "EAS", "ETP")) %>% 
      group_by(year, region, category, color, text_title) %>% 
      summarise(mean = mean(measurementValue)) %>% 
      ungroup()
    
    plot_i <- ggplot(data = data_i) +
      geom_point(aes(x = year, y = mean, color = color)) +
      facet_grid(region~category, scales = "free") +
      scale_color_identity() +
      scale_fill_identity() +
      theme_graph() +
      theme(legend.title.position = "top",
            strip.text = element_markdown(hjust = 0.5, size = 14),
            legend.title = element_text(face = "bold", hjust = 0.5)) +
      scale_y_continuous(limits = c(0, 65)) +
      scale_x_continuous(breaks = c(1980, 1985, 1990, 1995, 2000, 2005, 2010, 2015, 2020, 2025),
                         limits = c(1979, 2026),
                         labels = c("1980", "", "1990", "", "2000", "", "2010", "", "2020", "")) +
      labs(x = "Year", y = "Benthic cover (%)")
    
    ggsave(filename = "../figs/06_supp-mat/raw-average_a.png",
           plot = plot_i, width = 7, height = 10, dpi = fig_resolution)
    
    # 5 last regions
    
    data_i <- data_benthic_cover %>% 
      filter(category %in% c("Hard coral", "Macroalgae")) %>% 
      filter(!(region %in% c("Australia", "Brazil", "Caribbean", "EAS", "ETP"))) %>% 
      group_by(year, region, category, color, text_title) %>% 
      summarise(mean = mean(measurementValue)) %>% 
      ungroup()
    
    plot_i <- ggplot(data = data_i) +
      geom_point(aes(x = year, y = mean, color = color)) +
      facet_grid(region~category, scales = "free") +
      scale_color_identity() +
      scale_fill_identity() +
      theme_graph() +
      theme(legend.title.position = "top",
            strip.text = element_markdown(hjust = 0.5, size = 14),
            legend.title = element_text(face = "bold", hjust = 0.5)) +
      scale_y_continuous(limits = c(0, 65)) +
      scale_x_continuous(breaks = c(1980, 1985, 1990, 1995, 2000, 2005, 2010, 2015, 2020, 2025),
                         limits = c(1979, 2026),
                         labels = c("1980", "", "1990", "", "2000", "", "2010", "", "2020", "")) +
      labs(x = "Year", y = "Benthic cover (%)")
    
    ggsave(filename = "../figs/06_supp-mat/raw-average_b.png",
           plot = plot_i, width = 7, height = 10, dpi = fig_resolution)
    
    # Subregion    
    
  }else if(level_i == "subregion"){
    
    data_i <- data_benthic_cover %>% 
      filter(region == region_i & !(is.na(subregion)) & category == category_i) %>% 
      group_by(year, subregion, category, color, text_title) %>% 
      summarise(mean = mean(measurementValue)) %>% 
      ungroup()
    
    nb_subregions <- length(unique(data_i$subregion))
    
    data_i <- tibble(subregion = sort(unique(data_i$subregion)),
                     letter = LETTERS[seq(from = 1, to = length(unique(data_i$subregion)))]) %>% 
      left_join(data_i, .) %>% 
      mutate(text_title = glue("**{letter}.** {subregion}"))
    
    plot_i <- ggplot(data = data_i,
                     aes(x = year, y = mean, fill = color, color = color)) +
      geom_point() +
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
            legend.title = element_text(face = "bold", hjust = 0.5)) +
      scale_x_continuous(breaks = c(1980, 1985, 1990, 1995, 2000, 2005, 2010, 2015, 2020, 2025),
                         limits = c(1979, 2026),
                         labels = c("1980", "", "", "", "2000", "", "", "", "2020", "")) +
      scale_y_continuous(limits = c(0, floor(max(data_i$mean)/10)*10+10)) +
      labs(x = "Year", y = "Benthic cover (%)")
    
    ggsave(filename = paste0("../figs/07_additional/subregions/raw_",
                             str_replace_all(str_replace_all(str_to_lower(category_i), " ", "-"), "---", "-"), "_",
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
    
    # Ecoregion
    
  }else if(level_i == "ecoregion"){
    
    data_i <- data_benthic_cover %>% 
      filter(region == region_i & !(is.na(ecoregion)) & category == category_i) %>% 
      group_by(year, ecoregion, category, color, text_title) %>% 
      summarise(mean = mean(measurementValue)) %>% 
      ungroup()
    
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
    
    plot_i <- ggplot(data = data_i,
                     aes(x = year, y = mean, fill = color, color = color)) +
      geom_point() +
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
      scale_y_continuous(limits = c(0, floor(max(data_i$mean)/10)*10+10)) +
      labs(x = "Year", y = "Benthic cover (%)")
    
    ggsave(filename = paste0("../figs/07_additional/ecoregions/raw_",
                             str_replace_all(str_replace_all(str_to_lower(category_i), " ", "-"), "---", "-"), "_",
                             str_replace_all(str_replace_all(str_to_lower(region_i), " ", "-"), "---", "-"), ".png"),
           plot = plot_i, height = (2.5*length(unique(data_i$ecoregion))/3),
           width = 10, dpi = fig_resolution)
    
  }else{
    
    stop("The level_i argument must be global, region, subregion, or ecoregion.")
    
  }
  
}
