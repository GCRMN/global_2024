plot_map_labels <- function(subregion_name_i, color_major = "#c44d56", color_minor = "#f1a9a0"){
  
  data_labels_i <- data_labels <- data_monitoring %>% 
    rename(subregion_name = subregion) %>% 
    mutate(across(c(nb_sites, nb_surveys), ~str_trim(format(.x, big.mark = ","))),
           label_a = paste0("<span style = 'color: ", color_major,"'>",nb_sites, "</span> sites"),
           label_b = paste0("<span style = 'color: ", color_major,"'>",nb_surveys, "</span> surveys"),
           label_c = paste0("<span style = 'color: ", color_major,"'>",first_year,
                            "</span> → ","<span style = 'color: ", color_major,"'>",last_year, "</span>"),
           subregion_nb = parse_number(subregion_name))%>% 
    filter(subregion_name == subregion_name_i)
  
  ggplot() +
    annotate(geom = "rect", xmin = 0, xmax = 2, ymin = 0, ymax = 2,
             fill = "white") +
    annotate(geom = "rect", xmin = 0, xmax = 0.45, ymin = 1.5, ymax = 2,
             fill = color_major) +
    annotate(geom = "rect", xmin = 0.475, xmax = 2, ymin = 1.5, ymax = 2,
             fill = color_minor) +
    annotate(geom = "rect", xmin = 0, xmax = 2, ymin = 0, ymax = 2,
             fill = NA, color = "black", linewidth = 2) +
    geom_text(data = data_labels_i, aes(x = 0.235, y = 1.75, label = subregion_nb),
              color = "white", size = 20, family = font_choose_map, fontface = "bold") +
    geom_textbox(data = data_labels_i, aes(x = 0.6, y = 1.75, label = subregion_name), width = unit(15, "cm"),
                 box.colour = NA, fill = NA, size = 16, family = font_choose_map, hjust = 0) +
    geom_textbox(data = data_labels_i, aes(x = 0.235, y = 1.15, label = label_a), width = unit(15, "cm"),
                 box.colour = NA, fill = NA, size = 16, family = font_choose_map, hjust = 0) +
    geom_textbox(data = data_labels_i, aes(x = 0.235, y = 0.75, label = label_b), width = unit(15, "cm"),
                 box.colour = NA, fill = NA, size = 16, family = font_choose_map, hjust = 0) +
    geom_textbox(data = data_labels_i, aes(x = 0.235, y = 0.35, label = label_c), width = unit(15, "cm"),
                 box.colour = NA, fill = NA, size = 16, family = font_choose_map, hjust = 0) +
    theme_minimal() +
    theme(axis.text = element_blank(),
          axis.title = element_blank(),
          panel.grid = element_blank())
  
  ggsave(paste0("figs/02_part-2/fig-3/labels/", subregion_name_i, ".png"),  height = 5, width = 7)
  
}
