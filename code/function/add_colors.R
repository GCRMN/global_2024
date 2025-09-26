add_colors <- function(data){
  
  data <- data %>% 
    mutate(color = case_when(category == "Hard coral" ~ "#c44d56",
                             category == "Algae" ~ "#16a085",
                             category == "Other fauna" ~ "#714d69",
                             category == "Macroalgae" ~ "#03a678",
                             category == "Turf algae" ~ "#26a65b",
                             category == "Coralline algae" ~ "#C5987D"),
           text_title = case_when(category == "Hard coral" ~ 
                                    glue("**A.**<span style='color:{color}'> {category}</span>"),
                                  category == "Algae" ~ 
                                    glue("**B.**<span style='color:{color}'> {category}</span>"),
                                  category == "Other fauna" ~ 
                                    glue("**E.**<span style='color:{color}'> {category}</span>"),
                                  category == "Coralline algae" ~ 
                                    glue("**C.**<span style='color:{color}'> {category}</span>"),
                                  category == "Macroalgae" ~ 
                                    glue("**D.**<span style='color:{color}'> {category}</span>"),
                                  category == "Turf algae" ~ 
                                    glue("**E.**<span style='color:{color}'> {category}</span>")))
  
  return(data)
  
}
