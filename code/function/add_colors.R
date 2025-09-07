add_colors <- function(data){
  
  data <- data %>% 
    mutate(color = case_when(category == "Hard coral" ~ "#c44d56",
                             category == "Algae" ~ "#16a085",
                             category == "Other fauna" ~ "#714d69",
                             category == "Macroalgae" ~ "#03a678",
                             category == "Turf algae" ~ "#26a65b",
                             category == "Coralline algae" ~ "#C5987D",
                             category == "Acropora" ~ "#e08283",
                             category == "Orbicella" ~ "#c44d56",
                             category == "Porites" ~ "#a37c82"),
           text_title = case_when(category == "Hard coral" ~ 
                                    glue("**A.**<span style='color:{color}'> {category}</span>"),
                                  category == "Algae" ~ 
                                    glue("**B.**<span style='color:{color}'> {category}</span>"),
                                  category == "Other fauna" ~ 
                                    glue("**C.**<span style='color:{color}'> {category}</span>"),
                                  
                                  category == "Coralline algae" ~ 
                                    glue("**A.**<span style='color:{color}'> {category}</span>"),
                                  category == "Macroalgae" ~ 
                                    glue("**B.**<span style='color:{color}'> {category}</span>"),
                                  category == "Turf algae" ~ 
                                    glue("**C.**<span style='color:{color}'> {category}</span>"),
                                  
                                  category == "Acropora" ~ 
                                    glue("**A.***<span style='color:{color}'> {category}</span>*"),
                                  category == "Orbicella" ~ 
                                    glue("**B.***<span style='color:{color}'> {category}</span>*"),
                                  category == "Porites" ~ 
                                    glue("**C.***<span style='color:{color}'> {category}</span>*")))
  
  return(data)
  
}
