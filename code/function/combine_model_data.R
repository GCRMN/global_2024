combine_model_data <- function(model = "xgb"){
  
  # 1. List of RData files to combine
  
  data_files <- tibble(path = list.files("data/12_model-output/", full.names = TRUE, pattern = "RData"))
  
  # 2. Create a function to load RData files
  
  load_rdata <- function(path){
    
    load(file = path)
    
    return(model_results)
    
  }
  
  # 3. Combine files
  
  model_results <- map(data_files$path, ~load_rdata(path = .)) %>% 
    map_df(., ~ as.data.frame(map(.x, ~ unname(nest(.))))) %>% 
    map(., bind_rows)
  
  # 4. Add colors
  
  model_results <- model_results %>% 
    map(., ~ .x %>% add_colors)
  
  # 5. Return results
  
  return(model_results)
  
}
