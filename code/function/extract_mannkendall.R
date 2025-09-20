extract_mannkendall <- function(data, var_y){
  
  require(Kendall)
  
  data_vector <- data %>% 
    pull(var_y)
  
  model <- MannKendall(data_vector)
  
  results <- tibble(tau = model$tau,
                    p_value = model$sl) %>% 
    mutate(trend = case_when(p_value > 0.05 ~ "No trend",
                             p_value <= 0.05 & tau < 0 ~ "Negative trend",
                             p_value <= 0.05 & tau > 0 ~ "Positive trend"))
  
  return(results)
  
}
