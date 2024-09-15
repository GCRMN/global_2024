extract_coeff <- function(data, var_y, var_x){
  
  model <- lm(get(var_y) ~ get(var_x), data = data)
  
  results <- summary(model)$coefficients
  
  results <- tibble(intercept = results[1, "Estimate"],
                    slope = results[2, "Estimate"])
  
  return(results)
  
}