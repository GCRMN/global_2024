transform_ribbons <- function(data){
  
  cm <- data %>%
    mutate(rleid = with(rle(data_obs), rep(seq_along(lengths), lengths)),
           group = as.integer(rleid))
  
  result <- cm %>% 
    ungroup %>% 
    mutate(d = 3) %>%
    uncount(d, .id = "A") %>%
    mutate_at(vars(year, mean, lower_ci_80, upper_ci_80, lower_ci_95, upper_ci_95),
              function(x=.) ifelse(.$A == 1,(x + lag(x))/2,
                                   ifelse(.$A == 3, (x + lead(x))/2, x))) %>%
    group_by_at(group_vars(cm)) %>%
    filter(row_number()!= 1, row_number() != n()) %>% 
    ungroup() %>% 
    select(-A, -rleid) %>% 
    mutate(color = case_when(data_obs == FALSE ~ "#95afc0",
                             data_obs == TRUE ~ color))
  
  return(result)
  
}
