data_descriptors <- function(data){
  
  # Number of sites 
  
  nb_sites <- data %>% 
    select(decimalLongitude, decimalLatitude) %>%
    distinct() %>% 
    count(name = "nb_sites")
  
  # Number of surveys
  
  nb_surveys <- data %>% 
    select(decimalLongitude, decimalLatitude, year, month, eventDate) %>%
    distinct() %>% 
    count(name = "nb_surveys")
  
  # Number of individual datasets
  
  nb_datasets <- data %>% 
    select(datasetID) %>%
    distinct() %>% 
    count(name = "nb_datasets")
  
  # First and last year with data
  
  first_last_year <- data %>% 
    mutate(first_year = min(year),
           last_year = max(year)) %>% 
    select(first_year, last_year) %>% 
    distinct()
  
  # Year from which 90% of surveys were assessed
  
  surveys_90_perc <- data %>% 
    select(decimalLongitude, decimalLatitude, year, month, eventDate) %>%
    distinct() %>% 
    group_by(year, .add = TRUE) %>% 
    count() %>% 
    ungroup(year) %>% 
    mutate(perc = (n*100)/sum(n)) %>% 
    arrange(-year) %>% 
    mutate(cumsum = cumsum(perc)) %>% 
    filter(cumsum >= 90) %>% # Put the threshold here
    filter(cumsum == min(cumsum)) %>% 
    select(year) %>% 
    rename(surveys_90_perc = year)
  
  # Return the results
  
  if (is.grouped_df(data) == TRUE) {
    
    result <- nb_datasets %>% 
      left_join(., nb_sites) %>% 
      left_join(., nb_surveys) %>% 
      left_join(., first_last_year) %>% 
      left_join(., surveys_90_perc)
    
  }else{
    
    result <- bind_cols(nb_datasets, nb_sites, nb_surveys, first_last_year, surveys_90_perc)
    
  } 
  
  return(result)
  
}