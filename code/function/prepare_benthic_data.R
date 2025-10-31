prepare_benthic_data <- function(data, remove_na = FALSE, regenerate_zero = FALSE){
  
  # Create function_regenerate_zero
  
  function_regenerate_zero <- function(data, regenerate_zero){
    
    if(regenerate_zero == FALSE){
      
      result <- data
      
    }else if(regenerate_zero == TRUE){
      
      result <- data %>% 
        group_by(datasetID) %>% 
        complete(category,
                 nesting(region, subregion, ecoregion, country, territory, locality,
                         habitat, parentEventID, decimalLatitude, decimalLongitude, verbatimDepth,
                         year, month, day, eventDate),
                 fill = list(measurementValue = 0)) %>%
        ungroup()
      
    }else{
      
      stop("regenerate_zero must be either TRUE or FALSE")
      
    }
    
    return(result)
    
  }
  
  # Create function_remove_na
  
  function_remove_na <- function(data, remove_na){
    
    if(remove_na == FALSE){
      
      result <- data %>% 
        mutate(category = subcategory) %>% 
        drop_na(category)
      
    }else if(remove_na == TRUE){
      
      result <- data %>% 
        group_by(datasetID, region, subregion, ecoregion, country, territory, locality, habitat, parentEventID,
                 decimalLatitude, decimalLongitude, verbatimDepth, year, month, day, eventDate, eventID) %>% 
        filter(any(is.na(subcategory)) == FALSE) %>% 
        ungroup() %>% 
        mutate(category = subcategory)
      
    }else{
      
      stop("remove_na must be either TRUE or FALSE")
      
    }
    
    return(result)
    
  }
  
  # Main categories
  
  result_category <- data %>% 
    # 1. Filter taxonomic level
    filter(category %in% c("Hard coral", "Algae", "Other fauna")) %>% 
    # 2. Sum of benthic cover per sampling unit (site, transect, quadrat) and category
    group_by(datasetID, region, subregion, ecoregion, country, territory, locality, habitat, parentEventID,
             decimalLatitude, decimalLongitude, verbatimDepth, year, month, day, eventDate, eventID, category) %>% 
    summarise(measurementValue = sum(measurementValue)) %>% 
    ungroup() %>% 
    # 3. Regenerate 0 values
    function_regenerate_zero(data = ., regenerate_zero = regenerate_zero) %>%
    # 4. Average of benthic cover per transect (i.e. mean of photo-quadrats)
    # This avoid getting semi-quantitative data (e.g. when there is only 10 points per photo-quadrat)
    group_by(datasetID, region, subregion, ecoregion, country, territory, locality, habitat, parentEventID,
             decimalLatitude, decimalLongitude, verbatimDepth, year, month, day, eventDate, category) %>% 
    summarise(measurementValue = mean(measurementValue)) %>% 
    ungroup() %>% 
    # 5. Remove values greater than 100 (unlikely but included to avoid any issues later)
    filter(measurementValue <= 100)# %>% 
    # 6. Remove datasetID among top predictors for a given category
    #filter(!(category == "Algae" & datasetID %in% c("0015", "0104", "0151")))
  
  # Algae subcategories
  
  result_subcategory <- data %>% 
    # 1. Filter taxonomic level
    filter(category == "Algae" & subcategory != "Cyanobacteria") %>% 
    # 2. Sum of benthic cover per sampling unit (site, transect, quadrat) and category
    group_by(datasetID, region, subregion, ecoregion, country, territory, locality, habitat, parentEventID,
             decimalLatitude, decimalLongitude, verbatimDepth, year, month, day, eventDate, eventID, category, subcategory) %>% 
    summarise(measurementValue = sum(measurementValue)) %>% 
    ungroup() %>% 
    # 3. Remove NA subcategories
    function_remove_na(data = ., remove_na = remove_na) %>% 
    # 4. Regenerate 0 values
    function_regenerate_zero(data = ., regenerate_zero = regenerate_zero) %>%
    # 5. Average of benthic cover per transect (i.e. mean of photo-quadrats)
    # This avoid getting semi-quantitative data (e.g. when there is only 10 points per photo-quadrat)
    group_by(datasetID, region, subregion, ecoregion, country, territory, locality, habitat, parentEventID,
             decimalLatitude, decimalLongitude, verbatimDepth, year, month, day, eventDate, category) %>% 
    summarise(measurementValue = mean(measurementValue)) %>% 
    ungroup() %>% 
    # 6. Remove values greater than 100 (unlikely but included to avoid any issues later)
    filter(measurementValue <= 100)#%>% 
    # 7. Remove datasetID among top predictors for a given category
    #filter(!(category == "Macroalgae" & datasetID %in% c()))
  
  # Combine data
  
  result <- bind_rows(result_category, result_subcategory)
  
  return(result)
  
}
