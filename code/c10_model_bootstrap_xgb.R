# 1. Load packages ----

library(tidyverse) # Core tidyverse packages
library(tidymodels) # Core tidymodels packages
library(DALEX)
library(DALEXtra)
library(caret)
library(xgboost)
library(vip)
library(future)
library(furrr)

options(future.globals.maxSize = 100000*1024^2) # 100 Gb
plan(strategy = multisession, workers = 6)

# 2. Load data ----

load("data/11_model-data/data_benthic_prepared.RData")
load("data/11_model-data/data_predictors_pred.RData")
load("data/12_model-output/model_tuning_xgb.RData")

data_benthic <- data_benthic %>% 
  select(-datasetID, -month, -parentEventID, -verbatimDepth, -reef_type)

data_predictors_pred <- data_predictors_pred %>% 
  select(-datasetID, -month, -parentEventID, -verbatimDepth, -reef_type)

model_hyperparams <- tuning_results$model_hyperparams

# 3. Create the function ----

model_bootstrap <- function(category_i, bootstrap_i, pdp){
  
  start <- Sys.time()
  
  # 1. Data preparation
  
  ## 1.1 Filter the category
  
  data_split <- data_benthic %>% 
    filter(category == category_i) %>% 
    select(-category)
  
  ## 1.2 Sample with replacement (for bootstrap)
  
  data_split <- data_split %>% 
    slice_sample(., prop = runif(1, min = 0.01, max = 1), replace = TRUE)
  
  ## 1.3 Split into training and testing data
  
  data_split <- initial_split(data_split, prop = 3/4)
  data_train <- training(data_split)
  data_test <- testing(data_split)
  
  # 2. Fit the model
  
  ## 2.1 Define the recipe
  
  tune_recipe <- recipe(measurementValue ~ ., data = data_train) %>% 
    # Selecting the correct step_x() is very important
    #step_unknown(all_nominal_predictors()) %>%
    #step_novel(all_nominal_predictors()) %>%
    #step_dummy(all_nominal_predictors()) %>% # Those lines are for qualitative predictors
    update_role(c("region", "subregion", "ecoregion", "country", "territory"),
                new_role = "ID") %>% # Variables not used as predictors
    step_zv(all_predictors()) # Remove zero variance predictors (i.e. uninformative predictors)
  
  ## 2.2 Define the model
  
  model_hyperparams_i <- model_hyperparams %>% 
    filter(category == category_i)
  
  tune_model <- boost_tree(learn_rate = model_hyperparams_i$learn_rate,
                           trees = model_hyperparams_i$trees,
                           tree_depth = model_hyperparams_i$tree_depth,
                           min_n = model_hyperparams_i$min_n, 
                           loss_reduction = model_hyperparams_i$loss_reduction) %>% # Model type
    set_engine("xgboost") %>% # Model engine
    set_mode("regression") # Model mode
  
  ## 2.3 Define the workflow
  
  tune_workflow <- workflow() %>%
    add_recipe(tune_recipe) %>% 
    add_model(tune_model)
  
  ## 2.4 Fit the final model
  
  final_model <- last_fit(tune_workflow, data_split)
  
  final_fitted <- final_model$.workflow[[1]]
  
  # 3. Model outputs
  
  ## 3.1 Model performance
  
  model_performance <- collect_metrics(final_model) %>% 
    select(-".estimator", -".config") %>% 
    pivot_wider(names_from = ".metric", values_from = ".estimate") %>% 
    mutate(category = category_i, bootstrap = bootstrap_i)
  
  ## 3.2 Variable importance
  
  result_vip <- final_model %>% 
    extract_fit_parsnip() %>% 
    vip(num_features = 100) %>% 
    .$data %>% 
    rename(predictor = 1, importance = 2) %>% 
    mutate(category = category_i, bootstrap = bootstrap_i)
  
  ## 3.3 Partial Dependence Plots
  
  if(pdp == TRUE){
    
    model_explain <- explain_tidymodels(model = tune_workflow %>% fit(data = data_train), 
                                        data = dplyr::select(data_train, -measurementValue), 
                                        y = data_train$measurementValue)
    
    result_pdp <- model_profile(explainer = model_explain,
                                N = 2000, # Number of observations to use
                                center = FALSE,
                                type = "partial",
                                variable_splits_type = "uniform",
                                variables = colnames(data_predictors_pred)) %>% 
      .$agr_profiles %>% 
      as_tibble(.) %>% 
      select(-"_label_", -"_ids_") %>% 
      rename(x = "_x_", y_pred = "_yhat_", predictor = "_vname_") %>% 
      mutate(category = category_i, bootstrap = bootstrap_i)
    
  }
  
  # 4. Predictions
  
  ## 4.1 Predict values for new set of predictors
  
  results_predicted <- data_predictors_pred %>% 
    mutate(measurementValuepred = predict(final_fitted, data_predictors_pred)$.pred)
  
  ## 4.2 Summarise predictions over space and time
  
  ### 4.2.1 Global
  
  results_global <- results_predicted %>% 
    group_by(year) %>% 
    summarise(mean = mean(measurementValuepred)) %>% 
    ungroup() %>% 
    mutate(category = category_i, bootstrap = bootstrap_i, region = "All")
  
  ### 4.2.2 Region
  
  results_region <- results_predicted %>% 
    group_by(year, region) %>% 
    summarise(mean = mean(measurementValuepred)) %>% 
    ungroup() %>% 
    mutate(category = category_i, bootstrap = bootstrap_i)
  
  ### 4.2.3 Subregion
  
  results_subregion <- results_predicted %>% 
    group_by(year, region, subregion) %>% 
    summarise(mean = mean(measurementValuepred)) %>% 
    ungroup() %>% 
    mutate(category = category_i, bootstrap = bootstrap_i)
  
  ### 4.2.4 Ecoregion
  
  results_ecoregion <- results_predicted %>% 
    group_by(year, region, subregion, ecoregion) %>% 
    summarise(mean = mean(measurementValuepred)) %>% 
    ungroup() %>% 
    mutate(category = category_i, bootstrap = bootstrap_i)
  
  ### 4.2.5 Country
  
  results_country <- results_predicted %>% 
    group_by(year, country) %>% 
    summarise(mean = mean(measurementValuepred)) %>% 
    ungroup() %>% 
    mutate(category = category_i, bootstrap = bootstrap_i)
  
  ### 4.2.6 Territory
  
  results_territory <- results_predicted %>% 
    group_by(year, territory) %>% 
    summarise(mean = mean(measurementValuepred)) %>% 
    ungroup() %>% 
    mutate(category = category_i, bootstrap = bootstrap_i)
  
  ### 4.2.7 Combine results 
  
  result_trends <- bind_rows(results_global, results_region, results_subregion,
                             results_ecoregion, results_country, results_territory)
  
  # 4.3 Raw predictions (per site)
  
  if(bootstrap_i <= 2){ # Return only the results for the first two iterations
    
    results_predicted <- results_predicted %>% 
      select(year, decimalLatitude, decimalLongitude, measurementValuepred) %>% 
      mutate(category = category_i, bootstrap = bootstrap_i)
    
  }else{
    
    results_predicted <- tibble(year = NA, decimalLatitude = NA,
                                decimalLongitude = NA, measurementValuepred = NA,
                                category = category_i, bootstrap = bootstrap_i)
    
  }
  
  # 5. Running time
  
  model_time <- tibble(category = category_i,
                       bootstrap = bootstrap_i,
                       start = start,
                       end = Sys.time()) %>%
    mutate(duration = end - start)
  
  # 6. Return the results
  
  model_description <- model_hyperparams_i %>% 
    mutate(model = "XGBoost")
  
  model_pred_obs <- tuning_results$result_pred_obs %>% 
    mutate(model = "XGBoost")
  
  if(pdp == TRUE){
    
    return(lst(model_description,
               model_time,
               model_performance,
               model_pred_obs,
               results_predicted,
               result_vip,
               result_pdp,
               result_trends))
    
  }else{
    
    return(lst(model_description,
               model_time,
               model_performance,
               model_pred_obs,
               results_predicted,
               result_vip,
               result_trends))
    
  }
  
}

# 4. Map over the function ----

## 4.1 Hard coral ----

model_results <- future_map(1:10, ~model_bootstrap(category_i = "Hard coral",
                                                   bootstrap_i = .,
                                                   pdp = FALSE),
                            .options = furrr_options(seed = TRUE)) %>% 
  map_df(., ~ as.data.frame(map(.x, ~ unname(nest(.))))) %>% 
  map(., bind_rows) %>% 
  map(., ~distinct(.x))

save(model_results, file = "data/12_model-output/model_results_hard-coral_xgb_separated.RData")

## 4.2 Algae ----

model_results <- future_map(1:10, ~model_bootstrap(category_i = "Algae",
                                                   bootstrap_i = .,
                                                   pdp = FALSE),
                            .options = furrr_options(seed = TRUE)) %>% 
  map_df(., ~ as.data.frame(map(.x, ~ unname(nest(.))))) %>% 
  map(., bind_rows) %>% 
  map(., ~distinct(.x))

save(model_results, file = "data/12_model-output/model_results_algae_xgb_separated.RData")

## 4.3 Turf algae ----

model_results <- future_map(1:10, ~model_bootstrap(category_i = "Turf algae",
                                                   bootstrap_i = .,
                                                   pdp = FALSE),
                            .options = furrr_options(seed = TRUE)) %>% 
  map_df(., ~ as.data.frame(map(.x, ~ unname(nest(.))))) %>% 
  map(., bind_rows) %>% 
  map(., ~distinct(.x))

save(model_results, file = "data/12_model-output/model_results_turf-algae_xgb_separated.RData")

## 4.4 Coralline algae ----

model_results <- future_map(1:10, ~model_bootstrap(category_i = "Coralline algae",
                                                   bootstrap_i = .,
                                                   pdp = FALSE),
                            .options = furrr_options(seed = TRUE)) %>% 
  map_df(., ~ as.data.frame(map(.x, ~ unname(nest(.))))) %>% 
  map(., bind_rows) %>% 
  map(., ~distinct(.x))

save(model_results, file = "data/12_model-output/model_results_coralline-algae_xgb_separated.RData")

## 4.5 Macroalgae ----

model_results <- future_map(1:10, ~model_bootstrap(category_i = "Macroalgae",
                                                   bootstrap_i = .,
                                                   pdp = FALSE),
                            .options = furrr_options(seed = TRUE)) %>% 
  map_df(., ~ as.data.frame(map(.x, ~ unname(nest(.))))) %>% 
  map(., bind_rows) %>% 
  map(., ~distinct(.x))

save(model_results, file = "data/12_model-output/model_results_macroalgae_xgb_separated.RData")

## 4.6 Other fauna ----

model_results <- future_map(1:10, ~model_bootstrap(category_i = "Other fauna",
                                                   bootstrap_i = .,
                                                   pdp = FALSE),
                            .options = furrr_options(seed = TRUE)) %>% 
  map_df(., ~ as.data.frame(map(.x, ~ unname(nest(.))))) %>% 
  map(., bind_rows) %>% 
  map(., ~distinct(.x))

save(model_results, file = "data/12_model-output/model_results_other-fauna_xgb_separated.RData")
