# 1. Load packages ----

library(tidyverse)
library(tidymodels)
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

data_benthic <- data_benthic %>% 
  select(-month, -parentEventID, -verbatimDepth)

data_predictors_pred <- data_predictors_pred %>% 
  select(-month, -parentEventID, -verbatimDepth)

# 3. Create the function ----

model_xgboost <- function(category_i, bootstrap_i, vfolds = 3, gridsize = 10){
  
  full_start <- Sys.time()
  
  ## 1.1 Filter the category
  
  data_benthic_i <- data_benthic %>% 
    filter(category == category_i) %>% 
    select(-category)
  
  ## 1.2 Sample with replacement by datasetID
  
  data_boot <- data_benthic_i %>% 
    group_by(datasetID) %>% 
    group_modify(~ .x[sample.int(nrow(.x), size = nrow(.x), replace = TRUE), ]) %>%
    ungroup()
  
  ## 1.3 Split into training and testing data
  
  data_split <- initial_split(data_boot, prop = 3/4)
  data_train <- training(data_split)
  data_test <- testing(data_split)
  
  ## 1.4 Remove useless objects
  
  rm(data_benthic_i, data_boot)
  
  # 2. HP tuning ############################################
  
  hp_start <- Sys.time()
  
  ## 2.1 Define the recipe
  
  tune_recipe <- recipe(measurementValue ~ ., data = data_train) %>% 
    # Selecting the correct step_x() is very important
    #step_unknown(all_nominal_predictors()) %>%
    #step_novel(all_nominal_predictors()) %>%
    #step_dummy(all_nominal_predictors()) %>% # Those lines are for qualitative predictors
    #step_zv(all_predictors()) # Remove zero variance predictors (i.e. uninformative predictors)
    update_role(c("region", "subregion", "ecoregion", "country", "territory", "datasetID"),
                new_role = "ID") # Variables not used as predictors
  
  # Check of the recipe
  #prep_rec <- prep(tune_recipe)
  #summary(prep_rec) # roles of each variables
  
  ## 2.2 Define the model
  
  tune_model <- boost_tree(learn_rate = tune(),
                           trees = tune(), 
                           tree_depth = tune(),
                           min_n = tune(), 
                           loss_reduction = tune()) %>% # Model type
    set_engine("xgboost") %>% # Model engine
    set_mode("regression") # Model mode
  
  ## 2.3 Define the workflow
  
  tune_workflow <- workflow() %>%
    add_recipe(tune_recipe) %>% 
    add_model(tune_model)
  
  ## 2.4 Define the grid
  
  tune_grid_values <- grid_space_filling(learn_rate(),
                                         trees(),
                                         tree_depth(),
                                         min_n(),
                                         loss_reduction(),
                                         size = gridsize,
                                         type = "max_entropy")
  
  ## 2.5 Run the tuning
  
  tuned_results <- tune_grid(tune_workflow,
                             resamples = vfold_cv(data_train, v = vfolds),
                             grid = tune_grid_values)
  
  ## 2.6 Get best set of parameters
  
  model_hyperparams <- select_best(tuned_results, metric = "rmse") %>% 
    select(-".config") %>% 
    as_tibble(.) %>%
    mutate(nb_training = nrow(data_train),
           nb_testing = nrow(data_test),
           grid_size = gridsize,
           v_folds = vfolds) %>% 
    mutate(category = category_i, bootstrap = bootstrap_i)
  
  hp_end <- Sys.time()
  
  # 3. Evaluation #########################################
  
  pred_start <- Sys.time()
  
  ## 3.1 Redefine the model (with hyper parameters)
  
  tune_model <- boost_tree(learn_rate = model_hyperparams$learn_rate,
                           trees = model_hyperparams$trees,
                           tree_depth = model_hyperparams$tree_depth,
                           min_n = model_hyperparams$min_n,
                           loss_reduction = model_hyperparams$loss_reduction) %>% # Model type
    set_engine("xgboost") %>% # Model engine
    set_mode("regression") # Model mode
  
  ## 3.2 Redefine the workflow
  
  tune_workflow <- workflow() %>%
    add_recipe(tune_recipe) %>% 
    add_model(tune_model)
  
  ## 3.3 Fit the final model
  
  final_model <- last_fit(tune_workflow, data_split)
  
  final_fitted <- final_model$.workflow[[1]]
  
  ## 3.4 Model performance
  
  model_performance <- collect_metrics(final_model) %>% 
    select(-".estimator", -".config") %>% 
    pivot_wider(names_from = ".metric", values_from = ".estimate") %>% 
    mutate(category = category_i, bootstrap = bootstrap_i)
  
  ## 3.5 Variable importance
  
  result_vip <- final_model %>% 
    extract_fit_parsnip() %>% 
    vip(num_features = 100) %>% 
    .$data %>% 
    rename(predictor = 1, importance = 2) %>% 
    mutate(category = category_i, bootstrap = bootstrap_i)
  
  ## 3.6 Predicted vs Observed
  
  result_pred_obs <- data_test %>% 
    mutate(yhat = predict(final_fitted, data_test)$.pred) %>% 
    rename(y = measurementValue) %>% 
    select(region, subregion, ecoregion, country, territory, datasetID, y, yhat) %>% 
    mutate(category = category_i, bootstrap = bootstrap_i)
  
  pred_end <- Sys.time()
  
  # 4. Predictions #########################################
  
  ## 4.1 Predict values for new set of predictors
  
  result_predicted <- data_predictors_pred %>% 
    mutate(measurementValuepred = predict(final_fitted, data_predictors_pred)$.pred)
  
  ## 4.2 Summarise predictions over space and time
  
  ### 4.2.1 Global
  
  results_global <- result_predicted %>% 
    group_by(year) %>% 
    summarise(mean = mean(measurementValuepred)) %>% 
    ungroup() %>% 
    mutate(category = category_i, bootstrap = bootstrap_i, level = "global")
  
  ### 4.2.2 Region
  
  results_region <- result_predicted %>% 
    group_by(year, region) %>% 
    summarise(mean = mean(measurementValuepred)) %>% 
    ungroup() %>% 
    mutate(category = category_i, bootstrap = bootstrap_i, level = "region") %>% 
    drop_na(region)
  
  ### 4.2.3 Subregion
  
  results_subregion <- result_predicted %>% 
    group_by(year, region, subregion) %>% 
    summarise(mean = mean(measurementValuepred)) %>% 
    ungroup() %>% 
    mutate(category = category_i, bootstrap = bootstrap_i, level = "subregion") %>% 
    drop_na(subregion)
  
  ### 4.2.4 Ecoregion
  
  results_ecoregion <- result_predicted %>% 
    group_by(year, region, subregion, ecoregion) %>% 
    summarise(mean = mean(measurementValuepred)) %>% 
    ungroup() %>% 
    mutate(category = category_i, bootstrap = bootstrap_i, level = "ecoregion") %>% 
    drop_na(ecoregion)
  
  ### 4.2.5 Country
  
  results_country <- result_predicted %>% 
    group_by(year, country) %>% 
    summarise(mean = mean(measurementValuepred)) %>% 
    ungroup() %>% 
    mutate(category = category_i, bootstrap = bootstrap_i, level = "country") %>% 
    drop_na(country)
  
  ### 4.2.6 Territory
  
  results_territory <- result_predicted %>% 
    group_by(year, territory) %>% 
    summarise(mean = mean(measurementValuepred)) %>% 
    ungroup() %>% 
    mutate(category = category_i, bootstrap = bootstrap_i, level = "territory") %>% 
    drop_na(territory)
  
  ### 4.2.7 Combine results
  
  result_trends <- bind_rows(results_global, results_region, results_subregion,
                             results_ecoregion, results_country, results_territory)
  
  # 5. Return the results #######################################
  
  model_time <- tibble(category = category_i,
                       bootstrap = bootstrap_i,
                       full_start = full_start,
                       hp_start = hp_start,
                       hp_end = hp_end,
                       pred_start = pred_start,
                       pred_end = pred_end,
                       full_end = Sys.time()) %>%
    mutate(full_duration = full_end - full_start,
           hp_duration = hp_end - hp_start,
           pred_duration = pred_end - pred_start)
  
  model_hyperparams <- model_hyperparams %>% 
    mutate(model = "XGBoost")
  
  return(lst(model_hyperparams,
             model_time,
             model_performance,
             result_pred_obs,
             result_vip,
             result_trends))
  
}

# 4. Map over the function ----

## 4.1 Hard coral ----

model_results <- future_map(1:20, ~model_xgboost(category_i = "Hard coral",
                                                 bootstrap_i = .),
                            .options = furrr_options(seed = TRUE)) %>% 
  map_df(., ~ as.data.frame(map(.x, ~ unname(nest(.))))) %>% 
  map(., bind_rows) %>% 
  map(., ~distinct(.x))

save(model_results, file = "data/12_model-output/model-results_hard-coral.RData")

## 4.2 Macroalgae ----

model_results <- future_map(1:20, ~model_xgboost(category_i = "Macroalgae",
                                                 bootstrap_i = .),
                            .options = furrr_options(seed = TRUE)) %>% 
  map_df(., ~ as.data.frame(map(.x, ~ unname(nest(.))))) %>% 
  map(., bind_rows) %>% 
  map(., ~distinct(.x))

save(model_results, file = "data/12_model-output/model-results_macroalgae.RData")

## 4.3 Turf algae ----

model_results <- future_map(1:20, ~model_xgboost(category_i = "Turf algae",
                                                 bootstrap_i = .),
                            .options = furrr_options(seed = TRUE)) %>% 
  map_df(., ~ as.data.frame(map(.x, ~ unname(nest(.))))) %>% 
  map(., bind_rows) %>% 
  map(., ~distinct(.x))

save(model_results, file = "data/12_model-output/model-results_turf-algae.RData")

## 4.4 Coralline algae ----

model_results <- future_map(1:20, ~model_xgboost(category_i = "Coralline algae",
                                                 bootstrap_i = .),
                            .options = furrr_options(seed = TRUE)) %>% 
  map_df(., ~ as.data.frame(map(.x, ~ unname(nest(.))))) %>% 
  map(., bind_rows) %>% 
  map(., ~distinct(.x))

save(model_results, file = "data/12_model-output/model-results_coralline-algae.RData")

## 4.5 Other fauna ----

model_results <- future_map(1:20, ~model_xgboost(category_i = "Other fauna",
                                                 bootstrap_i = .),
                            .options = furrr_options(seed = TRUE)) %>% 
  map_df(., ~ as.data.frame(map(.x, ~ unname(nest(.))))) %>% 
  map(., bind_rows) %>% 
  map(., ~distinct(.x))

save(model_results, file = "data/12_model-output/model-results_other-fauna.RData")

## 4.6 Algae ----

model_results <- future_map(1:20, ~model_xgboost(category_i = "Algae",
                                                 bootstrap_i = .),
                            .options = furrr_options(seed = TRUE)) %>% 
  map_df(., ~ as.data.frame(map(.x, ~ unname(nest(.))))) %>% 
  map(., bind_rows) %>% 
  map(., ~distinct(.x))

save(model_results, file = "data/12_model-output/model-results_algae.RData")
