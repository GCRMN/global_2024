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
plan(strategy = multisession, workers = 4)

# 2. Load data ----

load("data/09_model-data/data_benthic_prepared.RData")
load("data/09_model-data/data_predictors_pred.RData")

# 3. Create the function ----

hyperparam_tuning <- function(category_i){
  
  hp_start <- Sys.time()
  
  # 1. Data preparation
  
  ## 1.1 Filter the category
  
  data_split <- data_benthic %>% 
    filter(category == category_i) %>% 
    select(-category)
  
  ## 1.2 Split into training and testing data
  
  data_split <- initial_split(data_split, prop = 3/4)
  data_train <- training(data_split)
  data_test <- testing(data_split)
  
  # 2. Hyper parameters tuning
  
  ## 2.1 Define the recipe
  
  tune_recipe <- recipe(measurementValue ~ ., data = data_train) %>% 
    step_novel("territory") %>% 
    step_dummy(all_nominal_predictors())
  
  ## 2.2 Define the model
  
  tune_model <- boost_tree(learn_rate = tune(),
                           trees = tune(), 
                           min_n = tune(), 
                           tree_depth = tune()) %>% # Model type
    set_engine("xgboost") %>% # Model engine
    set_mode("regression") # Model mode
  
  ## 2.3 Define the workflow
  
  tune_workflow <- workflow() %>%
    add_recipe(tune_recipe) %>% 
    add_model(tune_model)
  
  tune_grid_values <- grid_space_filling(learn_rate(),
                                         trees(),
                                         tree_depth(),
                                         min_n(),
                                         size = 15,
                                         type = "max_entropy")
  
  ## 2.4 Run the hyper parameters tuning
  
  tuned_results <- tune_grid(tune_workflow,
                             resamples = vfold_cv(data_train, v = 5),
                             grid = tune_grid_values)
  
  ## 2.5 Get best set of parameters
  
  model_hyperparams <- select_best(tuned_results, metric = "rmse") %>% 
    select(-".config") %>% 
    as_tibble(.) %>%
    mutate(nb_training = nrow(data_train),
           nb_testing = nrow(data_test),
           grid_size = nrow(tune_grid_values)) %>% 
    mutate(category = category_i)
  
  # 3. Final model
  
  ## 3.1 Redefine the model (with hyper parameters)
  
  tune_model <- boost_tree(learn_rate = model_hyperparams$learn_rate,
                           trees = model_hyperparams$trees, 
                           min_n = model_hyperparams$min_n, 
                           tree_depth = model_hyperparams$tree_depth) %>% # Model type
    set_engine("xgboost") %>% # Model engine
    set_mode("regression") # Model mode
  
  ## 3.2 Redefine the workflow
  
  tune_workflow <- workflow() %>%
    add_recipe(tune_recipe) %>% 
    add_model(tune_model)
  
  ## 3.3 Fit the final model
  
  final_model <- tune_workflow %>%
    last_fit(data_split)
  
  final_fitted <- final_model$.workflow[[1]]
  
  ## 3.4 Model performance
  
  model_performance <- collect_metrics(final_model) %>% 
    select(-".estimator", -".config") %>% 
    pivot_wider(names_from = ".metric", values_from = ".estimate") %>% 
    mutate(category = category_i)
  
  ## 3.5 Predicted vs Observed
  
  result_pred_obs <- data_test %>% 
    mutate(yhat = predict(final_fitted, data_test)$.pred) %>% 
    rename(y = measurementValue) %>% 
    select(area, y, yhat) %>% 
    mutate(category = category_i)
  
  # 4. Running time
  
  model_time <- tibble(hp_start = hp_start,
                       hp_end = Sys.time()) %>%
    mutate(hp_duration = hp_end - hp_start)
  
  # 5. Return the results
  
  return(lst(model_hyperparams,
             model_performance,
             model_time,
             result_pred_obs))
  
}

# 4. Map over the function ----

tuning_results <- future_map(c("Algae", "Hard coral", "Other fauna",
                               "Coralline algae", "Macroalgae", "Turf algae"),
                             ~hyperparam_tuning(category_i = .)) %>% 
  map_df(., ~ as.data.frame(map(.x, ~ unname(nest(.))))) %>% 
  map(., bind_rows)

# 5. Export the data ----

save(tuning_results, file = "data/10_model-output/model_tuning_xgb.RData")
