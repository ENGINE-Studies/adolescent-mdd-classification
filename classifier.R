require(tidymodels)
require(parsnip)
require(yardstick)
require(vip)
require(ggplot2)

## set paths
path_data <- '/path/to/baseline/data/'
path_out <- '/path/to/output/'

## set parameters
n_perm <- 1000
n_inner <- 10
n_outer <- 10

## Prepare data
data <- read.csv(paste0(path_data, 'MDD_HC_Baseline_Coherence.csv'))

data_clf <- data %>%
  select(-X, -Study_ID, -Session, -Epoch_no, -cdrs_total)

data_clf$MDD <- as.factor(data_clf$MDD)

# resample
set.seed(1)
folds <- vfold_cv(data_clf, v=n_inner, repeats = n_outer, strata = MDD)

# Model specification
rf_mod <- rand_forest(mode='classification', trees = 1000) %>%
  set_engine('ranger', importance = 'permutation')

rec <- 
  recipe(MDD ~ ., data = data_clf)

rf_workflow <- 
  workflow() %>% 
  add_recipe(rec) %>% 
  add_model(rf_mod)

ctrl <- control_grid(verbose = FALSE, save_pred = TRUE)

metric_list <- metric_set(roc_auc, pr_auc, sens, spec, ppv, npv)

set.seed(1)
rf_fit <- rf_workflow %>%
  fit_resamples(folds, metrics = metric_list, control = ctrl)

collect_metrics(rf_fit)

## Permutation
permuted_performance_list <- lapply(1:n_perm, function(p){
  
  print(p)
  
  data_clf_perm <- data_clf
  set.seed(p)
  data_clf_perm$MDD <- sample(data_clf_perm$MDD, replace = FALSE)
  
  # resample
  set.seed(p)
  folds_perm <- vfold_cv(data_clf_perm, v=n_inner, repeats = n_outer, strata = MDD)
  
  # Model specification
  rf_mod_perm <- rand_forest(mode='classification', trees = 1000) %>%
    set_engine('ranger', importance = 'permutation')
  
  rec_perm <- 
    recipe(MDD ~ ., data = data_clf_perm)
  
  rf_workflow_perm <- 
    workflow() %>% 
    add_recipe(rec_perm) %>% 
    add_model(rf_mod_perm)
  
  set.seed(p)
  rf_fit_perm <- rf_workflow_perm %>%
    fit_resamples(folds_perm, metrics = metric_list)
  
  permutation_metrics <- collect_metrics(rf_fit_perm)
    
  return(permutation_metrics)
  
})  

# aggregate permutation distributions for all metrics
permuted_performance_df <- do.call(rbind, permuted_performance_list)
permuted_performance_by_metric <- split(permuted_performance_df, permuted_performance_df$.metric)

save.image(paste0(path_out, 'results.Rdata'))



