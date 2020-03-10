
sample_max <- function(x, size) {
  if (NROW(x) < size) return(x)
  else return(x[sample(1:NROW(x), size = size, replace = FALSE), , drop = FALSE])
}

# extract_oob_performance <- function(model) {
#   model$finalModel$err.rate[, "OOB"] %>% tail(1)
# }
extract_mcc_performance <- function(model, outcome) {
  ModelMetrics::mcc(as.integer(outcome == "positive"), 1 - model$finalModel$fitted.values, 0.5)
}

# Find index of model with best mean OOB error
inf_NULLs <- function(x) {
  x[sapply(x, is.null)] <- -Inf
  x[sapply(x, is.nan)] <- -Inf
  return(x)
}

select_best <- function(models, imputations, outcome, hyperparams) {
  
  # Get the error estimate from each leaf of the tree (i.e. all trained models)
  perf <- map_depth(models, .f = function(model) extract_mcc_performance(model, outcome), .depth = 3)
  perf <- map_depth(perf, . %>% inf_NULLs, .depth = 2)
  
  # The estimates are in lists with one value per completed dataset.
  # Unlisting that list before mean gives mean the desired input type (numeric vector).
  mean_perf <- map_depth(perf, . %>% unlist %>% mean, .depth = 2)
  best_model_ix <- map_int(mean_perf, which.max)
  
  # Extract the best models, best imputers and their best hyperparameters for each method
  best_models <- map(enumerate(best_model_ix), . %>% with(models[[name]][[value]]))
  best_imputers <- map(enumerate(best_model_ix), . %>% with(imputations[[name]][[value]]))
  best_hyperparams <- map(enumerate(best_model_ix), . %>% with(hyperparams[[name]][value, ]))
  
  # For the methods that properly accept them, we should store parameters from the training set
  # to use in imputing the test set.
  for (h in names(best_hyperparams)) {
    # kNN allows use of another dataset to find the neighbors. Thus, let's store the completed dataset.
    if (h == "knnImputation") {
      attr(best_hyperparams[[h]], "imputation_estimates") <- best_imputers[[h]][["completed_datasets"]][[1]]
    }
    # E.g. median imputations should impute the test set with the median of the training set instead of
    # the median of the test set. Thus such values must be stored.
    if (h %in% names(single_value_imputation_hyperparameter_grids)) {
      attr(best_hyperparams[[h]], "imputation_estimates") <- attr(best_imputers[[h]][["completed_datasets"]][[1]], "imputation_estimates")
    }
  }
  
  return(list(ix = best_model_ix, models = best_models, imputers = best_imputers, hyperparams = best_hyperparams))
}

train_rf <- function(data, outcome, control, grid) {
  
  if(is.null(data)) {
    flog.pid.info("Dataset was NULL; returning NULL")
    return(NULL)
  }
  
  rf_model <- tryCatch({
    caret::train(x = data,
                 y = outcome,
                 method = "rf",
                 preProcess = c("center", "scale"),
                 trControl = control,
                 tuneGrid = grid)
  }, error = function(e) { 
    flog.pid.debug(e)
    return(NULL)
  })
  
  return(rf_model)
}
train_lr <- function(dataset, outcome, control, grid) {
  
  lr_model <- NULL
  tryCatch({
    lr_model <- caret::train(x = dataset,
                             y = outcome,
                             method = "glm",
                             preProcess = c("center", "scale"),
                             trControl = control)
  }, error = function(e) flog.pid.debug(e))
  
  return(lr_model)
}

loop_models <- function(training_function, classifier_name, imputations, control, grid, outcome, seed) {
  foreach(method = enumerate(imputations), .options.RNG = seed) %dorng% {
    flog.pid.info("Starting execution of %s on datasets produced by %s", classifier_name, method$name)
    foreach(mi_iter = method$value) %do% {
      model_per_dataset <- foreach(data = mi_iter$completed_datasets) %do% training_function(data, outcome, control, grid)
      return(model_per_dataset %>% set_names(names(mi_iter$completed_datasets)))
    } %>% set_names(names(method$value))
  } %>% set_names(names(imputations))
}
