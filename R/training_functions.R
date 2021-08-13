#' @importFrom magrittr %>%
#' @importFrom foreach %do%
#' @importFrom doRNG %dorng%

sample_max <- function(x, size) {
  if (!is.data.frame(x)) stop("`x` must be a data.frame")
  if (!is.integer(size) || length(size) != 1) stop("`size` must be an integer vector of length 1")

  if (NROW(x) <= size) return(x)
  else return(x[sample(1:NROW(x), size = size, replace = FALSE), , drop = FALSE])
}

extract_mcc_performance <- function(model) {

  if (is.null(model)) return(NULL)
  if (!class(model) == "train") stop("`model` must be a caret `train` object")
  preds <- predict(model)
  positive_label <- model$trainingData$.outcome %>% levels %>% magrittr::extract(1)

  return(ModelMetrics::mcc(as.integer(model$trainingData$.outcome == positive_label), as.integer(preds == positive_label), 0.5))
}

# Find index of model with best mean OOB error
inf_NULLs <- function(x) {
  if (!is.list(x)) stop("`x` must be a list")
  if (!all(sapply(x, function(i) {is.null(i) | is.numeric(i)}))) stop("`x` must have all numeric or NULL components")

  x[sapply(x, is.null)] <- -Inf
  x[sapply(x, is.nan)] <- -Inf

  return(x)
}

get_model_performance_estimates <- function(models) {
  perf <- purrr::map_depth(models, .f = function(model) extract_mcc_performance(model), .depth = 3)
  perf <- purrr::map_depth(perf, . %>% inf_NULLs, .depth = 2)

  return(perf)
}

get_best_model_index <- function(perf_tree) {

  # The estimates are in lists with one value per completed dataset.
  # Unlisting that list before mean gives mean the desired input type (numeric vector).
  mean_perf <- purrr::map_depth(perf_tree, . %>% unlist %>% mean, .depth = 2)
  best_model_ix <- purrr::map_int(mean_perf, which.max)

  return(best_model_ix)
}


select_from_tree <- function(tree, ix) {

  # Extract the best models, best imputers and their best hyperparameters for each method
  selected <- purrr::map(enumerate(ix), . %>% with(tree[[name]][[value]]))

  return(selected)
}

select_hyperparams <- function(hyperparams, imputers, ix) {
  if (!all(names(ix) %in% names(hyperparams))) stop("Index names do not match hyperparameter names")

  best_hyperparams <- purrr::map(enumerate(ix), . %>% with(hyperparams[[name]][value, , drop = FALSE]))

  return(best_hyperparams)
}
bind_imp_parameters_for_reuse <- function(hyperparams, imputers) {
  
  # For the methods that properly accept them, we should store parameters from the training set
  # to use in imputing the test set.
  for (h in names(hyperparams)) {
    # kNN allows use of another dataset to find the neighbors. Thus, let's store the completed dataset.
    if (h == "knnImputation") {
      if (is.null(imputers[[h]][["completed_datasets"]][[1]])) stop(paste("Recycling parameters for method ", h, " were NULL"))
      attr(hyperparams[[h]], "imputation_reuse_parameters") <- imputers[[h]][["completed_datasets"]][[1]]
    }
    # E.g. median imputations should impute the test set with the median of the training set instead of
    # the median of the test set. Thus such values must be stored.
    if (h %in% SINGLE_IMP_METHODS) {
      if (is.null(attr(imputers[[h]][["completed_datasets"]][[1]], "imputation_reuse_parameters"))) stop(paste("Recycling parameters for method ", h, " were NULL"))
      attr(hyperparams[[h]], "imputation_reuse_parameters") <- attr(imputers[[h]][["completed_datasets"]][[1]],"imputation_reuse_parameters")
    }
  }
  return(hyperparams)
}

select_best <- function(models, imputations, hyperparameters) {

  if (!all(names(models) %in% names(hyperparameters))) stop("Model names do not match hyperparameter names")
  if (!all(names(models) %in% names(imputations))) stop("Model names do not match imputer names")
  if (!all(names(imputations) %in% names(hyperparameters))) stop("Imputer names do not match hyperparameter names")
  # Get the performance estimate from each leaf of the tree (i.e. all trained models)
  perf <- get_model_performance_estimates(models)

  best_model_ix <- get_best_model_index(perf)

  models <- select_from_tree(models, best_model_ix)
  imputers <- select_from_tree(imputations, best_model_ix)
  hyperparams <- select_hyperparams(hyperparameters, imputers, best_model_ix)
  hyperparams <- bind_imp_parameters_for_reuse(hyperparams, imputers)

  return(list(models = models, imputers = imputers, hyperparams = hyperparams))
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
train_xgboost <- function(data, outcome, control, grid) {

  if (is.null(data)) {
    flog.pid.info("Dataset was NULL; returning NULL")
    return(NULL)
  }

  model <- tryCatch({
    caret::train(x = data,
                 y = outcome,
                 method = "xgbTree",
                 preProcess = c("center", "scale"),
                 trControl = control,
                 tuneGrid = grid)
  }, error = function(e) {
    flog.pid.debug(e)
    return(NULL)
  })

  return(model)
}

train_lr <- function(data, outcome, control, grid) {

  if(is.null(data)) {
    flog.pid.info("Dataset was NULL; returning NULL")
    return(NULL)
  }

  lr_model <- tryCatch({
    caret::train(x = data,
                 y = outcome,
                 method = "glm",
                 preProcess = c("center", "scale"),
                 trControl = control)
  }, error = function(e) {
    flog.pid.debug(e)
    return(NULL)
  })

  return(lr_model)
}

loop_models <- function(training_function, classifier_name, imputations, control, grid, outcome, seed) {
  if (!is.function(training_function)) stop("`training_function` must be a function")
  if (!is.character(classifier_name)) stop("`classifier_name` must be a character vector")
  #if (!class(control) == "trainControl") stop("`control` must be a `trainControl` object")
  if (!is.data.frame(grid)) stop("`grid` must be a data.frame")
  if (!is.factor(outcome)) stop("`outcome` must be a factor")
  if (!is.numeric(seed) || length(seed) != 1) stop("`seed` must be a numeric vector of length 1")

  foreach::foreach(method = enumerate(imputations), .options.RNG = seed) %dorng% {
    flog.pid.info("Starting execution of %s on datasets produced by %s", classifier_name, method$name)
    foreach::foreach(mi_iter = method$value) %do% {
      model_per_dataset <- foreach::foreach(data = mi_iter$completed_datasets) %do% training_function(data = data, outcome = outcome, control = control, grid = grid)
      return(model_per_dataset %>% magrittr::set_names(names(mi_iter$completed_datasets)))
    } %>% magrittr::set_names(names(method$value))
  } %>% magrittr::set_names(names(imputations))
}
