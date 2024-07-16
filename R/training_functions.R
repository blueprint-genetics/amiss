
#' Take a sample from list without replacement. If list contains fewer than `size` values, return original list.
#'
#' @param x List to sample from
#' @param size Maximum size of output list
#'
#' @return Sample without replacement from `x` of length at most `size`.
#' @export
sample_max <- function(x, size) {
  if (!is.data.frame(x)) stop("`x` must be a data.frame")
  if (!is.integer(size) || length(size) != 1) stop("`size` must be an integer vector of length 1")

  if (NROW(x) <= size) return(x)
  else return(x[sample(1:NROW(x), size = size, replace = FALSE), , drop = FALSE])
}


#' Extract MCC estimate from a `caret::train` object
#'
#' @param model A `caret::train` object
#'
#' @return Training set performance described via MCC
#'
#' @importFrom magrittr %>%
#' @importFrom foreach %do%
#' @importFrom doRNG %dorng%
#' @export
extract_mcc_performance <- function(model) {

  if (is.null(model)) return(NULL)
  if (!class(model) == "train") stop("`model` must be a caret `train` object")
  preds <- predict(model)
  positive_label <- model$trainingData$.outcome %>% levels %>% magrittr::extract(1)

  return(ModelMetrics::mcc(as.integer(model$trainingData$.outcome == positive_label), as.integer(preds == positive_label), 0.5))
}

#' Replace NULL values with -Inf in list
#'
#' @param x A list of numeric values
#'
#' @return `x` but with NULL elements replaced with `-Inf`
#' @export
inf_NULLs <- function(x) {
  if (!is.list(x)) stop("`x` must be a list")
  if (!all(sapply(x, function(i) {is.null(i) | is.numeric(i)}))) stop("`x` must have all numeric or NULL components")

  x[sapply(x, is.null)] <- -Inf
  x[sapply(x, is.nan)] <- -Inf

  return(x)
}

#' Get training set performance estimates as MCC from model tree
#'
#' Models for which performance can't be estimated get an estimate of `-Inf`.
#'
#' @param models A tree of `caret::model` objects
#'
#' @return A tree of performance estimates whose structure matches that of the input
#' @export
get_model_performance_estimates <- function(models) {
  perf <- purrr::map_depth(models, .f = function(model) extract_mcc_performance(model), .depth = 3)
  perf <- purrr::map_depth(perf, . %>% inf_NULLs, .depth = 2)

  return(perf)
}

#' Get index of subtree with best mean error
#'
#' @param perf_tree Tree of performance estimates
#'
#' @return Index of model subtree with best mean error
#' @export
get_best_model_index <- function(perf_tree) {

  # The estimates are in lists with one value per completed dataset.
  # Unlisting that list before mean gives mean the desired input type (numeric vector).
  mean_perf <- purrr::map_depth(perf_tree, . %>% unlist %>% mean, .depth = 2)
  best_model_ix <- purrr::map_int(mean_perf, which.max)

  return(best_model_ix)
}

#' Select subtree from tree
#'
#' @param tree Tree to select from
#' @param ix List mapping a name to a numeric index
#'
#' @return Selected subtree
#' @export
select_from_tree <- function(tree, ix) {

  selected <- purrr::map(enumerate(ix), . %>% with(tree[[name]][[value]]))

  return(selected)
}

#' Select a subtree from a tree of hyperparameters
#'
#' @param hyperparams A tree of hyperparameters
#' @param ix List mapping a name to a numeric index
#'
#' @return List containing data.frames containing selected hyperparameters
#' @export
select_hyperparams <- function(hyperparams, ix) {
  if (!all(names(ix) %in% names(hyperparams))) stop("Index names do not match hyperparameter names")

  best_hyperparams <- purrr::map(enumerate(ix), . %>% with(hyperparams[[name]][value, , drop = FALSE]))

  return(best_hyperparams)
}

#' Bind imputation parameters to an imputation model tree
#'
#' @param hyperparams Hyperparameter tree in which to bind parameters
#' @param imputers Imputation model tree from which to obtain training set parameter estimates for relevant imputation models
#'
#' @return `hyperparams` with parameters bound
#' @export
bind_imp_parameters_for_reuse <- function(hyperparams, imputers) {

  # For the methods that properly accept them, we should store parameters from the training set
  # to use in imputing the test set.
  for (h in names(hyperparams)) {
    # E.g. median imputations should impute the test set with the median of the training set instead of
    # the median of the test set. Thus such values must be stored.
    if (h %in% SINGLE_IMP_METHODS) {
      if (is.null(attr(imputers[[h]][["completed_datasets"]][[1]], "imputation_reuse_parameters"))) stop(paste("Recycling parameters for method ", h, " were NULL"))
      attr(hyperparams[[h]], "imputation_reuse_parameters") <- attr(imputers[[h]][["completed_datasets"]][[1]],"imputation_reuse_parameters")
    }
  }
  return(hyperparams)
}

#' Select best models from a model tree as well as corresponding imputation hyperparameters and imputation models
#'
#' Additionally, training set estimates of parameters for single value imputation methods are bound to the
#' hyperparameter tree.
#'
#' @param models Classifier model tree
#' @param imputations Imputation model tree
#' @param hyperparameters Imputation hyperparameter tree
#'
#' @return List with selected subtrees of the classifier model tree, imputation model tree and the hyperparameter tree
#' as elements.
#' @export
select_best <- function(models, imputations, hyperparameters) {

  if (!all(names(models) %in% names(hyperparameters))) stop("Model names do not match hyperparameter names")
  if (!all(names(models) %in% names(imputations))) stop("Model names do not match imputer names")
  if (!all(names(imputations) %in% names(hyperparameters))) stop("Imputer names do not match hyperparameter names")
  # Get the performance estimate from each leaf of the tree (i.e. all trained models)
  perf <- get_model_performance_estimates(models)

  best_model_ix <- get_best_model_index(perf)

  models <- select_from_tree(models, best_model_ix)
  imputers <- select_from_tree(imputations, best_model_ix)
  hyperparams <- select_hyperparams(hyperparameters, best_model_ix)
  hyperparams <- bind_imp_parameters_for_reuse(hyperparams, imputers)

  return(list(models = models, imputers = imputers, hyperparams = hyperparams))
}

#' Wrapper to train an RF classifier model
#'
#' These functions exist to give a uniform interface to classifier training with some model type specific options passed
#' to `caret::train`.
#'
#' @param data Training data as a data.frame to pass to `caret::train`
#' @param outcome Training outcomes as a factor to pass to `caret::train`
#' @param control A `caret::trainControl` object to pass to `caret::train`
#' @param grid A hyperparameter grid as a data.frame to pass to `caret::train`
#' @param tunelength Tuning length scalar to pass to `caret::train`
#'
#' @return A `caret::train` object with a fitted RF model
#' @export
train_rf <- function(data, outcome, control, grid, tunelength) {

  if (is.null(data)) {
    flog.pid.info("Dataset was NULL; returning NULL")
    return(NULL)
  }

  rf_model <- tryCatch({
    caret::train(x = data,
                 y = outcome,
                 method = "rf",
                 preProcess = c("center", "scale"),
                 trControl = control,
                 tuneLength = tunelength,
                 tuneGrid = grid)
  }, error = function(e) {
    flog.pid.debug(e)
    return(NULL)
  })

  return(rf_model)
}

#' Wrapper to train an XGBoost classifier model
#'
#' These functions exist to give a uniform interface to classifier training with some model type specific options passed
#' to `caret::train`.
#'
#' For XGBoost, the number of threads the training process is permitted to use has been limited to 2. XGBoost is set to
#' use the "hist" tree method to reduce computational cost.
#'
#' @param data Training data as a data.frame to pass to `caret::train`
#' @param outcome Training outcomes as a factor to pass to `caret::train`
#' @param control A `caret::trainControl` object to pass to `caret::train`
#' @param grid A hyperparameter grid as a data.frame to pass to `caret::train`
#' @param tunelength Tuning length scalar to pass to `caret::train`
#'
#' @return A `caret::train` object with a fitted XGBoost model
#' @export
train_xgboost <- function(data, outcome, control, grid, tunelength) {

  if (is.null(data)) {
    flog.pid.info("Dataset was NULL; returning NULL")
    return(NULL)
  }

  model <- tryCatch({
    caret::train(x = data,
                 y = outcome,
                 method = "xgbTree",
                 nthread = 2,
                 tree_method = "hist",
                 preProcess = c("center", "scale"),
                 trControl = control,
                 tuneLength = tunelength,
                 tuneGrid = grid)
  }, error = function(e) {
    flog.pid.debug(e)
    return(NULL)
  })

  return(model)
}

#' Wrapper to train an logistic regression classifier model
#'
#' These functions exist to give a uniform interface to classifier training with some model type specific options passed
#' to `caret::train`.
#'
#' For logistic regression, the hyperparameter grid and tune length are ignored.
#'
#' @param data Training data as a data.frame to pass to `caret::train`
#' @param outcome Training outcomes as a factor to pass to `caret::train`
#' @param control A `caret::trainControl` object to pass to `caret::train`
#' @param grid Ignored
#' @param tunelength Ignored
#'
#' @return A `caret::train` object with a fitted logistic regression model
#' @export
train_lr <- function(data, outcome, control, grid, tunelength=NULL) {

  if (is.null(data)) {
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

#' Loop classifier training over a tree of imputed training sets
#'
#' @param training_function One of the above defined classifier training wrappers
#' @param classifier_name Name for the classifier that will be used in the resulting model tree
#' @param imputations Tree of imputed training sets
#' @param control A `caret::trainControl` object
#' @param grid Hyperparameter grid to pass to `training_function`
#' @param tunelength Tuning length to pass to `training_function`
#' @param outcome Outcome vector
#' @param seed Seed value to set at start of loop
#'
#' @return Classifier model tree
#' @export
loop_models <- function(training_function, classifier_name, imputations, control, grid, tunelength, outcome, seed) {
  if (!is.function(training_function)) stop("`training_function` must be a function")
  if (!is.character(classifier_name)) stop("`classifier_name` must be a character vector")
  #if (!class(control) == "trainControl") stop("`control` must be a `trainControl` object")
  if (!is.data.frame(grid) && !is.null(grid)) stop("`grid` must be a data.frame or NULL")
  if (!is.factor(outcome)) stop("`outcome` must be a factor")
  if (!is.numeric(seed) || length(seed) != 1) stop("`seed` must be a numeric vector of length 1")

  foreach::foreach(method = enumerate(imputations), .options.RNG = seed) %dorng% {
    flog.pid.info("PROGRESS Starting execution of %s on datasets produced by %s", classifier_name, method$name)
    foreach::foreach(mi_iter = method$value) %do% {
      model_per_dataset <- foreach::foreach(data = mi_iter$completed_datasets) %do% training_function(data = data, outcome = outcome, control = control, grid = grid)
      return(model_per_dataset %>% magrittr::set_names(names(mi_iter$completed_datasets)))
    } %>% magrittr::set_names(names(method$value))
  } %>% magrittr::set_names(names(imputations))
}
