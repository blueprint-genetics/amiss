
library(purrr)
library(futile.logger)
library(caret)
library(mice)
library(ModelMetrics)
library(DMwR)
library(pcaMethods)
library(foreach)
library(doParallel)
library(doRNG)
library(missForest)

impute_and_train <- function(training_path, outcome_path, output_path, cores, seed = 42, lean) {

  if (!dir.exists(output_path)) {
    dir_creation_success <- dir.create(output_path, showWarnings = TRUE)
    if (!dir_creation_success) {
      stop("Failed to create directory for saving output.")
    }
  }

  flog.appender(appender.tee(file.path(output_path, "04_impute_and_train.log")))

  flog.threshold(DEBUG)

  source("R/utils.R")
  source("R/imputation_definitions.R")
  sample_max <- function(x, size) {
    if (NROW(x) < size) return(x)
    else return(x[sample(1:NROW(x), size = size, replace= FALSE), , drop = FALSE])
  }
  if (lean) {
    mice_hyperparameter_grids <- lapply(mice_hyperparameter_grids, . %>% sample_max(size = 8))
    other_hyperparameter_grids <- lapply(other_hyperparameter_grids, . %>% sample_max(size = 8))
  }

  source("R/recursive_application.R")
  source("R/imputation.R")

  flog.pid.info("04_impute_and_train.log")
  flog.pid.info("Arguments: %s", paste0(list(training_path, outcome_path, output_path, cores, lean), collapse = ", "))

  flog.pid.info("Using %d cores", cores)
  registerDoParallel(cores)

  flog.pid.info("Using seed: %d", seed)
  set.seed(seed)

  flog.pid.info("Reading data")
  training_data <- read.csv(training_path, row.names = 1, as.is = TRUE)
  tryCatch({
  outcome <- read.csv(outcome_path, as.is = TRUE)
  }, error = function(e) {
    flog.pid.debug("Could not open file %s", outcome_path)
    flog.pid.debug(e)
  })
  outcome <- factor(outcome[,2], levels = c("positive", "negative"))
  flog.pid.info("Outcome levels: %s", paste0(levels(outcome), collapse = ", "))

  ## Removal of problematic features

  # Some imputation methods cannot deal with features that have certain unwanted properties, and thus they must be removed prior to imputation.

  ### Near-zero variance

  flog.pid.info("Removing features with near-zero variance")
  cut_p <- 1
  flog.pid.info("Uniqueness cutoff: %d %%", cut_p)
  nzv_features <- caret::nearZeroVar(training_data, saveMetrics = TRUE, uniqueCut = cut_p)
  flog.pid.info(capture.output(print(nzv_features[nzv_features$nzv, ])))

  if (any(nzv_features$nzv)) {
    training_data <- training_data[, !nzv_features$nzv]
  }

  ### Highly correlated features

  flog.pid.info("Removing highly correlated features:")
  correlations <- cor(training_data, use = "pairwise.complete.obs")
  correlations[is.na(correlations)] <- 0.0

  highly_correlated_features <- caret::findCorrelation(correlations, verbose = TRUE, names = TRUE)
  flog.pid.info(highly_correlated_features)

  if (highly_correlated_features %>% length > 0) {
    training_data <- training_data[, !colnames(training_data) %in% highly_correlated_features]
  }

  ## Imputation

  flog.pid.info("Imputation hyperparameter configuration counts:")
  # Check the number of hyperparameter configurations for each imputation method:
  flog.pid.info(paste0(names(mice_hyperparameter_grids), ": ",  lapply(mice_hyperparameter_grids, nrow)))
  flog.pid.info(paste0(names(other_hyperparameter_grids), ": ",  lapply(other_hyperparameter_grids, nrow)))

  ### MICE
  flog.pid.info("Starting MICE imputation")
  if(!lean) {
    times <- 10
  } else {
    times <- 1
  }
  iters <- 10
  flog.pid.info("Imputing %d times, with max. %d iterations", times, iters)

  mice_imputations <- foreach(method = enumerate(mice_hyperparameter_grids)) %do% {

    hyperparams <- method$value

    if (nrow(hyperparams) == 0) {
      imputations <- list(imp_hp_1 = run_mice(training_data, method$name, list(), times, iters))
    }
    else {
      imputations <- foreach(hp_row = 1:nrow(hyperparams), .options.RNG = seed) %dorng% {
        flog.pid.info("Imputing with method %s, parameters %s", method$name, paste0(names(hyperparams), ": ", hyperparams[hp_row, ], collapse = ", "))
        run_mice(training_data, method$name, unlist(hyperparams[hp_row,]), times, iters)
      } %>% set_names(paste0("imp_hp_", 1:nrow(hyperparams)))
    }

    # Combine timings of different hyperparameter configs
    timings <- do.call(rbind, lapply(imputations, . %>% attr("timing")))
    # Return them along with the imputation object
    attr(imputations, "timings") <- timings

    return(imputations)
  }
  names(mice_imputations) <- names(mice_hyperparameter_grids)

  ### Non-MICE imputations

  flog.pid.info("Starting non-MICE imputation")
  other_imputations <- foreach(method = enumerate(other_hyperparameter_grids)) %do%  {

    hyperparams <- method$value

    if (method$name == "bpca") {
      imputations <- foreach(hp_row = 1:nrow(hyperparams), .options.RNG = seed) %dorng% {
        flog.pid.info("Imputing with method %s, parameters %s", method$name, paste0(names(hyperparams), ": ", hyperparams[hp_row, ], collapse = ", "))
        run_bpca(data = training_data, hyperparams = hyperparams[hp_row, ])
      } %>% set_names(paste0("imp_hp_", 1:nrow(hyperparams)))
    }
    else if (method$name == "knnImputation") {
      imputations <- foreach(hp_row = 1:nrow(hyperparams), .options.RNG = seed) %dorng% {
        flog.pid.info("Imputing with method %s, parameters %s", method$name, paste0(names(hyperparams), ": ", hyperparams[hp_row, ], collapse = ", "))
        run_knn(data = training_data, hyperparams = hyperparams[hp_row, ])
      } %>% set_names(paste0("imp_hp_", 1:nrow(hyperparams)))
    } else if (method$name == "missForest") {
      imputations <- foreach(hp_row = 1:nrow(hyperparams), .options.RNG = seed) %dorng% {
        flog.pid.info("Imputing with method %s, parameters %s", method$name, paste0(names(hyperparams), ": ", hyperparams[hp_row, ], collapse = ", "))
        run_missforest(data = training_data, hyperparams = hyperparams[hp_row, ], times = times)
      } %>% set_names(paste0("imp_hp_", 1:nrow(hyperparams)))
    } else {
      flog.pid.info("Method %s is not implemented", method$name)
      imputations <- NULL
    }

    # Combine timings of different hyperparameter configs
    timings <- do.call(rbind, lapply(imputations, . %>% attr("timing")))
    # Return them along with the imputation object
    attr(imputations, "timings") <- timings

    return(imputations)
  } %>% set_names(names(other_hyperparameter_grids))

  ### Single value imputations

  flog.pid.info("Starting single value imputation")
  single_value_imputations <- lapply(enumerate(single_value_imputation_hyperparameter_grids), function(method) {
    list(`imp_hp_1` = list(completed_datasets = list(get(method$name)(training_data))))
  }) %>% set_names(names(single_value_imputation_hyperparameter_grids))

  ### List and drop imputation methods that failed completely

  imputations <- c(mice_imputations, other_imputations, single_value_imputations)

  flog.pid.info("Checking and dropping failed imputation methods")
  valid_methods <- sapply(names(imputations), function(method) {

    null_hpsets <- sapply(imputations[[method]], function(x) x[["completed_datasets"]] %>% unlist %>% is.null)
    if (all(null_hpsets)) {
      flog.pid.info("Imputation method %s did not successfully produce any datasets", method)
      return(FALSE)
    }
    return(TRUE)
  })
  imputations <- imputations[valid_methods]

  ## Training classifier
  flog.pid.info("Starting classifier training")
  hyperparameter_grid <- data.frame(mtry = 1:5 * 8 - 1)
  flog.pid.info("Hyperparameter grid:")
  flog.pid.info(capture.output(print(hyperparameter_grid)))

  rf_training_settings <- trainControl(classProbs = TRUE,
                                       verboseIter = FALSE,
                                       method = "oob",  # Use out-of-bag error estimate for model selection
                                       returnResamp = "final",
                                       allowParallel = FALSE) # Don't use parallelization inside training loop; it will be done on a higher level
  lr_training_settings <- trainControl(classProbs = TRUE,
                                       verboseIter = FALSE,
                                       allowParallel = FALSE) # Don't use parallelization inside training loop; it will be done on a higher level

  train_rf <- function(dataset) {

    rf_model <- NULL
    tryCatch({
      rf_model <- caret::train(x = dataset,
                               y = outcome,
                               method = "rf",
                               preProcess = c("center", "scale"),
                               trControl = rf_training_settings,
                               tuneGrid = hyperparameter_grid)
    }, error = function(e) flog.pid.debug(e))

    return(rf_model)
  }
  train_lr <- function(dataset) {

    lr_model <- NULL
    tryCatch({
      lr_model <- caret::train(x = dataset,
                               y = outcome,
                               method = "glm",
                               preProcess = c("center", "scale"),
                               trControl = lr_training_settings)
    }, error = function(e) flog.pid.debug(e))

    return(lr_model)
  }
  # Train on every completed dataset
  rf_models <- foreach(method = enumerate(imputations), .options.RNG = seed) %dorng% {
    flog.pid.info("Starting training of RFs on datasets produced by %s", method$name)
    foreach(mi_iter = method$value) %do% {
      foreach(data = mi_iter$completed_datasets) %do% {
        if(is.null(data)) {
          flog.pid.info("Dataset was NULL")
          return(NULL)
        } else {
          return(train_rf(data))
        }
      } %>% set_names(names(mi_iter$completed_datasets))
    } %>% set_names(names(method$value))
  } %>% set_names(names(imputations))

  lr_models <- foreach(method = enumerate(imputations), .options.RNG = seed) %dorng% {
    flog.pid.info("Starting training of LRs on datasets produced by %s", method$name)
    foreach(mi_iter = method$value) %do% {
      foreach(data = mi_iter$completed_datasets) %do% {
        if(is.null(data)) {
          flog.pid.info("Dataset was NULL")
          return(NULL)
        } else {
          return(train_lr(data))
        }
      } %>% set_names(names(mi_iter$completed_datasets))
    } %>% set_names(names(method$value))
  } %>% set_names(names(imputations))

  ## Model selection
  flog.pid.info("Starting model selection")
  extract_oob_performance <- function(model) {
    model$finalModel$err.rate[, "OOB"] %>% tail(1)
  }
  extract_mcc_performance <- function(model) {
    ModelMetrics::mcc(as.integer(outcome == "positive"), 1 - model$finalModel$fitted.values, 0.5)
  }

  # Find index of model with best mean OOB error
  inf_NULLs <- function(x, positive = TRUE) {
    x[sapply(x, is.null)] <- ifelse(positive, Inf, -Inf)
    return(x)
  }

  select_best <- function(models, imputations, hyperparams, performance_function, positive) {

    # Get the error estimate from each leaf of the tree (i.e. all trained models)
    perf <- map_depth(models, .f = performance_function, .depth = 3)
    perf <- map_depth(perf, . %>% inf_NULLs(positive = positive), .depth = 2)

    # The OOB estimates are in lists with one value per completed dataset.
    # Unlisting that list before mean gives mean the desired input type (numeric vector).
    mean_perf <- map_depth(perf, . %>% unlist %>% mean, .depth = 2)
    best_model_ix <- map_int(mean_perf, which.min)

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

  rf_bests <- select_best(rf_models, imputations, c(mice_hyperparameter_grids, other_hyperparameter_grids, single_value_imputation_hyperparameter_grids), performance_function = extract_oob_performance, TRUE)
  lr_bests <- select_best(lr_models, imputations, c(mice_hyperparameter_grids, other_hyperparameter_grids, single_value_imputation_hyperparameter_grids), performance_function = extract_mcc_performance, FALSE)

  flog.pid.info("Saving data")
  ## Saving model
  saveRDS(rf_bests$models, file = file.path(output_path, "rf_classifiers.rds"))
  if (!lean) saveRDS(rf_bests$imputers, file = file.path(output_path, "rf_imputers.rds"))
  saveRDS(rf_bests$hyperparams, file = file.path(output_path, "rf_hp_configs.rds"))

  # glm models in R contain references to environments, but for prediction it doesn't seem that 
  # the environment needs to be the exact one defined during training. Using a dummy `refhook`-argument
  # we can bypass saving the environments and save *a lot* of space (~ 50 Mb per model -> 7 Mb per model).
  # See https://stackoverflow.com/questions/54144239/how-to-use-saverds-refhook-parameter for an example of
  # using the `refhook`.
  saveRDS(lr_bests$models, file = file.path(output_path, "lr_classifiers.rds"), refhook = function(x) "")
  if (!lean) saveRDS(lr_bests$imputers, file = file.path(output_path, "lr_imputers.rds"))
  saveRDS(lr_bests$hyperparams, file = file.path(output_path, "lr_hp_configs.rds"))

  saveRDS(colnames(training_data), file.path(output_path, "final_features.rds"))
  flog.pid.info("Done saving data")
}
