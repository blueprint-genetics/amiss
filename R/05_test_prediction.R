library(magrittr)
library(futile.logger)
library(mice)
library(ModelMetrics)
library(caret)
library(stringr)
library(foreach)
library(doParallel)
library(doRNG)
library(DMwR)
library(missForest)

source("R/utils.R")
source("R/recursive_application.R")
source("R/imputation_definitions.R")
source("R/imputation.R")

test_prediction <- function(test_path, outcome_path, tr_output_path, results_dir_path, lean, cores, seed = 42) {

  if (!dir.exists(results_dir_path)) {
    dir_creation_success <- dir.create(results_dir_path, showWarnings = TRUE)
    if (!dir_creation_success) {
      stop("Failed to create directory for saving results.")
    }
  }

  flog.appender(appender.tee(file.path(results_dir_path, "05_test_prediction.log")))
  flog.threshold(DEBUG)

  flog.pid.info("05_test_prediction.log")
  flog.pid.info("Arguments: %s", paste0(list(test_path, outcome_path, tr_output_path, results_dir_path, lean, cores, seed), collapse = ", "))

  flog.pid.info("Using %d cores", cores)
  registerDoParallel(cores)

  if(!is.null(seed)) {
    flog.pid.info("Using seed: %d", seed)
    set.seed(seed)
  }

  flog.pid.info("Reading data")
  test_data <- read.csv(test_path, row.names = 1, as.is = TRUE)
  outcome <- read.csv(outcome_path, as.is = TRUE)
  outcome <- factor(outcome[,2], levels = c("positive", "negative"))


  # Keep exactly those features that were kept in training data
  flog.pid.info("Reading features used in training")
  final_features <- readRDS(file.path(tr_output_path, "final_features.rds"))
  test_data <- test_data[, final_features]


  ## Multiply impute the test set using the best hyperparameter configurations from the training set

  flog.pid.info("Reading best hyperparameter configurations for imputation methods")
  rf_hyperparams <- readRDS(file.path(tr_output_path, "rf_hp_configs.rds"))
  lr_hyperparams <- readRDS(file.path(tr_output_path, "lr_hp_configs.rds"))

  flog.pid.info("Starting imputation of test set")
  if(!lean) {
    times <- 10
  } else {
    times <- 1
  }
  iters <- 10
  flog.pid.info("For MICE methods, imputing %d times, with max. %d iterations", times, iters)

  impute_w_hps <- function(data, hp_tree){

    imputations <- foreach(hps = enumerate(hp_tree), .options.RNG = seed) %dorng% {

      # The imputation parameters estimated from the training set should be used
      # where possible.
      estimates <- attr(hps$value, "imputation_estimates")

      method <- hps$name
      flog.pid.info("Imputing with method %s", method)
      if (method %in% names(mice_imputation_hyperparameters)) {

        run_mice(data, method, hps$value, times, iters)

      } else if (method == "bpca") {

        run_bpca(data, hps$value)

      } else if (method == "knnImputation") {

        run_knn(data, hps$value, old_data = estimates)

      } else if (method == "missForest") {

        run_missforest(data, hps$value, times = times)

      } else if (method == "missingness_indicators") {

        list(completed_datasets = list(`1` = missingness_indicators(data, remove_vector = estimates)))

      } else if (method %in% names(single_value_imputation_hyperparameter_grids)) {

        list(completed_datasets = list(`1` = reimpute(dataframe = data, value = estimates)))

      } else {

        flog.pid.debug("Unknown imputation_method: %s ; returning NULL", method)
        list(completed_datasets = list(`1` = NULL))

      }
    }
    names(imputations) <- names(hp_tree)

    completions <- imputations %>% lapply(. %>% extract2(1))

    return(completions)
  }
  flog.pid.info("Imputation of test set with best hyperparameter configurations for RF")
  rf_completions <- impute_w_hps(test_data, rf_hyperparams)
  flog.pid.info("Imputation of test set with best hyperparameter configurations for LR")
  lr_completions <- impute_w_hps(test_data, lr_hyperparams)


  ## Predict on test set completions using best classifier models

  flog.pid.info("Reading classifier models")
  rf_models <- readRDS(file.path(tr_output_path, "rf_classifiers.rds"))
  lr_models <- readRDS(file.path(tr_output_path, "lr_classifiers.rds"), refhook = function(x) .GlobalEnv)

  prediction <- function(models, completions) {

    predictions <- lapply(names(models), function(method) {

      pred_per_model <- lapply(models[[method]], function(model) {

        pred_per_completion <- foreach(completed_dataset = completions[[method]], .options.RNG = seed) %dorng% {
          if (!is.null(completed_dataset)) {
            tryCatch({
              flog.pid.info("Predicting using best model for %s", method)
              return(predict(model, completed_dataset, type = "prob")[,"positive", drop = TRUE])
            }, error = function(e) {
              flog.pid.debug(e)
              return(NA)
            })
          }
          else  {
            return(NA)
          }
        }

        names(pred_per_completion) <- paste0("imp_", seq_along(pred_per_completion))
        pred_per_completion

      })

      names(pred_per_model) <- paste0("model_", seq_along(pred_per_model))
      pred_per_model

    })
    names(predictions) <- names(models)
    return(predictions)
  }

  flog.pid.info("Starting prediction by RF models")
  rf_predictions <- prediction(rf_models, rf_completions)
  flog.pid.info("Starting prediction by LR models")
  lr_predictions <- prediction(lr_models, lr_completions)

  ## Compute performance statistics on the test set
  flog.pid.info("Computing performance statistics")
  performance_stats <- function(predictions) {

    confusion_matrices <- recursive_apply_numeric(predictions, function(pred) {
      pred <- factor(c("positive", "negative")[2 - (pred > 0.5)], c("positive", "negative"))
      caret::confusionMatrix(pred, outcome)
    })

    extract_stat <- function(stat) function(x) x %>% use_series("byClass") %>% extract(stat)

    positive_outcome_indicator <- as.integer(outcome == "positive")

    mcc <- recursive_apply_numeric(predictions, . %>% mcc(actual = positive_outcome_indicator, predicted = ., cutoff = 0.5))
    auc <- recursive_apply_numeric(predictions, . %>% auc(actual = positive_outcome_indicator, predicted = .))

    recursive_apply_cm <- function(x, fun) recursive_apply(x = x, fun = fun, x_class = "confusionMatrix")

    tp <- recursive_apply_cm(confusion_matrices, . %>% use_series("table") %>% extract("positive", "positive") %>% as.numeric %>% set_names("tp"))
    fp <- recursive_apply_cm(confusion_matrices, . %>% use_series("table") %>% extract("positive", "negative") %>% as.numeric %>% set_names("fp"))
    fn <- recursive_apply_cm(confusion_matrices, . %>% use_series("table") %>% extract("negative", "positive") %>% as.numeric %>% set_names("fn"))
    tn <- recursive_apply_cm(confusion_matrices, . %>% use_series("table") %>% extract("negative", "negative") %>% as.numeric %>% set_names("tn"))
    sensitivity <- recursive_apply_cm(confusion_matrices, extract_stat("Sensitivity"))
    specificity <- recursive_apply_cm(confusion_matrices, extract_stat("Specificity"))
    f1 <- recursive_apply_cm(confusion_matrices, extract_stat("F1"))
    precision <- recursive_apply_cm(confusion_matrices, extract_stat("Precision"))
    recall <- recursive_apply_cm(confusion_matrices, extract_stat("Recall"))

    perfs <- list(tp = tp,
                  fp = fp,
                  fn = fn,
                  tn = tn,
                  mcc = mcc,
                  auc = auc,
                  sensitivity = sensitivity,
                  specificity = specificity,
                  f1 = f1,
                  precision = precision,
                  recall = recall)

    return(perfs)

  }
  rf_perf <- performance_stats(rf_predictions)
  lr_perf <- performance_stats(lr_predictions)


  turn_table <- function(perf_tree) {

    tree_names <- recursive_apply_numeric(perf_tree, function(x, name_list) return(name_list), pass_node_names = TRUE)
    tree_names %<>% leaf_apply(. %>% paste0(collapse = ":"), docall = FALSE)
    tree_names %<>% unlist(use.names = FALSE)

    values <- perf_tree %>% unlist(use.names = FALSE)
    names(values) <- tree_names

    df <- lapply(names(values), function(name) {
      stringr::str_split(string = name, pattern = stringr::fixed(":"), simplify = TRUE)
    })
    df <- data.frame(do.call(rbind, df), value = values)

    colnames(df) <- c("method", "model_index", "test_realization", "value")

    return(df)
  }

  rf_tables <- lapply(rf_perf, turn_table)
  lr_tables <- lapply(lr_perf, turn_table)

  merge_tables <- function(tables) {
    perf_table <- Reduce(function(x, y) merge(x, y, by = c("method", "model_index", "test_realization")), tables)
    colnames(perf_table) <- c("method", "model_index", "test_realization", names(tables))
    perf_table
  }
  rf_perf_table <- merge_tables(rf_tables)
  lr_perf_table <- merge_tables(lr_tables)


  flog.pid.info("Writing performance tables")
  write.csv(x = rf_perf_table, file = file.path(results_dir_path, "rf_performance.csv"), row.names = FALSE)
  write.csv(x = lr_perf_table, file = file.path(results_dir_path, "lr_performance.csv"), row.names = FALSE)

  flog.pid.info("Done")

}
