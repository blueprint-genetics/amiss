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
source("R/prediction.R")
source("R/performance.R")
source("R/constants.R")

test_prediction <- function(test_path, outcome_path, tr_output_path, results_dir_path, lean, cores, seed = 42) {

  create_dir(results_dir_path)

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
  outcome <- read.csv(outcome_path, row.names = 1, as.is = TRUE)[,1, drop = TRUE]
  outcome <- factor(outcome, levels = c(POSITIVE_LABEL, NEGATIVE_LABEL))

  # Keep exactly those features that were kept in training data
  flog.pid.info("Reading features used in training")
  final_features <- readRDS(file.path(tr_output_path, FILE_FINAL_FEATURES_RDS))
  test_data <- test_data[, final_features]

  ## Multiply impute the test set using the best hyperparameter configurations from the training set
  flog.pid.info("Reading best hyperparameter configurations for imputation methods")
  rf_hyperparams <- readRDS(file.path(tr_output_path, FILE_RF_HP_CONFIGS_RDS))
  lr_hyperparams <- readRDS(file.path(tr_output_path, FILE_LR_HP_CONFIGS_RDS))

  flog.pid.info("Starting imputation of test set")
  if (!lean) {
    times <- 10
  } else {
    times <- 1
  }
  iters <- 10
  flog.pid.info("For MICE methods, imputing %d times, with max. %d iterations", times, iters)

  flog.pid.info("Imputation of test set with best hyperparameter configurations for RF")
  rf_completions <- impute_w_hps(test_data, rf_hyperparams, times, iters, seed)
  flog.pid.info("Imputation of test set with best hyperparameter configurations for LR")
  lr_completions <- impute_w_hps(test_data, lr_hyperparams, times, iters, seed)

  ## Predict on test set completions using best classifier models
  flog.pid.info("Reading classifier models")
  rf_models <- readRDS(file.path(tr_output_path, FILE_RF_CLASSIFIERS_RDS))
  lr_models <- readRDS(file.path(tr_output_path, FILE_LR_CLASSIFIERS_RDS), refhook = function(x) .GlobalEnv)

  flog.pid.info("Starting prediction by RF models")
  rf_predictions <- prediction(rf_models, rf_completions)
  flog.pid.info("Starting prediction by LR models")
  lr_predictions <- prediction(lr_models, lr_completions)

  ## Compute performance statistics on the test set
  flog.pid.info("Computing performance statistics")
  rf_perf <- performance_stats(rf_predictions, outcome = outcome)
  lr_perf <- performance_stats(lr_predictions, outcome = outcome)

  rf_tables <- lapply(rf_perf, turn_table)
  lr_tables <- lapply(lr_perf, turn_table)

  rf_perf_table <- merge_tables(rf_tables)
  lr_perf_table <- merge_tables(lr_tables)

  flog.pid.info("Writing performance tables")
  write.csv(x = rf_perf_table, file = file.path(results_dir_path, FILE_RF_PERFORMANCE_CSV), row.names = FALSE)
  write.csv(x = lr_perf_table, file = file.path(results_dir_path, FILE_LR_PERFORMANCE_CSV), row.names = FALSE)

  flog.pid.info("Done")

}
