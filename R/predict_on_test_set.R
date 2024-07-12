#' Predict and evaluate predictions on test set
#'
#' This function imputes the test set using the hyperparameters determined in the impute and train -step,
#' and uses the classifier models to produce predictions from the imputed test set. The predictions are evaluated
#' and results are written (into separate files per classification method).
#'
#' Predictions are also evaluated separately for every consequence class, and results are written to a separate file
#' (for each classification method).
#'
#' @param test_path Path to the test data CSV
#' @param outcome_path Path to the test outcomes CSV
#' @param tr_output_path Path to the folder to which the impute and train -step was set to store output
#' @param results_dir_path Path to the folder to which result files should be written
#' @param seed Seed value
#' @export
predict_on_test_set <- function(test_path, outcome_path, tr_output_path, results_dir_path, seed = 42) {

  ### Setup ###
  results_dir_path <- normalizePath(results_dir_path, mustWork = FALSE)
  create_dir(results_dir_path)

  flog.pid.info("predict_on_test_set.R")
  flog.pid.info("PROGRESS Arguments: %s", paste0(list(test_path, outcome_path, tr_output_path, results_dir_path, seed), collapse = ", "))

  if (!is.null(seed)) {
    flog.pid.info("DESIGN_CHOICE Using seed: %d", seed)
    set.seed(seed)
  }

  ### Reading input ###
  test_path <- normalizePath(test_path)
  flog.pid.info("INPUT Reading test data from delimited file at %s", test_path)
  test_data <- read.csv(test_path, row.names = 1)
  consequences <- row.names(test_data) %>% strsplit(":", fixed = TRUE) %>% sapply(. %>% tail(1))

  outcome_path <- normalizePath(outcome_path)
  flog.pid.info("INPUT Reading test outcomes from delimited file at %s", outcome_path)
  outcome <- read.csv(outcome_path, row.names = 1)[,1, drop = TRUE]
  outcome <- factor(outcome, levels = c(POSITIVE_LABEL, NEGATIVE_LABEL))

  ### Processing input ###
  # Keep exactly those features that were kept in training data
  tr_output_path <- normalizePath(tr_output_path)
  final_features_path <- file.path(tr_output_path, FILE_FINAL_FEATURES_RDS)
  flog.pid.info("INPUT Reading features used in training from RDS file at %s", final_features_path)
  final_features <- readRDS(final_features_path)
  test_data <- test_data[, final_features]

  rf_models_path <- file.path(tr_output_path, FILE_RF_CLASSIFIERS_RDS)
  flog.pid.info("INPUT Reading RF classifier models from RDS file at %s", rf_models_path)
  rf_models <- readRDS(rf_models_path)
  xg_models_path <- file.path(tr_output_path, FILE_XGBOOST_CLASSIFIERS_RDS)
  flog.pid.info("INPUT Reading XGBoost classifier models from RDS file at %s", xg_models_path)
  xg_models <- readRDS(xg_models_path)
  lr_models_path <- file.path(tr_output_path, FILE_LR_CLASSIFIERS_RDS)
  flog.pid.info("INPUT Reading LR classifier models from RDS file at %s", lr_models_path)
  lr_models <- readRDS(lr_models_path, refhook = function(x) .GlobalEnv)

  times <- IMPUTE_TIMES
  iters <- MICE_ITERATIONS
  flog.pid.info("DESIGN_CHOICE For MICE methods, imputing %d times, with max. %d iterations", times, iters)

  ### Imputation and prediction ###
  # Random Forest
  if (rf_models %>% unlist(recursive = TRUE) %>% is.null %>% all %>% `!`) {

    ## Multiply impute the test set using the best hyperparameter configurations from the training set
    rf_hyperparams_path <- file.path(tr_output_path, FILE_RF_HP_CONFIGS_RDS)
    flog.pid.info("INPUT Reading best hyperparameter configurations for best imputation methods wrt. RF from RDS file at %s", rf_hyperparams_path)
    rf_hyperparams <- readRDS(rf_hyperparams_path)

    flog.pid.info("PROGRESS Imputation of test set with best imputation hyperparameter configurations wrt. RF")
    rf_completions <- impute_w_hps(test_data, rf_hyperparams, times, iters, seed)

    flog.pid.info("PROGRESS Starting prediction by RF models")
    perform_test_process_for_model_tree(
      rf_models,
      rf_completions,
      test_data,
      outcome,
      consequences,
      file.path(results_dir_path, FILE_RF_PERFORMANCE_CSV),
      per_conseq_output_path = file.path(results_dir_path, FILE_RF_PERFORMANCE_PER_CONSEQUENCE_CSV)
    )

  }
  # XGBoost
  if (xg_models %>% unlist(recursive = TRUE) %>% is.null %>% all %>% `!`) {

    ## Multiply impute the test set using the best hyperparameter configurations from the training set
    xg_hyperparams_path <- file.path(tr_output_path, FILE_XGBOOST_HP_CONFIGS_RDS)
    flog.pid.info("INPUT Reading best hyperparameter configurations for best imputation methods wrt. XGBoost from RDS file at %s", xg_hyperparams_path)
    xg_hyperparams <- readRDS(xg_hyperparams_path)

    flog.pid.info("PROGRESS Imputation of test set with best imputation hyperparameter configurations wrt. XGBoost")
    xg_completions <- impute_w_hps(test_data, xg_hyperparams, times, iters, seed)

    flog.pid.info("PROGRESS Starting prediction by XGBoost models")
    perform_test_process_for_model_tree(
      xg_models,
      xg_completions,
      test_data,
      outcome,
      consequences,
      file.path(results_dir_path, FILE_XGBOOST_PERFORMANCE_CSV),
      per_conseq_output_path = file.path(
        results_dir_path,
        FILE_XGBOOST_PERFORMANCE_PER_CONSEQUENCE_CSV
      )
    )

  }
  # Logistic regression
  if (lr_models %>% unlist(recursive = TRUE) %>% is.null %>% all %>% `!`) {

    ## Multiply impute the test set using the best hyperparameter configurations from the training set
    lr_hyperparams_path <- file.path(tr_output_path, FILE_LR_HP_CONFIGS_RDS)
    flog.pid.info("INPUT Reading best hyperparameter configurations for best imputation methods %s", lr_hyperparams_path)
    lr_hyperparams <- readRDS(lr_hyperparams_path)

    flog.pid.info("PROGRESS Imputation of test set with best imputation hyperparameter configurations wrt. LR")
    lr_completions <- impute_w_hps(test_data, lr_hyperparams, times, iters, seed)

    flog.pid.info("PROGRESS Starting prediction by LR models")
    perform_test_process_for_model_tree(
      lr_models,
      lr_completions,
      test_data,
      outcome,
      consequences,
      file.path(results_dir_path, FILE_LR_PERFORMANCE_CSV),
      file.path(results_dir_path, FILE_LR_PERFORMANCE_PER_CONSEQUENCE_CSV)
    )

  }
  flog.pid.info("PROGRESS Done")
}

perform_test_process_for_model_tree <- function(models, completions, test_data, outcome, consequences, output_path, per_conseq_output_path) {

  predictions <- prediction(models, completions)

  compute_perfs_per_conseq <- function(consequence, consequence_vector, completions, models) {
    outcome_w_conseq <- outcome[consequence_vector == consequence]
    completed_data_per_consequence <- recursive_apply(completions, fun = function(df) df[consequence_vector == consequence,,drop = FALSE], x_class = "data.frame")
    predictions <- prediction(models, completed_data_per_consequence)
    perf <- performance_stats(predictions, outcome = outcome_w_conseq)
    perf_table <- lapply(perf, transform_perf_tree_to_table) %>% merge_tables
    perf_table$consequence <- consequence
    return(perf_table)
  }
  flog.pid.info("PROGRESS Computing performance statistics per consequences:")
  flog.pid.info(paste0("PROGRESS ", unique(consequences)))
  perf_table_per_consequence <- lapply(unique(consequences), . %>% compute_perfs_per_conseq(consequences, completions, models))
  perf_table_per_consequence <- do.call(rbind, perf_table_per_consequence)
  flog.pid.info("OUTPUT Writing performance statistics per consequences into delimited file at %s", per_conseq_output_path)
  write.csv(x = perf_table_per_consequence, file = per_conseq_output_path, row.names = FALSE)

  flog.pid.info("PROGRESS Computing performance statistics")
  perf <- performance_stats(predictions, outcome = outcome)
  tables <- lapply(perf, transform_perf_tree_to_table)
  perf_table <- merge_tables(tables)
  flog.pid.info("OUTPUT Writing performance tables to delimited file at %s", output_path)
  write.csv(x = perf_table, file = output_path, row.names = FALSE)

}
