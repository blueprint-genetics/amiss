
predict_on_test_set <- function(test_path, outcome_path, tr_output_path, results_dir_path, lean, cores, seed = 42) {

  create_dir(results_dir_path)

  futile.logger::flog.appender(futile.logger::appender.tee(file.path(results_dir_path, "predict_on_test_set.log")))
  futile.logger::flog.threshold(futile.logger::DEBUG)

  flog.pid.info("predict_on_test_set.R")
  flog.pid.info("Arguments: %s", paste0(list(test_path, outcome_path, tr_output_path, results_dir_path, lean, cores, seed), collapse = ", "))

  flog.pid.info("Using %d cores", cores)
  doParallel::registerDoParallel(cores)

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
    times <- IMPUTE_TIMES
  } else {
    times <- SIMULATION_IMPUTE_TIMES
  }
  iters <- MICE_ITERATIONS
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

  if (!lean) {
    compute_perfs_per_conseq <- function(conseq, completions, models) {
      if(length(conseq) > 1) {
        # If there are multiple consequences, they are assumed to represent all possible consequences,
        # and we are looking for variants that are not part of any of them. Thus comparison to 0.
        data_w_conseq_ix <- apply(test_data[, conseq, drop = FALSE] == 0, MARGIN = 1, all)
      } else {
        data_w_conseq_ix <- test_data[[conseq]] == 1
      }
      outcome_w_conseq <- outcome[data_w_conseq_ix]
      completed_data_per_consequence <- recursive_apply(completions, fun = function(df) df[data_w_conseq_ix,,drop = FALSE], x_class = "data.frame")
      predictions <- prediction(models, completed_data_per_consequence)
      perf <- performance_stats(predictions, outcome = outcome_w_conseq)
      perf_table <- lapply(perf, turn_table) %>% merge_tables
      if(length(conseq) > 1) {
        perf_table$consequence <- "Other"
      } else {
        perf_table$consequence <- conseq
      }
      return(perf_table)
    }
    # Per consequence
    consequences <- find_dummies(CONSEQUENCE_COLUMN, colnames(test_data))
    flog.pid.info("Computing performance statistics per consequences:")
    flog.pid.info(consequences)
    rf_perf_table_per_consequence <- lapply(consequences, . %>% compute_perfs_per_conseq(rf_completions, rf_models))
    rf_perf_table_per_consequence <- c(rf_perf_table_per_consequence, list(compute_perfs_per_conseq(consequences, rf_completions, rf_models)))
    rf_perf_table_per_consequence <- do.call(rbind, rf_perf_table_per_consequence)
    lr_perf_table_per_consequence <- lapply(consequences, . %>% compute_perfs_per_conseq(lr_completions, lr_models))
    lr_perf_table_per_consequence <- c(lr_perf_table_per_consequence, list(compute_perfs_per_conseq(consequences, lr_completions, lr_models)))
    lr_perf_table_per_consequence <- do.call(rbind, lr_perf_table_per_consequence)
    write.csv(x = rf_perf_table_per_consequence, file = file.path(results_dir_path, FILE_RF_PERFORMANCE_PER_CONSEQUENCE_CSV), row.names = FALSE)
    write.csv(x = lr_perf_table_per_consequence, file = file.path(results_dir_path, FILE_LR_PERFORMANCE_PER_CONSEQUENCE_CSV), row.names = FALSE)
  }

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
