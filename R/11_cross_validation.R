get_result_frame <- function(path, fold_i) {
  results <- tryCatch({
    results <- read.csv(path)
  }, error = function(e) {
    results <- data.frame(t(rep(NA, 17)))
    colnames(results) <- c(METHOD_COLUMN, MODEL_INDEX_COLUMN, TEST_COMPLETION_INDEX_COLUMN, "TP","FP","FN","TN","Accuracy","Brier","MCC","AUC","Sensitivity","Specificity","F1","Precision", "tr_time", "te_time")
    return(results)
  })
  results$fold <- fold_i

}
#' Cross-validation body
#'
#' This function performs training and testing with a given parameter combination,
#' training data and testing data sets and their respective label vectors.
#' The results files are read, a fold number is appended to each row, and
#' the results are returned in a list. The list is ordered to contain random forest results first,
#' XGBoost results second and logistic regression results third.
#'
#' @param fold_tr_datas List of training datasets for each cross-validation fold
#' @param fold_te_datas List of test datasets for each cross-validation fold
#' @param fold_tr_outcomes List of training outcome vectors for each cross-validation fold
#' @param fold_te_outcomes List of test outcome vectors for each cross-validation fold
#' @param output_path Path to directory to write results in
#' @param i Fold index
#'
#' @return A triple (3-element list) of data.frames containing results for random forest, xgboost and
#' logistic regression, respectively. When a classifier fails to produce results, a row of missing values is returned.
#' @export
cv_loop <- function(parameters, fold_paths, output_path, i) {

  flog.pid.info("PROGRESS Starting fold %d", i)

  # Define hyperparameter grid for the selected imputation method. This is a bit complicated due to the
  # `impute_and_train` method being designed to run several imputations; it could be simplified.
  flog.pid.info("PARAMETER %s = %s", IMPUTATION_METHOD, parameters[[IMPUTATION_METHOD]])
  flog.pid.info("PARAMETER Obtaining hyperparameter grid for %s", parameters[[IMPUTATION_METHOD]])
  single_value_imputation_hyperparameter_grids_in <- NULL
  if (parameters[[IMPUTATION_METHOD]] %in% names(single_value_imputation_hyperparameter_grids)) {
     single_value_imputation_hyperparameter_grids_in <- single_value_imputation_hyperparameter_grids[ parameters[[IMPUTATION_METHOD]] ]
  } else stop(
    paste0("Unknown value \"", parameters[[IMPUTATION_METHOD]], "\" for parameter \"", IMPUTATION_METHOD, "\"")
  )

  # Do the real work, i.e. imputation, training and testing
  flog.pid.info("PROGRESS Starting imputation and training process")
  impute_and_train(training_path = fold_paths[["tr_data_path"]], outcome_path = fold_paths[["tr_outcome_path"]], output_path = fold_paths[["dir_path"]],
                   single_value_imputation_hyperparameter_grids = single_value_imputation_hyperparameter_grids_in,
                   parameter_list = parameters,
                   seed = 42)
  flog.pid.info("PROGRESS Starting test process")
  predict_on_test_set(test_path = fold_paths[["te_data_path"]], outcome_path = fold_paths[["te_outcome_path"]], tr_output_path = fold_paths[["dir_path"]], results_dir_path = file.path(fold_paths[["dir_path"]], "results"), seed = 42)

  # Obtain and combine results
  rf_results_path <- file.path(fold_paths[["dir_path"]], "results", FILE_RF_PERFORMANCE_CSV)
  flog.pid.info("PROGRESS Obtaining results for RF from delimited file %s", rf_results_path)
  rf_results <- get_result_frame(rf_results_path, i)

  rf_results_per_conseq_path <- file.path(fold_paths[["dir_path"]], "results", FILE_RF_PERFORMANCE_PER_CONSEQUENCE_CSV)
  flog.pid.info("PROGRESS Obtaining per-consequence results for RF from delimited file %s", rf_results_per_conseq_path)
  rf_results_per_conseq <- get_result_frame(rf_results_per_conseq_path, i)

  xg_results_path <- file.path(fold_paths[["dir_path"]], "results", FILE_XGBOOST_PERFORMANCE_CSV)
  flog.pid.info("PROGRESS Obtaining results for XGBoost from delimited file %s", xg_results_path)
  xg_results <- get_result_frame(xg_results_path, i)

  xg_results_per_conseq_path <- file.path(fold_paths[["dir_path"]], "results_per_conseq", FILE_XGBOOST_PERFORMANCE_CSV)
  flog.pid.info("PROGRESS Obtaining results per consequence for XGBoost from delimited file %s", xg_results_per_conseq_path)
  xg_results_per_conseq <- get_result_frame(xg_results_per_conseq_path, i)

  lr_results_path <- file.path(fold_paths[["dir_path"]], "results", FILE_LR_PERFORMANCE_CSV)
  flog.pid.info("PROGRESS Obtaining results for LR from delimited file %s", lr_results_path)
  lr_results <- get_result_frame(lr_results_path, i)

  lr_results_per_conseq_path <- file.path(fold_paths[["dir_path"]], "results_per_conseq", FILE_LR_PERFORMANCE_CSV)
  flog.pid.info("PROGRESS Obtaining results per consequence for LR from delimited file %s", lr_results_per_conseq_path)
  lr_results_per_conseq <- get_result_frame(lr_results_per_conseq_path, i)

  flog.pid.info("PROGRESS Finishing fold %d", i)
  return(list(rf_results = rf_results,
              xg_results = xg_results,
              lr_results = lr_results,
              rf_results_per_conseq = rf_results_per_conseq,
              xg_results_per_conseq = xg_results_per_conseq,
              lr_results_per_conseq = lr_results_per_conseq)
         )
}

prep_cv <- function(preprocessed_data_path,
                    output_path,
                    parameters_path,
                    n_folds,
                    seed) {

  # Setup paths
  training_data_path <- file.path(preprocessed_data_path, FILE_PREPROCESSED_TRAINING_DATA_CSV) %>% normalizePath
  flog.pid.info("INPUT Reading preprocessed data from delimited file at %s", training_data_path)
  training_data <- read.csv(training_data_path, row.names = 1, as.is = TRUE)

  preprocessed_data_path <- normalizePath(preprocessed_data_path)
  preprocessed_outcomes_path <- file.path(preprocessed_data_path, FILE_TRAINING_OUTCOMES_CSV)
  flog.pid.info("INPUT Reading preprocessed outcomes from delimited file at %s", preprocessed_outcomes_path)
  outcomes <- read.csv(preprocessed_outcomes_path, row.names = 1, as.is = TRUE)[[1]]

  ### Cross-validation fold generation ###
  # Reorder rows
  flog.pid.info("PROGRESS Preparing for cross-validation: reordering rows")
  rows <- NROW(training_data)
  reordering <- sample(1:rows, rows, replace = FALSE)
  training_data <- training_data[reordering,]
  outcomes <- outcomes[reordering]

  flog.pid.info("PROGRESS Preparing for cross-validation: generating cross-validation folds")
  folds <- replicate(n_folds, sample(1:rows, 0.7*rows, replace = FALSE), simplify = FALSE)

  fold_tr_datas <- lapply(folds, function(fold) training_data[-fold, ])
  fold_tr_outcomes <- lapply(folds, function(fold) outcomes[-fold])
  fold_te_datas <- lapply(folds, function(fold) training_data[fold, ])
  fold_te_outcomes <- lapply(folds, function(fold) outcomes[fold])

  paths <- foreach::foreach(i = 1:n_folds) %do% {
    # Define output paths
    dir_path <- file.path(output_path, paste0("fold_", i)) %>% normalizePath(mustWork = FALSE)
    create_dir(dir_path)
    flog.pid.info("OUTPUT Output root folder for fold %d set to %s", i, dir_path)

    tr_data_path <- file.path(dir_path, paste0(FILE_CROSSVALIDATION_TRAINING_DATA, "_", i, ".csv"))
    flog.pid.info("INPUT Writing training data for fold %d into delimited file %s", i, tr_data_path)
    tr_outcome_path <- file.path(dir_path, paste0(FILE_CROSSVALIDATION_TRAINING_OUTCOMES, "_", i, ".csv"))
    flog.pid.info("INPUT Writing training outcomes for fold %d into delimited file %s", i, tr_outcome_path)
    te_data_path <- file.path(dir_path, paste0(FILE_CROSSVALIDATION_TEST_DATA, "_", i, ".csv"))
    flog.pid.info("INPUT Writing test data for fold %d into delimited file %s", i, te_data_path)
    te_outcome_path <- file.path(dir_path, paste0(FILE_CROSSVALIDATION_TEST_OUTCOMES, "_", i, ".csv"))
    flog.pid.info("INPUT Writing test outcomes for fold %d into delimited file %s", i, te_outcome_path)

    # Write out data for fold
    write.csv(fold_tr_datas[[i]], tr_data_path)
    write.csv(fold_tr_outcomes[[i]], tr_outcome_path)
    write.csv(fold_te_datas[[i]], te_data_path)
    write.csv(fold_te_outcomes[[i]], te_outcome_path)

    return(
      list(
        dir_path = dir_path,
        tr_data_path = tr_data_path,
        tr_outcome_path = tr_outcome_path,
        te_data_path = te_data_path,
        te_outcome_path = te_outcome_path
        )
    )
  }

  return(paths)

}


#' Step 11: Cross-validation
#'
#' This step performs parallel cross-validation with a specified parameter combination.
#' The preprocessed training data from step 2 is read and split into `n_folds` folds
#' (with 70/30 splits). Training and testing is done for each fold in parallel (if a
#' parallelization backend for foreach has been set). Results from every fold are gathered
#' to files separated by the classifier model type.
#'
#' Parameters directly used by this step (referenced with constant names,
#' see R/parameters.R for explicit string values):
#' - IMPUTATION_METHOD, which specifies the imputation method to be used
#'
#' Parameters indirectly used by this step (used inside functions called from this step):
#' - DOWNSAMPLING, which specifies whether the label majority class should be downsampled to
#' the size of the minority class,
#' - FEATURE_SAMPLING_PERCENTAGE, which specifies the percentage of features that should be kept
#' when doing feature sampling,
#' - TRAINING_DATA_SAMPLING_PERCENTAGE, which specifies the percentage of rows that should be kept
#' when doing training data sampling,
#' - NZV_CHECK, which specifies whether features with near-zero variance should be removed,
#' - CORRELATION_CHECK, which specifies whether features that are highly correlated with another
#' feature should be removed,
#' - HYPERPARAMETER_SEARCH_TYPE, which specifies whether grid or random search should be used for
#' optimizing classifier hyperparameters
#' - CATEGORICAL_ENCODING, which specifies whether categorical features should
#' be encoded as dummy variables
#'
#' @param preprocessed_data_path Path to directory containing the results from step 02
#' @param output_path Path to directory in which outputs will be written
#' @param parameters_path Path to file specifying parameter combination
#' @param n_folds Number of cross-validation folds
#' @param seed Seed value
#'
#' @return
#' @export
S11_cross_validation <- function(preprocessed_data_path,
                                 output_path,
                                 parameters_path,
                                 n_folds,
                                 logs_dir = normalizePath("logs", mustWork = FALSE),
                                 seed = 10) {

  ### Setup ###
  output_path <- normalizePath(output_path, mustWork = FALSE)
  create_dir(logs_dir)
  create_dir(output_path)

  futile.logger::flog.info("DESIGN_CHOICE Using seed: %d", seed)
  set.seed(seed)

  futile.logger::flog.appender(futile.logger::appender.tee(file.path(logs_dir, "11_cross_validation.log")))
  futile.logger::flog.threshold(futile.logger::DEBUG)
  flog.pid.info("START 11_cross_validation.R")
  flog.pid.info("OUTPUT Output root path set to %s", output_path)

  # Process parameters
  config <- get_config(parameters_path)

  if (config[[IMPUTATION_METHOD]] %>% is.null) {
    stop("Required parameter \"" %>% paste0(IMPUTATION_METHOD, "\" not provided"))
  }

  fold_paths <- prep_cv(
    preprocessed_data_path = preprocessed_data_path,
    output_path = output_path,
    parameters_path = parameters_path,
    n_folds = n_folds,
    seed = seed
  )

  flog.pid.info("PROGRESS Performing cross-validation")
  pkgs <- c(
    "amiss",
    "purrr",
    "doRNG",
    "caret",
    "futile.logger",
    "magrittr",
    "ModelMetrics"
  )

  ### Cross-validation ###
  results_triplet <- foreach::foreach(i = 1:n_folds,
                                      .packages = pkgs) %dopar% amiss::cv_loop(
                                        config,
                                        fold_paths[[i]],
                                        output_path,
                                        i
                                      )

  ### Gathering results ###
  flog.pid.info("PROGRESS Gathering results from cross-validation")
  rf_results <- lapply(results_triplet, function(x) x[["rf_results"]])
  xg_results <- lapply(results_triplet, function(x) x[["xg_results"]])
  lr_results <- lapply(results_triplet, function(x) x[["lr_results"]])
  rf_results <- do.call(rbind, rf_results)
  xg_results <- do.call(rbind, xg_results)
  lr_results <- do.call(rbind, lr_results)
  rf_results <- rename_methods(rf_results)
  xg_results <- rename_methods(xg_results)
  lr_results <- rename_methods(lr_results)

  flog.pid.info("PROGRESS Gathering results from cross-validation")
  rf_results_per_conseq <- lapply(results_per_conseq_triplet, function(x) x[["rf_results_per_conseq"]])
  xg_results_per_conseq <- lapply(results_per_conseq_triplet, function(x) x[["xg_results_per_conseq"]])
  lr_results_per_conseq <- lapply(results_per_conseq_triplet, function(x) x[["lr_results_per_conseq"]])
  rf_results_per_conseq <- do.call(rbind, rf_results_per_conseq)
  xg_results_per_conseq <- do.call(rbind, xg_results_per_conseq)
  lr_results_per_conseq <- do.call(rbind, lr_results_per_conseq)
  rf_results_per_conseq <- rename_methods(rf_results_per_conseq)
  xg_results_per_conseq <- rename_methods(xg_results_per_conseq)
  lr_results_per_conseq <- rename_methods(lr_results_per_conseq)

  ### Writing output ###
  rf_output_path <- file.path(output_path, FILE_RF_CROSSVALIDATION_RESULTS_CSV)
  flog.pid.info("OUTPUT Writing results from cross-validation for RF into delimited file at %s", rf_output_path)
  write.csv(rf_results, rf_output_path)

  xg_output_path <- file.path(output_path, FILE_XGBOOST_CROSSVALIDATION_RESULTS_CSV)
  flog.pid.info("OUTPUT Writing results from cross-validation for XGBoost into delimited file at %s", xg_output_path)
  write.csv(xg_results, xg_output_path)

  lr_output_path <- file.path(output_path, FILE_LR_CROSSVALIDATION_RESULTS_CSV)
  flog.pid.info("OUTPUT Writing results from cross-validation for LR into delimited file at %s", lr_output_path)
  write.csv(lr_results, lr_output_path)

  rf_output_per_conseq_path <- file.path(output_path, FILE_RF_CROSSVALIDATION_RESULTS_PER_CONSEQUENCE_CSV)
  flog.pid.info("OUTPUT Writing results per consequence from cross-validation for RF into delimited file at %s", rf_output_per_conseq_path)
  write.csv(rf_results_per_conseq, rf_output_per_conseq_path)

  xg_output_per_conseq_path <- file.path(output_path, FILE_XGBOOST_CROSSVALIDATION_RESULTS_PER_CONSEQUENCE_CSV)
  flog.pid.info("OUTPUT Writing results per consequence from cross-validation for XGBoost into delimited file at %s", xg_output_per_conseq_path)
  write.csv(xg_results_per_conseq, xg_output_per_conseq_path)

  lr_output_per_conseq_path <- file.path(output_path, FILE_LR_CROSSVALIDATION_RESULTS_PER_CONSEQUENCE_CSV)
  flog.pid.info("OUTPUT Writing results per consequence from cross-validation for LR into delimited file at %s", lr_output_per_conseq_path)
  write.csv(lr_results_per_conseq, lr_output_per_conseq_path)

  flog.pid.info("DONE 11_cross_validation.R")
}
