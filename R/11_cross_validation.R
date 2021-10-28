#' This function constitutes the body inside a cross-validation loop.
#' I.e. it performs training and testing with a given parameter combination,
#' training data and testing data sets and their respective label vectors.
#'
#' @param fold_tr_datas List of training datasets for each cross-validation fold
#' @param fold_te_datas List of test datasets for each cross-validation fold
#' @param fold_tr_outcomes List of training outcome vectors for each cross-validation fold
#' @param fold_te_outcomes List of test outcome vectors for each cross-validation fold
#' @param output_path Path to directory to write results in
#' @param i Fold index
#'
#' @return A pair (2-element list) of results for random forest and logistic regression, respectively.
#' @export
cv_loop <- function(parameters, fold_tr_datas, fold_te_datas, fold_tr_outcomes, fold_te_outcomes, output_path, i) {
  
    flog.pid.info("PROGRESS Starting fold %d", i)
  
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
    
    # Define hyperparameter grid for the selected imputation method. This is a bit complicated due to the
    # `impute_and_train` method being designed to run several imputations; it could be simplified.
    flog.pid.info("PARAMETER %s = %s", IMPUTATION_METHOD, parameters[[IMPUTATION_METHOD]])
    flog.pid.info("PARAMETER Obtaining hyperparameter grid for %s", parameters[[IMPUTATION_METHOD]])
    mice_hyperparameter_grids_in <- NULL
    other_hyperparameter_grids_in <- NULL
    single_value_imputation_hyperparameter_grids_in <- NULL
    if (parameters[[IMPUTATION_METHOD]] %in% names(mice_hyperparameter_grids)) {
       mice_hyperparameter_grids_in <- mice_hyperparameter_grids[ parameters[[IMPUTATION_METHOD]] ] 
    } else if (parameters[[IMPUTATION_METHOD]] %in% names(other_hyperparameter_grids)) {
       other_hyperparameter_grids_in <- other_hyperparameter_grids[ parameters[[IMPUTATION_METHOD]] ] 
    } else if (parameters[[IMPUTATION_METHOD]] %in% names(single_value_imputation_hyperparameter_grids)) {
       single_value_imputation_hyperparameter_grids_in <- single_value_imputation_hyperparameter_grids[ parameters[[IMPUTATION_METHOD]] ] 
    } else stop(
      paste0("Unknown value \"", parameters[[IMPUTATION_METHOD]], "\" for parameter \"", IMPUTATION_METHOD, "\"")
    )
    
    # Do the real work, i.e. imputation, training and testing
    flog.pid.info("PROGRESS Starting imputation and training process")
    impute_and_train(training_path = tr_data_path, outcome_path = tr_outcome_path, output_path = dir_path,
                     mice_hyperparameter_grids = mice_hyperparameter_grids_in, other_hyperparameter_grids = other_hyperparameter_grids_in, single_value_imputation_hyperparameter_grids = single_value_imputation_hyperparameter_grids_in,
                     parameter_list = parameters,
                     cores = 1, seed = 42, lean = TRUE)
    flog.pid.info("PROGRESS Starting test process")
    predict_on_test_set(test_path = te_data_path, outcome_path = te_outcome_path, tr_output_path = dir_path, results_dir_path = file.path(dir_path, "results"), cores = 1, seed = 42, lean = TRUE)
    
    # Obtain and combine results
    rf_results_path <- file.path(dir_path, "results", FILE_RF_PERFORMANCE_CSV)
    flog.pid.info("PROGRESS Obtaining results for RF from delimited file %s", rf_results_path)
    rf_results <- tryCatch({
      rf_results <- read.csv(rf_results_path)
    }, error = function(e) {
      rf_results <- data.frame(t(rep(NA, 15)))
      colnames(rf_results) <- c(METHOD_COLUMN, MODEL_INDEX_COLUMN, TEST_COMPLETION_INDEX_COLUMN, "TP","FP","FN","TN","Accuracy","Brier","MCC","AUC","Sensitivity","Specificity","F1","Precision")
      return(rf_results)
    })
    rf_results$fold <- i
    xg_results_path <- file.path(dir_path, "results", FILE_XGBOOST_PERFORMANCE_CSV)
    flog.pid.info("PROGRESS Obtaining results for XGBoost from delimited file %s", xg_results_path)
    xg_results <- tryCatch({
      xg_results <- read.csv(xg_results_path)
    }, error = function(e) {
      xg_results <- data.frame(t(rep(NA, 15)))
      colnames(xg_results) <- c(METHOD_COLUMN, MODEL_INDEX_COLUMN, TEST_COMPLETION_INDEX_COLUMN, "TP","FP","FN","TN","Accuracy","Brier","MCC","AUC","Sensitivity","Specificity","F1","Precision")
      return(xg_results)
    })
    xg_results$fold <- i
    lr_results_path <- file.path(dir_path, "results", FILE_LR_PERFORMANCE_CSV)
    flog.pid.info("PROGRESS Obtaining results for LR from delimited file %s", lr_results_path)
    lr_results <- tryCatch({
      lr_results <- read.csv(lr_results_path)
    }, error = function(e) {
      lr_results <- data.frame(t(rep(NA, 15)))
      colnames(lr_results) <- c(METHOD_COLUMN, MODEL_INDEX_COLUMN, TEST_COMPLETION_INDEX_COLUMN, "TP","FP","FN","TN","Accuracy","Brier","MCC","AUC","Sensitivity","Specificity","F1","Precision")
      return(lr_results)
    })
    lr_results$fold <- i
    
    flog.pid.info("PROGRESS Finishing fold %d", i)
    return(list(rf_results, xg_results, lr_results))
}

#' Step 11: Cross-validation
#' 
#' This step performs parallel cross-validation with a specified parameter combination.
#' 
#' Parameters used by this step (referenced with constant names,
#' see R/parameters.R for explicit string values):
#' - IMPUTATION_METHOD, which specifies the imputation method to be used
#' 
#' @param preprocessed_data_path Path to directory containing the results from step 02
#' @param output_path Path to directory in which outputs will be written
#' @param parameters_path Path to file specifying parameter combination
#' @param n_folds Number of cross-validation folds
#' @param seed Seed value
#'
#' @return
#' @export
S11_cross_validation <- function(preprocessed_data_path, output_path, parameters_path, n_folds, seed = 10) {
  
  ### Setup ###
  output_path <- normalizePath(output_path, mustWork = FALSE)
  create_dir(output_path)
  
  futile.logger::flog.appender(futile.logger::appender.tee(file.path(output_path, "11_cross_validation.log")))
  futile.logger::flog.threshold(futile.logger::DEBUG)
  flog.pid.info("START 11_cross_validation.R")
  flog.pid.info("OUTPUT Output root path set to %s", output_path)
  
  # Setup paths
  training_data_path <- file.path(preprocessed_data_path, FILE_PREPROCESSED_TRAINING_DATA_CSV) %>% normalizePath
  flog.pid.info("INPUT Reading preprocessed data from delimited file at %s", training_data_path)
  training_data <- read.csv(training_data_path, row.names = 1, as.is = TRUE)
  
  preprocessed_data_path <- normalizePath(preprocessed_data_path)
  preprocessed_outcomes_path <- file.path(preprocessed_data_path, FILE_TRAINING_OUTCOMES_CSV)
  flog.pid.info("INPUT Reading preprocessed outcomes from delimited file at %s", preprocessed_outcomes_path)
  outcomes <- read.csv(preprocessed_outcomes_path, row.names = 1, as.is = TRUE)[[1]]
  
  # Process parameters
  config <- get_config(parameters_path)
  
  if (config[[IMPUTATION_METHOD]] %>% is.null) {
    stop("Required parameter \"" %>% paste0(IMPUTATION_METHOD, "\" not provided"))
  }
  
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
  
  flog.pid.info("PROGRESS Performing cross-validation")
  pkgs <- c(
    "amiss",
    "purrr",
    "doRNG",
    "caret",
    "futile.logger",
    "magrittr",
    "DMwR2",
    "ModelMetrics"
  )
  
  ### Cross-validation ###
  results_triplet <- foreach::foreach(i = 1:length(folds),
                                      .packages = pkgs) %dopar% amiss::cv_loop(
                                        config,
                                        fold_tr_datas,
                                        fold_te_datas,
                                        fold_tr_outcomes,
                                        fold_te_outcomes,
                                        output_path,
                                        i
                                      )
  
  ### Gathering results ###
  flog.pid.info("PROGRESS Gathering results from cross-validation")
  rf_results <- lapply(results_triplet, function(x) x[[1]])
  xg_results <- lapply(results_triplet, function(x) x[[2]])
  lr_results <- lapply(results_triplet, function(x) x[[3]])
  rf_results <- do.call(rbind, rf_results)
  xg_results <- do.call(rbind, xg_results)
  lr_results <- do.call(rbind, lr_results)
  
  rf_results <- rename_methods(rf_results)
  rf_results$method <- reorder(rf_results$method, rf_results$MCC, mean)
  xg_results <- rename_methods(xg_results)
  lr_results <- rename_methods(lr_results)
  
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
  
  flog.pid.info("DONE 11_cross_validation.R")
}
