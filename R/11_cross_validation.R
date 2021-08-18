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
    dir_path <- file.path(output_path, paste0("fold_", i))
    create_dir(dir_path)
    
    tr_data_path <- file.path(dir_path, paste0(FILE_CROSSVALIDATION_TRAINING_DATA, "_", i, ".csv"))
    tr_outcome_path <- file.path(dir_path, paste0(FILE_CROSSVALIDATION_TRAINING_OUTCOMES, "_", i, ".csv"))
    te_data_path <- file.path(dir_path, paste0(FILE_CROSSVALIDATION_TEST_DATA, "_", i, ".csv"))
    te_outcome_path <- file.path(dir_path, paste0(FILE_CROSSVALIDATION_TEST_OUTCOMES, "_", i, ".csv") )
    
    write.csv(fold_tr_datas[[i]], tr_data_path)
    write.csv(fold_tr_outcomes[[i]],  tr_outcome_path)
    write.csv(fold_te_datas[[i]], te_data_path)
    write.csv(fold_te_outcomes[[i]], te_outcome_path)
    
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
    
    impute_and_train(training_path = tr_data_path, outcome_path = tr_outcome_path, output_path = dir_path,
                     mice_hyperparameter_grids = mice_hyperparameter_grids_in, other_hyperparameter_grids = other_hyperparameter_grids_in, single_value_imputation_hyperparameter_grids = single_value_imputation_hyperparameter_grids_in,
                     cores = 1, seed = 42, lean = TRUE)
    predict_on_test_set(test_path = te_data_path, outcome_path = te_outcome_path, tr_output_path = dir_path, results_dir_path = file.path(dir_path, "results"), cores = 1, seed = 42, lean = TRUE)
    
    rf_results <- read.csv(file.path(dir_path, "results", FILE_RF_PERFORMANCE_CSV))
    rf_results$fold <- i
    xg_results <- read.csv(file.path(dir_path, "results", FILE_XGBOOST_PERFORMANCE_CSV))
    xg_results$fold <- i
    lr_results <- read.csv(file.path(dir_path, "results", FILE_LR_PERFORMANCE_CSV))
    lr_results$fold <- i
    
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
  
  training_data <- read.csv(file.path(preprocessed_data_path, FILE_PREPROCESSED_TRAINING_DATA_CSV), row.names = 1, as.is = TRUE)
  outcomes <- read.csv(file.path(preprocessed_data_path, FILE_TRAINING_OUTCOMES_CSV), row.names = 1, as.is = TRUE)[[1]]
  rows <- NROW(training_data)
  
  config <- get_config(parameters_path)
  
  # Reorder rows
  reordering <- sample(1:rows, rows, replace = FALSE)
  training_data <- training_data[reordering,]
  outcomes <- outcomes[reordering]
  
  folds <- replicate(n_folds, sample(1:rows, 0.7*rows, replace = TRUE), simplify = FALSE)
  
  fold_tr_datas <- lapply(folds, function(fold) training_data[-fold, ])
  fold_tr_outcomes <- lapply(folds, function(fold) outcomes[-fold])
  fold_te_datas <- lapply(folds, function(fold) training_data[fold, ])
  fold_te_outcomes <- lapply(folds, function(fold) outcomes[fold])
  
  create_dir(output_path)
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
  
  write.csv(rf_results, file.path(output_path, FILE_RF_CROSSVALIDATION_RESULTS_CSV))
  write.csv(xg_results, file.path(output_path, FILE_XGBOOST_CROSSVALIDATION_RESULTS_CSV))
  write.csv(lr_results, file.path(output_path, FILE_LR_CROSSVALIDATION_RESULTS_CSV))
  
  # ggplot2::ggsave(filename = file.path(output_path, "si_cv_double_boxplots.pdf"),
  #        plot = doubleboxplot("MCC", rf_results, lr_results, FALSE),
  #        device = "pdf", width = 170, height = 180, units = "mm")
  
}
