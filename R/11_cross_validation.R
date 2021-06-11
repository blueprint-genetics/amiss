#' Script 11, cross-validation
#'
#' @param preprocessed_data_path
#' @param output_path
#' @param n_folds
#' @param cores
#' @param seed
#'
#' @return
#' @export
S11_cross_validation <- function(preprocessed_data_path, output_path, n_folds, cores = get_env_cores(), seed = 10) {
  cores <- get_env_cores()
  cl <- doMPI::startMPIcluster(count=cores)
  doMPI::registerDoMPI(cl)
  
  seed <- 10
  set.seed(seed)
  
  training_data <- read.csv(file.path(preprocessed_data_path, FILE_PREPROCESSED_TRAINING_DATA_CSV), row.names = 1, as.is = TRUE)
  outcomes <- read.csv(file.path(preprocessed_data_path, FILE_TRAINING_OUTCOMES_CSV), row.names = 1, as.is = TRUE)[[1]]
  n_folds <- 3
  rows <- NROW(training_data)
  
  # Reorder rows
  reordering <- sample(1:rows, rows, replace = FALSE)
  training_data <- training_data[reordering,]
  outcomes <- outcomes[reordering]
  
  folds <- replicate(n_folds, sample(1:rows, 0.7*rows, replace = TRUE), simplify = FALSE)
  
  fold_tr_datas <- lapply(folds, function(fold) training_data[-fold, ])
  fold_tr_outcomes <- lapply(folds, function(fold) outcomes[-fold])
  fold_te_datas <- lapply(folds, function(fold) training_data[fold, ])
  fold_te_outcomes <- lapply(folds, function(fold) outcomes[fold])
  
  create_dir(here("output", "cv"))
  
  results_pair <- foreach(i = 1:length(folds), .packages = c("purrr", "doRNG", "caret", "futile.logger", "here", "magrittr", "DMwR2", "ModelMetrics"), .export = c("max_imp", "min_imp", "mean_imp", "median_imp", "produce_outlier", "outlier_imp")) %dopar% {
    
    set.seed(seed)
    
    impute_and_train(training_path = tr_data_path, outcome_path = tr_outcome_path, output_path = dir_path,
                     mice_hyperparameter_grids = mice_hyperparameter_grids, other_hyperparameter_grids = other_hyperparameter_grids, single_value_imputation_hyperparameter_grids = single_value_imputation_hyperparameter_grids,
                     cores = 1, seed = 42, lean = TRUE)
    predict_on_test_set(test_path = te_data_path, outcome_path = te_outcome_path, tr_output_path = dir_path, results_dir_path = file.path(dir_path, "results"), cores = 1, seed = 42, lean = TRUE)
    
    rf_results <- read.csv(file.path(dir_path, "results", FILE_RF_PERFORMANCE_CSV))
    rf_results$fold <- i
    lr_results <- read.csv(file.path(dir_path, "results", FILE_LR_PERFORMANCE_CSV))
    lr_results$fold <- i
    
    return(list(rf_results, lr_results))
  }
  rf_results <- lapply(results_pair, function(x) x[[1]])
  lr_results <- lapply(results_pair, function(x) x[[2]])
  rf_results <- do.call(rbind, rf_results)
  lr_results <- do.call(rbind, lr_results)
  
  rf_results <- rename_methods(rf_results)
  rf_results$method <- reorder(rf_results$method, rf_results$MCC, mean)
  lr_results <- rename_methods(lr_results)
  
  write.csv(rf_results, here("output", "cv", FILE_RF_CROSSVALIDATION_RESULTS_CSV))
  write.csv(lr_results, here("output", "cv", FILE_LR_CROSSVALIDATION_RESULTS_CSV))
  
  ggsave(filename = here("output", "cv", "si_cv_double_boxplots.pdf"), 
         plot = doubleboxplot("MCC", rf_results, lr_results, FALSE),
         device = "pdf", width = 170, height = 180, units = "mm")
  
  
  #mpi.finalize()
  #mpi.exit()
  #closeCluster(cl)
}
