POSITIVE_LABEL <- "positive"
NEGATIVE_LABEL <- "negative"

IMPUTATION_REUSE_PARAMETERS <- "imputation_reuse_parameters"
TIMING_ATTR <- "timing"

SINGLE_IMP_METHODS <- c(
  "missingness_indicators",
  "max_imp",
  "min_imp",
  "mean_imp",
  "median_imp",
  "zero_imp",
  "outlier_imp"
)

FILE_FULL_CLINGEN_CSV <- "full_clingen.csv"
FILE_MERGED_DATA_CSV <- "merged_data.csv"
FILE_TRAINING_DATA_CSV <- "training_data.csv"
FILE_TEST_DATA_CSV <- "test_data.csv"
FILE_PREPROCESSED_TRAINING_DATA_CSV <- "preprocessed_training_data.csv"
FILE_PREPROCESSED_TEST_DATA_CSV <- "preprocessed_test_data.csv"
FILE_TRAINING_DATA_FOR_STATS_CSV <- "training_data_for_stats.csv"
FILE_TRAINING_OUTCOMES_CSV <- "training_outcomes.csv"
FILE_TEST_OUTCOMES_CSV <- "test_outcomes.csv"

FILE_SIMULATED_FILE_LIST_CSV <- "simulated_file_list.csv"
FILE_SUCCESSFULLY_SIMULATED_FILE_LIST_CSV <- "successfully_simulated_file_list.csv"
FILE_SIMULATED_RF_RESULTS_CSV <- "simulated_rf_results.csv"
FILE_SIMULATED_LR_RESULTS_CSV <- "simulated_lr_results.csv"

FILE_RF_RMSE_CSV <- "rf_rmse.csv"
FILE_LR_RMSE_CSV <- "lr_rmse.csv"

FILE_RF_CLASSIFIERS_RDS <- "rf_classifiers.rds"
FILE_XGBOOST_CLASSIFIERS_RDS <- "xgboost_classifiers.rds"
FILE_LR_CLASSIFIERS_RDS <- "lr_classifiers.rds"

FILE_LR_PERFORMANCE_CSV <- "lr_performance.csv"
FILE_RF_PERFORMANCE_CSV <- "rf_performance.csv"
FILE_XGBOOST_PERFORMANCE_CSV <- "xgboost_performance.csv"

FILE_LR_PERFORMANCE_PER_CONSEQUENCE_CSV <- "lr_performance_per_consequence.csv"
FILE_RF_PERFORMANCE_PER_CONSEQUENCE_CSV <- "rf_performance_per_consequence.csv"
FILE_XGBOOST_PERFORMANCE_PER_CONSEQUENCE_CSV <- "xgboost_performance_per_consequence.csv"
FILE_SIMULATED_RF_RESULTS_PER_CONSEQUENCE_CSV <- "simulated_rf_results_per_consequence.csv"
FILE_SIMULATED_LR_RESULTS_PER_CONSEQUENCE_CSV <- "simulated_lr_results_per_consequence.csv"

FILE_RF_HP_CONFIGS_RDS <- "rf_hp_configs.rds"
FILE_XGBOOST_HP_CONFIGS_RDS <- "xgboost_hp_configs.rds"
FILE_LR_HP_CONFIGS_RDS <- "lr_hp_configs.rds"

FILE_FINAL_FEATURES_RDS <- "final_features.rds"

METHOD_COLUMN <- "method"
MODEL_INDEX_COLUMN <- "model_ix"
TEST_COMPLETION_INDEX_COLUMN <- "test_completion_ix"

IMPUTE_TIMES <- 10L
SIMULATION_IMPUTE_TIMES <- 1L

MICE_ITERATIONS <- 10L

SIMULATION_HP_SAMPLE_SIZE <- 8L

RF_HYPERPARAMETER_GRID <- data.frame(mtry = 1:5 * 8 - 1)
XGBOOST_HYPERPARAMETER_GRID <- expand.grid(eta = (0:2) / 2,
                                           nrounds = (1:2) * 200,
                                           #n_estimators = (1:5) * 100,
                                           gamma = c(0, 1, 10, 100),
                                           max_depth = (1:2)*5,
                                           min_child_weight = 1,
                                           subsample = 1,
                                           #reg_lambda = c(0,1),
                                           colsample_bytree = (1:2) / 2)

UNIQUENESS_CUTOFF_PERCENTAGE <- 1

FILE_RF_RUNTIMES_CSV <- "rf_run_times.csv"
FILE_XGBOOST_RUNTIMES_CSV <- "xgboost_run_times.csv"
FILE_LR_RUNTIMES_CSV <- "lr_run_times.csv"

FILE_RF_IMPUTERS_RDS <- "rf_imputers.rds"
FILE_XGBOOST_IMPUTERS_RDS <- "xgboost_imputers.rds"
FILE_LR_IMPUTERS_RDS <- "lr_imputers.rds"

CONSEQUENCE_COLUMN <- "Consequence.x"

FILE_CROSSVALIDATION_TRAINING_DATA <- "cv_training_data"
FILE_CROSSVALIDATION_TEST_DATA <- "cv_test_data"
FILE_CROSSVALIDATION_TRAINING_OUTCOMES <- "cv_training_outcome"
FILE_CROSSVALIDATION_TEST_OUTCOMES <- "cv_test_outcome"

FILE_RF_CROSSVALIDATION_RESULTS_CSV <- "cv_rf_results.csv"
FILE_XGBOOST_CROSSVALIDATION_RESULTS_CSV <- "cv_xg_results.csv"
FILE_LR_CROSSVALIDATION_RESULTS_CSV <- "cv_lr_results.csv"
FILE_RF_CROSSVALIDATION_RESULTS_PER_CONSEQUENCE_CSV <- "cv_rf_pc_results.csv"
FILE_XGBOOST_CROSSVALIDATION_RESULTS_PER_CONSEQUENCE_CSV <- "cv_xg_pc_results.csv"
FILE_LR_CROSSVALIDATION_RESULTS_PER_CONSEQUENCE_CSV <- "cv_lr_pc_results.csv"
