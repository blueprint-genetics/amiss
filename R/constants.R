POSITIVE_LABEL<- "positive"
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

FILE_RF_CLASSIFIERS_RDS <- "rf_classifiers.rds"
FILE_LR_CLASSIFIERS_RDS <- "lr_classifiers.rds"

FILE_LR_PERFORMANCE_CSV <- "lr_performance.csv"
FILE_RF_PERFORMANCE_CSV <- "rf_performance.csv"

FILE_RF_HP_CONFIGS_RDS <- "rf_hp_configs.rds"
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

UNIQUENESS_CUTOFF_PERCENTAGE <- 1

FILE_RF_RUNTIMES_CSV <- "rf_run_times.csv"
FILE_LR_RUNTIMES_CSV <- "lr_run_times.csv"

FILE_RF_IMPUTERS_RDS <- "rf_imputers.rds"
FILE_LR_IMPUTERS_RDS <- "lr_imputers.rds"