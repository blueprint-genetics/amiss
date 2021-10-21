
single_value_imputation_hyperparameter_grids <- list(
  missingness_indicators = "missingness_indicators",
  max_imp = "max_imp",
  min_imp = "min_imp",
  mean_imp = "mean_imp",
  median_imp = "median_imp",
  zero_imp = "zero_imp",
  outlier_imp = "outlier_imp"
) %>% lapply(expand.grid)
