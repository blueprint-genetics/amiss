
mice_imputation_hyperparameters <- list(

  pmm = list(donors = (0:9)*4 + 1, ridge = c(1e-03, 1e-04, 1e-05, 1e-06, 1e-07, 1e-08), matchtype = 0:2),
  norm.predict = list(),
  norm = list(),
  rf = list(ntree = (0:35) * 15 + 1)
)

mice_hyperparameter_grids <- lapply(mice_imputation_hyperparameters, expand.grid)

other_imputation_hyperparameters <- list(
  knnImputation = list(k = 1:5),
  bpca = list(nPcs = 2:15, maxSteps = 1:8*20),
  missForest = list(mtry = 1:3 * 15 - 10, ntree = c(50, 100, 200, 500))
)

other_hyperparameter_grids <- lapply(other_imputation_hyperparameters, expand.grid)

single_value_imputation_hyperparameter_grids <- list(
  missingness_indicators = "missingness_indicators",
  max_imp = "max_imp",
  min_imp = "min_imp",
  mean_imp = "mean_imp",
  median_imp = "median_imp",
  zero_imp = "zero_imp",
  outlier_imp = "outlier_imp"
) %>% lapply(expand.grid)
