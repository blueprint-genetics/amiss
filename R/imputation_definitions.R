
mice_imputation_hyperparameters <- list(

  pmm = list(donors = (0:10)*3 + 1, ridge = c(1e-03, 1e-04, 1e-05, 1e-06, 1e-07, 1e-08), matchtype = 0:2),
  norm.predict = list(),
  norm = list(),
  rf = list(ntree = 1:5 * 8 - 4, ntree = c(10, 50, 100, 200))
  #midastouch = list(ridge = c(1e-03, 1e-04, 1e-05, 1e-06, 1e-07, 1e-08), outout = FALSE)
)

mice_hyperparameter_grids <- lapply(mice_imputation_hyperparameters, expand.grid)

deterministic_imputation_hyperparameters <- list(
  knnImputation = list(k = 1:20),
  bpca = list(nPcs = 2:15, maxSteps = 1:8*20),
  missForest = list(mtry = 1:5 * 8 - 4, ntree = c(10, 50, 100, 200))
)

other_hyperparameter_grids <- lapply(deterministic_imputation_hyperparameters, expand.grid)

single_value_imputation_hyperparameter_grids <- list(
  missingness_indicators = "missingness_indicators",
  max_imp = "max_imp",
  min_imp = "min_imp",
  mean_imp = "mean_imp",
  median_imp = "median_imp",
  zero_imp = "zero_imp",
  outlier_imp = "outlier_imp"
) %>% lapply(expand.grid)
