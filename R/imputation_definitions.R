
imputation_hyperparameters <- list(

  pmm = list(donors = (0:2)*3 + 1, ridge = c(1e-03, 1e-04, 1e-05, 1e-06, 1e-07, 1e-08), matchtype = 0:2)

)

imputation_hyperparameter_grids <- lapply(imputation_hyperparameters, expand.grid)
