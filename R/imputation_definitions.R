
imputation_hyperparameters <- list(

  pmm = list(donors = (0:2)*3 + 1, ridge = c(1e-03, 1e-04, 1e-05, 1e-06, 1e-07, 1e-08), matchtype = 0:2),
  norm.predict = list(),
  norm = list(),
  rf = list(ntree = 0:10 * 2 + 1),
  ri = list(ri.maxit = 0:10 * 2 + 1),
  midastouch = list(ridge = c(1e-03, 1e-04, 1e-05, 1e-06, 1e-07, 1e-08), output = FALSE)
)

imputation_hyperparameter_grids <- lapply(imputation_hyperparameters, expand.grid)
