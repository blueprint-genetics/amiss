
run_mice <- function(data, method, hyperparams, times, iterations) {

  imputation_object <- NULL
  completed_datasets <- rep(list(NULL), iterations)

  timing <- system.time(
    tryCatch({

      imputation_object <- mice::mice(data = data,
                                      method = method,
                                      m = times,
                                      maxit = iterations,
                                      printFlag = FALSE,
                                      ... = hyperparams)

      completed_datasets <- mice::complete(imputation_object, action = "all")

    }, error = function(e) print(e))
  )

  result <- list(
    completed_datasets = completed_datasets,
    imputation_object = imputation_object
  )
  attr(result, "timing") <- timing

  return(result)
}

median_imp <- function(col) {
  if (any(is.na(col))) {
    col[is.na(col)] <- median(col, na.rm = TRUE)
  }
  return(col)
}
