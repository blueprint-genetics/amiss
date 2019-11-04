
single_value_univariate_imputation <- function(f) {
  replace_na <- function(column) {
    missing <- is.na(column)
    impute_value <- f(column[!missing])
    column[missing] <- rep(impute_value, sum(missing))
    return(column)
  }
  return(function(dataframe) lapply(dataframe, replace_na))
}

produce_outlier <- function(x) {
  abs(max(x, na.rm = TRUE) - min(x, na.rm = TRUE)) * 10
}

max_imp <- single_value_univariate_imputation(max)
min_imp <- single_value_univariate_imputation(min)
mean_imp <- single_value_univariate_imputation(mean)
median_imp <- single_value_univariate_imputation(median)
zero_imp <- single_value_univariate_imputation(function(x) 0.0)
outlier_imp <- single_value_univariate_imputation(produce_outlier)

#' Run and time `mice `
#'
#' @param data data.frame to impute
#' @param method imputation method
#' @param hyperparams A named list of hyperparameters for this run
#' @param times Number of times to multiply impute
#' @param iterations How many iterations to run mice for
#'
#' @return A named two-element list, where
#' first element is a list of completed datasets (of length `times`), 
#' second element is the `mice`-returned `mids` object. 
#' The list has an additional attribute `timing`, which contains the timing information.
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

#' Impute a numeric vector with its median
#'
#' @param col Numeric vector
#'
#' @return The original vector, but with missing values replaced by the vector's median value
# median_imp <- function(col) {
#   if (any(is.na(col))) {
#     col[is.na(col)] <- median(col, na.rm = TRUE)
#   }
#   return(col)
# }
