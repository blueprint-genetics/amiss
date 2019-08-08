
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
zero_imp <- single_value_univariate_imputation(function(x) 0.0)
outlier_imp <- single_value_univariate_imputation(produce_outlier)

