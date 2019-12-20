
#' Produce a function that replaces all missing values
#' with column-specific outputs
#'
#' This function is intended to be used to produce univariate
#' imputation functions that produce a single value.
#'
#' @param f Function that accepts a numeric vector as input
#'
#' @return A function that takes a data.frame and replaces
#' each missing value in each column with the output of `f`
#' on that column.
single_value_univariate_imputation <- function(f) {
  replace_na <- function(column) {
    missing <- is.na(column)
    impute_value <- f(column[!missing])
    column[missing] <- rep(impute_value, sum(missing))
    attr(column, "imputation_estimates") <- impute_value
    return(column)
  }
  imp_func <- function(dataframe) {
    imp_data <- lapply(dataframe, replace_na)
    estimates <- lapply(imp_data, function(col) attr(col, "imputation_estimates"))
    names(estimates) <- colnames(dataframe)
    df <- data.frame(imp_data)
    attr(df, "imputation_estimates") <- estimates
    return(df)
  }
  return(imp_func)
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

reimpute <- function(dataframe, value) data.frame(lapply(enumerate(dataframe), function(col) {
  col$value[is.na(col$value)] <- value[[col$name]]
  return(col$value)
}))

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

    }, error = function(e) flog.debug(e))
  )

  result <- list(
    completed_datasets = completed_datasets,
    imputation_object = imputation_object
  )
  attr(result, "timing") <- timing

  return(result)
}

#' Run and time BPCA
#'
#' @param data A data.frame to impute
#' @param hyperparams list of named values that will be fed as arguments to `pcaMethods::pca`
#'
#' @return A named two-element list, where
#' first element is a list of completed datasets (of length `times`),
#' second element is the `pca`-returned `pcaRes` object.
#' The list has an additional attribute `timing`, which contains the timing information.
run_bpca <- function(data, hyperparams) {

  data <- scale(as.matrix(data), TRUE, TRUE)

  timing <- system.time({

    imputation <- NULL

    tryCatch({
      imputation <- do.call(pcaMethods::pca, c(list(object = data, method = "bpca"), hyperparams))
    }, error = function(e) {
      flog.debug("Trying to execute BPCA, the following error occurred: %s", e$message)
      flog.debug("Output of checkData: %s", paste(attributes(checkData(data, verbose = TRUE))))
    })

  })

  dat <- data.frame(imputation@completeObs)

  result <- list(
    completed_datasets = list(`1` = dat),
    imputation_object = list(`1` = imputation)
  )
  attr(result, "timing") <- timing

  return(result)
}

#' Run and time kNN
#'
#' @param data A data.frame to impute
#' @param hyperparams list of named values that will be fed as arguments to `DMwR::knnImputation`
#' @param old_data Completed training set to use for finding neighbors
#'
#' @return A named two-element list, where
#' first element is a list of completed datasets (of length `times`),
#' second element is `NULL`.
#' The list has an additional attribute `timing`, which contains the timing information.
run_knn <- function(data, hyperparams, old_data = NULL) {

  timing <- system.time({
    imputation <- NULL
    tryCatch({
      if (!is.null(old_data)) {
        imputation <- do.call(knnImputation, c(list(I(data)), hyperparams, distData = list(old_data)))
      }
      else {
        imputation <- do.call(knnImputation, c(list(I(data)), hyperparams))
      }
    }, error = function(e) {
      flog.debug("Trying to execute knnImputation, the following error occurred: %s", e$message)
    })
  })

  result <- list(
    completed_datasets = list(`1` = imputation),
    imputation_object = NULL
  )
  attr(result, "timing") <- timing

  return(result)

}

#' Run and time addition of missingness indicators
#'
#' For missingness indicators which are perfect copies of each other, only one is kept.
#'
#' @param data A data.frame to add indicators into
#' @param remove_vector The variables whose missingness indicators should not be kept.
#' Used to get matching feature sets between training and test sets.
#'
#' @return A data.frame
missingness_indicators <- function(data, remove_vector = NULL) {

  timing <- system.time({

    miss_inds <- is.na(data)
    if (is.null(remove_vector)) {
      remove_vector <- miss_inds %>% t %>% duplicated
    }
    remove_names <- colnames(miss_inds)[remove_vector]
    miss_inds <- miss_inds[, !remove_vector]
    colnames(miss_inds) <- paste0(colnames(miss_inds), "_missing")

    data <- zero_imp(data)
    data <- cbind(data, miss_inds)

    attr(data, "imputation_estimates") <- remove_names
  })
  attr(data, "timing") <- timing
  return(data)
}
