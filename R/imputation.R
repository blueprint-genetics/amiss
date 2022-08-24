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
#'
#' @importFrom magrittr %>%
#' @importFrom foreach %do%
#' @importFrom foreach %dopar%

single_value_univariate_imputation <- function(f) {
  force(f)
  replace_na <- function(column) {
    missing <- is.na(column)
    impute_value <- f(column[!missing])
    column[missing] <- rep(impute_value, sum(missing))
    attr(column, IMPUTATION_REUSE_PARAMETERS) <- impute_value
    return(column)
  }
  imp_func <- function(df) {

    timing <- system.time({
      cols_w_nas <- sapply(df, . %>% is.na %>% any)
      if (!all(sapply(df[, cols_w_nas, drop = FALSE], is.numeric)))
        stop("All columns with missingness must be numeric")

      numeric_cols <- sapply(df, is.numeric)
      imp_data <- lapply(df[, numeric_cols, drop = FALSE], replace_na)
      estimates <- lapply(imp_data, function(col) attr(col, IMPUTATION_REUSE_PARAMETERS))
      names(estimates) <- names(imp_data)
      df[,numeric_cols] <- data.frame(imp_data)
    })
    attr(df, TIMING_ATTR) <- timing
    attr(df, IMPUTATION_REUSE_PARAMETERS) <- estimates
    return(df)
  }
  return(imp_func)
}

produce_outlier <- function(x) {
  if (!is.numeric(x)) stop("`x` must be a numeric vector")

  abs(max(x, na.rm = TRUE) - min(x, na.rm = TRUE)) * 10
}

max_imp <- single_value_univariate_imputation(max)
min_imp <- single_value_univariate_imputation(min)
mean_imp <- single_value_univariate_imputation(mean)
median_imp <- single_value_univariate_imputation(median)
zero_imp <- single_value_univariate_imputation(function(x) 0.0)
outlier_imp <- single_value_univariate_imputation(produce_outlier)

reimpute <- function(df, value) {
  if (!all(names(value) %in% colnames(df))) stop("Names of `value` and column names of `df` do not match")
  imputed <- lapply(enumerate(df[, names(value), drop = FALSE]), function(col) {
    col$value[is.na(col$value)] <- value[[col$name]]
    return(col$value)
  })
  df[, names(value)] <- data.frame(imputed)
  return(df)
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
      remove_logical <- miss_inds %>% t %>% duplicated
      remove_vector <- colnames(miss_inds)[remove_logical]
    } else {
      remove_logical <- colnames(miss_inds) %in% remove_vector
    }
    miss_inds <- miss_inds[, !remove_logical]
    colnames(miss_inds) <- paste0(colnames(miss_inds), "_missing")

    data <- zero_imp(data)
    data <- cbind(data, miss_inds)

    attr(data, IMPUTATION_REUSE_PARAMETERS) <- remove_vector
  })
  attr(data, TIMING_ATTR) <- timing
  return(data)
}

check_method_results <- function(imputations) {

  sapply(names(imputations), function(method) {

    null_hpsets <- sapply(imputations[[method]], function(x) x[["completed_datasets"]] %>% unlist %>% is.null)
    if (all(null_hpsets)) {
      flog.pid.info("Imputation method %s did not successfully produce any datasets", method)
      return(FALSE)
    }
    return(TRUE)
  })
}

impute_with_hyperparameters <- function(data, impute_function, method_name, hyperparameters, seed, times, ...) {

  if (nrow(hyperparameters) == 0) {
    imputations <- list(imp_hp_1 = impute_function(data, list(), times, ...))
  }
  else {
    imputations <- foreach::foreach(hp_row = 1:nrow(hyperparameters)) %do% {
      flog.pid.info("Imputing with %s, parameters %s", method_name, paste0(names(hyperparameters), ": ", hyperparameters[hp_row, ], collapse = ", "))
      impute_function(data, unlist(hyperparameters[hp_row,]), times, ...)
    } %>% magrittr::set_names(paste0("imp_hp_", 1:nrow(hyperparameters)))
  }
  return(imputations)
}
impute_over_grid <- function(data, hyperparameter_grids, seed, times, ...) {

  imputations <- lapply(enumerate(hyperparameter_grids), function(hyperparameter_grid) {
    impute_with_hyperparameters(data = data, impute_function = method_to_function(hyperparameter_grid$name), method_name = hyperparameter_grid$name, hyperparameters = hyperparameter_grid$value, seed = seed, times = times, ... = ...)
  })
  # Combine timings of different hyperparameter configs
  timings <- do.call(rbind, lapply(imputations, . %>% attr(TIMING_ATTR)))
  # Return them along with the imputation object
  attr(imputations, TIMING_ATTR) <- timings

  names(imputations) <- names(hyperparameter_grids)

  return(imputations)
}

impute_w_hps <- function(data, hp_tree, times, iters, seed){

  imputations <- foreach::foreach(hps = enumerate(hp_tree)) %do% {
    # The imputation parameters estimated from the training set should be used
    # where possible.
    estimates <- attr(hps$value, IMPUTATION_REUSE_PARAMETERS)

    method <- hps$name
    flog.pid.info("Imputing with method %s", method)
    if (method == "missingness_indicators") {

      return(list(completed_datasets = list(`1` = missingness_indicators(data, remove_vector = estimates))))

    } else if (method %in% names(single_value_imputation_hyperparameter_grids)) {

      return(list(completed_datasets = list(`1` = reimpute(df = data, value = estimates))))

    } else {

      flog.pid.debug("Unknown imputation_method: %s ; returning NULL", method)
      return(list(completed_datasets = list(`1` = NULL)))

    }
  }
  names(imputations) <- names(hp_tree)

  completions <- imputations %>% lapply(. %>% magrittr::extract2(1))

  return(completions)
}
