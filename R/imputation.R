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
#' @importFrom doRNG %dorng%
#'
#' @export
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

#' @export
produce_outlier <- function(x) {
  if (!is.numeric(x)) stop("`x` must be a numeric vector")

  abs(max(x, na.rm = TRUE) - min(x, na.rm = TRUE)) * 10
}

#' @export
max_imp <- single_value_univariate_imputation(max)
#' @export
min_imp <- single_value_univariate_imputation(min)
#' @export
mean_imp <- single_value_univariate_imputation(mean)
#' @export
median_imp <- single_value_univariate_imputation(median)
#' @export
zero_imp <- single_value_univariate_imputation(function(x) 0.0)
#' @export
outlier_imp <- single_value_univariate_imputation(produce_outlier)

#' Reimpute a data.frame with constant values
#'
#' @param df data.frame to impute
#' @param value named list of values for every column to impute
#'
#' @return imputed data.frame
#' @export
reimpute <- function(df, value) {

  if (!all(names(value) %in% colnames(df))) stop("Names of `value` and column names of `df` do not match")
  imputed <- lapply(enumerate(df[, names(value), drop = FALSE]), function(col) {
    col$value[is.na(col$value)] <- value[[col$name]]
    return(col$value)
  })
  df[, names(value)] <- data.frame(imputed)
  return(df)
}

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
#'
#' @export
run_mice <- function(data, method, hyperparams, times, iterations) {

  imputation_object <- NULL
  completed_datasets <- rep(list(NULL), times)

  timing <- system.time(
    result <- tryCatch({

      imputation_object <- mice::mice(data = data,
                                      method = method,
                                      m = times,
                                      maxit = iterations,
                                      printFlag = FALSE,
                                      ... = hyperparams)

      completed_datasets <- mice::complete(imputation_object, action = "all")
      if (sapply(completed_datasets, function(d) any(is.na(d))) %>% any) {
        stop("Error: missing values remain after imputation")
      }

      list(
        completed_datasets = completed_datasets,
        imputation_object = imputation_object
      )

    }, error = function(e) {
      flog.pid.debug(e)
      return(list(
        completed_datasets = rep(list(NULL), times),
        imputation_object = NULL
      ))
    })
  )

  attr(result, TIMING_ATTR) <- timing

  return(result)
}
#' @export
run_mice_pmm <- function(data,hyperparameters, times, iterations) {
  return(run_mice(data, "pmm", hyperparameters, times, iterations))
}
#' @export
run_mice_norm <- function(data,hyperparameters, times, iterations) {
  return(run_mice(data, "norm", hyperparameters, times, iterations))
}
#' @export
run_mice_norm.predict <- function(data,hyperparameters, times, iterations) {
  return(run_mice(data, "norm.predict", hyperparameters, times, iterations))
}
#' @export
run_mice_rf <- function(data,hyperparameters, times, iterations) {
  return(run_mice(data, "rf", hyperparameters, times, iterations))
}

#' Run and time BPCA
#'
#' @param data A data.frame to impute
#' @param hyperparams list of named values that will be fed as arguments to `pcaMethods::pca`
#'
#' @return A named two-element list, where
#' first element is a list of completed datasets (of length `1`),
#' second element is the `pca`-returned `pcaRes` object.
#' The list has an additional attribute `timing`, which contains the timing information.
#' @export
run_bpca <- function(data, hyperparams, times = NULL, iterations = NULL) {

  data <- as.matrix(data)

  timing <- system.time({

    imputation <- NULL

    tryCatch({
      imputation <- do.call(pcaMethods::pca, c(list(object = data, method = "bpca"), hyperparams, list(scale = "none", center = FALSE)))
    }, error = function(e) {
      flog.pid.debug("Trying to execute BPCA, the following error occurred: %s", e$message)
      flog.pid.debug("Output of checkData: %s", paste(attributes(checkData(data, verbose = TRUE))))
    })

  })

  if(!is.null(imputation)) {
    df <- data.frame(imputation@completeObs)
  } else {
    df <- NULL
  }

  result <- list(
    completed_datasets = list(`1` = df),
    imputation_object = list(`1` = imputation)
  )
  attr(result, TIMING_ATTR) <- timing

  return(result)
}
#' Run and time kNN
#'
#' @param data A data.frame to impute
#' @param hyperparams list of named values that will be fed as arguments to `DMwR::knnImputation`
#' @param old_data Completed training set to use for finding neighbors
#'
#' @return A named two-element list, where
#' first element is a list of completed datasets (of length `1`),
#' second element is `NULL`.
#' The list has an additional attribute `timing`, which contains the timing information.
#' @export
run_knn <- function(data, hyperparams, times = NULL, iterations = NULL, old_data = NULL) {

  timing <- system.time({
    imputation <- NULL
    tryCatch({
      if (!is.null(old_data)) {
        imputation <- do.call(DMwR::knnImputation, c(list(data = I(data)), hyperparams, distData = list(old_data)))
      }
      else {
        imputation <- do.call(DMwR::knnImputation, c(list(data = I(data)), hyperparams))
      }
    }, error = function(e) {
      flog.pid.debug("Trying to execute knnImputation, the following error occurred: %s", e$message)
    })
  })

  result <- list(
    completed_datasets = list(`1` = imputation),
    imputation_object = NULL
  )
  attr(result, TIMING_ATTR) <- timing

  return(result)

}

#' Run and time missForest
#'
#' @param data A data.frame to impute
#' @param hyperparams list of named values that will be fed as arguments to `missForest::missForest`
#' @param times number of datasets to produce
#'
#' @return A named two-element list, where
#' first element is a list of completed datasets (of length `times`),
#' second element is `NULL`.
#' The list has an additional attribute `timing`, which contains the timing information.
#' @export
run_missforest <- function(data, hyperparams, times, iterations = NULL) {

  timing <- system.time({
    completed_datasets <- foreach::foreach(i = 1:times) %do% {
      imputation <- NULL
      tryCatch({
        imputation <- do.call(missForest::missForest, c(list(I(data)), hyperparams))
      }, error = function(e) {
        flog.pid.debug("Trying to execute missForest, the following error occurred: %s", e$message)
      })
      return(imputation$ximp)
    } %>% magrittr::set_names(1:times)
  })

  result <- list(
    completed_datasets = completed_datasets,
    imputation_object = NULL
  )
  attr(result, TIMING_ATTR) <- timing

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
#' @export
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

#' Check that a method has produced at least one imputed dataset
#'
#' @param imputations
#'
#' @return
#' @export
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

#' NULL-returning method for default value of `method_to_function`
#'
#' @param x Method name
#'
#' @return NULL
#' @export
null_method = function(x) {
  flog.pid.info("Method %s is not implemented", x)
  return(NULL)
}

#' Mapping from method names to imputation method wrappers
#'
#' @param method Name of imputation method
#'
#' @return Wrapper for imputation method
#' @export
method_to_function <- function(method) {
  return(
    switch(method,
           bpca = run_bpca,
           knnImputation = run_knn,
           missForest = run_missforest,
           pmm = run_mice_pmm,
           norm = run_mice_norm,
           norm.predict = run_mice_norm.predict,
           rf = run_mice_rf,
           null_method)
  )
}
#' Impute over a hyperparameter grid for the method
#'
#' @param data data.frame to impute
#' @param impute_function Imputation method wrapper
#' @param method_name Name of imputation method
#' @param hyperparameters Hyperparameter grid data.frame
#' @param seed Seed to set before imputation
#' @param times Times to impute (if using stochastic or multiple imputation)
#' @param ... Additional parameters to pass to imputation method wrapper
#'
#' @return List of imputed data
#' @export
impute_with_hyperparameters <- function(data, impute_function, method_name, hyperparameters, seed, times, ...) {

  if (nrow(hyperparameters) == 0) {
    imputations <- list(imp_hp_1 = impute_function(data, list(), times, ...))
  }
  else {
    imputations <- foreach::foreach(hp_row = 1:nrow(hyperparameters), .options.RNG = seed) %dorng% {
      flog.pid.info("Imputing with %s, parameters %s", method_name, paste0(names(hyperparameters), ": ", hyperparameters[hp_row, ], collapse = ", "))
      impute_function(data, unlist(hyperparameters[hp_row,]), times, ...)
    } %>% magrittr::set_names(paste0("imp_hp_", 1:nrow(hyperparameters)))
  }
  return(imputations)
}
#' Impute the same dataset using multiple methods defined by set of hyperparameter grids
#'
#' Used in impute_and_train to produce the full set of imputed datasets for every type (mice, other, single) of imputation method.
#'
#' @param data data.frame to impute
#' @param hyperparameter_grids List of hyperparameter grids, named by method names
#' @param seed Seed to set before imputation
#' @param times Times to impute (if using stochastic or multiple imputation)
#' @param ... Additional parameters to pass to imputation method wrapper
#'
#' @return List of imputed data
#' @export
group_impute <- function(data, hyperparameter_grids, seed, times, ...) {

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

#' (re-)Impute using specific hyperparameters
#'
#' Used by predict_on_test_set to reimpute the test data using specific hyperparameters chosen in training.
#'
#' @param data data.frame to impute
#' @param hp_tree Tree of hyperparameters
#' @param times Times to impute (if using stochastic or multiple imputation)
#' @param iters Number of iterations when using mice
#' @param seed Seed to set before imputation
#'
#' @return List of imputed data
#' @export
impute_w_hps <- function(data, hp_tree, times, iters, seed){

  imputations <- foreach::foreach(hps = enumerate(hp_tree), .options.RNG = seed) %dorng% {
    # The imputation parameters estimated from the training set should be used
    # where possible.
    estimates <- attr(hps$value, IMPUTATION_REUSE_PARAMETERS)

    method <- hps$name
    flog.pid.info("Imputing with method %s", method)
    if (method %in% names(mice_imputation_hyperparameters)) {

      return(run_mice(data = data, method = method, hyperparams = hps$value, times = times, iterations = iters))

    } else if (method == "bpca") {

      return(run_bpca(data, hps$value))

    } else if (method == "knnImputation") {

      return(run_knn(data, hps$value, old_data = estimates))

    } else if (method == "missForest") {

      return(run_missforest(data, hps$value, times = times))

    } else if (method == "missingness_indicators") {

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
