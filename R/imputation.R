library(mice)
library(foreach)
library(doParallel)
library(doRNG)
library(magrittr)
library(pcaMethods)
library(DMwR2)
library(missForest)
library(here)

source(here("R", "utils.R"))
source(here("R", "recursive_application.R"))
source(here("R", "constants.R"))

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
  force(f)
  replace_na <- function(column) {
    missing <- is.na(column)
    impute_value <- f(column[!missing])
    column[missing] <- rep(impute_value, sum(missing))
    attr(column, "imputation_reuse_parameters") <- impute_value
    return(column)
  }
  imp_func <- function(dataframe) {

    timing <- system.time({
      cols_w_nas <- sapply(dataframe, . %>% is.na %>% any)
      if (!all(sapply(dataframe[,cols_w_nas, drop = FALSE], is.numeric))) stop("All columns with missingness must be numeric")
      imp_data <- lapply(dataframe, replace_na)
      estimates <- lapply(imp_data, function(col) attr(col, "imputation_reuse_parameters"))
      names(estimates) <- colnames(dataframe)
      df <- data.frame(imp_data)
    })
    attr(df, "timing") <- timing
    attr(df, "imputation_reuse_parameters") <- estimates
    return(df)
  }
  return(imp_func)
}

produce_outlier <- function(x) {
  if(!is.numeric(x)) stop("`x` must be a numeric vector")

  abs(max(x, na.rm = TRUE) - min(x, na.rm = TRUE)) * 10
}

max_imp <- single_value_univariate_imputation(max)
min_imp <- single_value_univariate_imputation(min)
mean_imp <- single_value_univariate_imputation(mean)
median_imp <- single_value_univariate_imputation(median)
zero_imp <- single_value_univariate_imputation(function(x) 0.0)
outlier_imp <- single_value_univariate_imputation(produce_outlier)

reimpute <- function(dataframe, value) {
  if (!setequal(names(value), colnames(dataframe))) stop("Names of `value` and column names of `dataframe` do not match")
  imputed <- lapply(enumerate(dataframe), function(col) {
    col$value[is.na(col$value)] <- value[[col$name]]
    return(col$value)
  })
  imputed <- data.frame(imputed)
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

  attr(result, "timing") <- timing

  return(result)
}
run_mice_pmm <- function(data,hyperparameters, times, iterations) {
  return(run_mice(data, "pmm", hyperparameters, times, iterations))
}
run_mice_norm <- function(data,hyperparameters, times, iterations) {
  return(run_mice(data, "norm", hyperparameters, times, iterations))
}
run_mice_norm.predict <- function(data,hyperparameters, times, iterations) {
  return(run_mice(data, "norm.predict", hyperparameters, times, iterations))
}
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
#' first element is a list of completed datasets (of length `1`),
#' second element is `NULL`.
#' The list has an additional attribute `timing`, which contains the timing information.
run_knn <- function(data, hyperparams, times = NULL, iterations = NULL, old_data = NULL) {

  timing <- system.time({
    imputation <- NULL
    tryCatch({
      if (!is.null(old_data)) {
        imputation <- do.call(knnImputation, c(list(data = I(data)), hyperparams, distData = list(old_data)))
      }
      else {
        imputation <- do.call(knnImputation, c(list(data = I(data)), hyperparams))
      }
    }, error = function(e) {
      flog.pid.debug("Trying to execute knnImputation, the following error occurred: %s", e$message)
    })
  })

  result <- list(
    completed_datasets = list(`1` = imputation),
    imputation_object = NULL
  )
  attr(result, "timing") <- timing

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
run_missforest <- function(data, hyperparams, times, iterations = NULL) {

  timing <- system.time({
    completed_datasets <- foreach(i = 1:times) %do% {
      imputation <- NULL
      tryCatch({
        imputation <- do.call(missForest, c(list(I(data)), hyperparams))
      }, error = function(e) {
        flog.pid.debug("Trying to execute missForest, the following error occurred: %s", e$message)
      })
      return(imputation$ximp)
    } %>% set_names(1:times)
  })

  result <- list(
    completed_datasets = completed_datasets,
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
      remove_logical <- miss_inds %>% t %>% duplicated
      remove_vector <- colnames(miss_inds)[remove_logical]
    } else {
      remove_logical <- colnames(miss_inds) %in% remove_vector
    }
    miss_inds <- miss_inds[, !remove_logical]
    colnames(miss_inds) <- paste0(colnames(miss_inds), "_missing")

    data <- zero_imp(data)
    data <- cbind(data, miss_inds)

    attr(data, "imputation_reuse_parameters") <- remove_vector
  })
  attr(data, "timing") <- timing
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

null_method = function(x) {
  flog.pid.info("Method %s is not implemented", x)
  return(NULL)
}
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
impute_with_hyperparameters <- function(data, impute_function, method_name, hyperparameters, seed, times, ...) {

  if (nrow(hyperparameters) == 0) {
    imputations <- list(imp_hp_1 = impute_function(data, list(), times, ...))
  }
  else {
    imputations <- foreach(hp_row = 1:nrow(hyperparameters), .options.RNG = seed) %dorng% {
      flog.pid.info("Imputing with %s, parameters %s", method_name, paste0(names(hyperparameters), ": ", hyperparameters[hp_row, ], collapse = ", "))
      impute_function(data, unlist(hyperparameters[hp_row,]), times, ...)
    } %>% set_names(paste0("imp_hp_", 1:nrow(hyperparameters)))
  }
  return(imputations)
}
impute_over_grid <- function(data, hyperparameter_grids, seed, times, ...) {

  imputations <- lapply(enumerate(hyperparameter_grids), function(hyperparameter_grid) {
    impute_with_hyperparameters(data = data, impute_function = method_to_function(hyperparameter_grid$name), method_name = hyperparameter_grid$name, hyperparameters = hyperparameter_grid$value, seed = seed, times = times, ... = ...)
  })
  # Combine timings of different hyperparameter configs
  timings <- do.call(rbind, lapply(imputations, . %>% attr("timing")))
  # Return them along with the imputation object
  attr(imputations, "timing") <- timings

  names(imputations) <- names(hyperparameter_grids)

  return(imputations)
}

impute_w_hps <- function(data, hp_tree, times, iters, seed){

  imputations <- foreach(hps = enumerate(hp_tree), .options.RNG = seed) %dorng% {
    # The imputation parameters estimated from the training set should be used
    # where possible.
    estimates <- attr(hps$value, "imputation_reuse_parameters")

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

      return(list(completed_datasets = list(`1` = reimpute(dataframe = data, value = estimates))))

    } else {

      flog.pid.debug("Unknown imputation_method: %s ; returning NULL", method)
      return(list(completed_datasets = list(`1` = NULL)))

    }
  }
  names(imputations) <- names(hp_tree)

  completions <- imputations %>% lapply(. %>% extract2(1))

  return(completions)
}
