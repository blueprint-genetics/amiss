#' Impute training set and train classifier models on imputed training set
#'
#' This function performs imputation of and training on the training set, as well as preprocessing steps that may differ
#' between different folds or that do sampling. Many of these are on or off depending on parameters.
#'
#' Parameters used by this step (presented via constant names; see R/parameters.R for explicit string values):
#' - DOWNSAMPLING, which specifies whether the label majority class should be downsampled to the size of the minority
#' class,
#' - FEATURE_SAMPLING_PERCENTAGE, which specifies the percentage of features that should be kept when doing feature
#' sampling,
#' - TRAINING_DATA_SAMPLING_PERCENTAGE, which specifies the percentage of rows that should be kept when doing training
#' data sampling,
#' - NZV_CHECK, which specifies whether features with near-zero variance should be removed,
#' - CORRELATION_CHECK, which specifies whether features that are highly correlated with another feature should be
#' removed,
#' - HYPERPARAMETER_SEARCH_TYPE, which specifies whether grid or random search should be used for optimizing classifier
#' hyperparameters
#' - CATEGORICAL_ENCODING, which specifies whether categorical features should be encoded as dummy variables
#' - IMPUTATION_METHOD, which specifies the imputation method that should be used.
#'
#' The process writes into the output folder two files per classification model type, one containing the model and one
#' containing the winning hyperparameter configuration for the imputation model (which for single value imputation
#' methods is actually just the constant value which is used for imputation).
#'
#' @param training_path Path to the training data CSV-file
#' @param outcome_path Path to the training outcomes CSV-file
#' @param output_path Path to the output folder
#' @param single_value_imputation_hyperparameter_grids Specification of the set of imputation
#' methods that should be used
#' @param parameter_list List containing all the abovementioned parameters and their values
#' @param seed Seed value
#'
#' @importFrom magrittr %>%
#' @importFrom foreach %do%
#' @export
impute_and_train <- function(training_path,
                             outcome_path,
                             output_path,
                             mice_hyperparameter_grids,
                             other_hyperparameter_grids,
                             single_value_imputation_hyperparameter_grids,
                             parameter_list,
                             cores, seed = 42, lean) {

  ### Setup ###
  flog.pid.info("impute_and_train.R")
  flog.pid.info("PROGRESS Arguments: %s", paste0(list(training_path, outcome_path, output_path, cores, lean), collapse = ", "))

  output_path <- normalizePath(output_path, mustWork = FALSE)
  create_dir(output_path)
  flog.pid.info("OUTPUT Output root folder set to %s", output_path)

  flog.pid.info("PROGRESS Using %d cores", cores)
  if (!is.null(seed)) {
    flog.pid.info("DESIGN_CHOICE Using seed: %d", seed)
    set.seed(seed)
  }

  ### Check whether required parameters are provided ###
  if (parameter_list[[DOWNSAMPLING]] %>% is.null) {
    stop("Required parameter \"" %>% paste0(DOWNSAMPLING, "\" not provided"))
  }
  if (parameter_list[[FEATURE_SAMPLING_PERCENTAGE]] %>% is.null) {
    stop("Required parameter \"" %>% paste0(FEATURE_SAMPLING_PERCENTAGE, "\" not provided"))
  }
  if (parameter_list[[TRAINING_DATA_SAMPLING_PERCENTAGE]] %>% is.null) {
    stop("Required parameter \"" %>% paste0(TRAINING_DATA_SAMPLING_PERCENTAGE, "\" not provided"))
  }
  if (parameter_list[[NZV_CHECK]] %>% is.null) {
    stop("Required parameter \"" %>% paste0(NZV_CHECK, "\" not provided"))
  }
  if (parameter_list[[CORRELATION_CHECK]] %>% is.null) {
    stop("Required parameter \"" %>% paste0(CORRELATION_CHECK, "\" not provided"))
  }
  if (parameter_list[[HYPERPARAMETER_SEARCH_TYPE]] %>% is.null) {
    stop("Required parameter \"" %>% paste0(HYPERPARAMETER_SEARCH_TYPE, "\" not provided"))
  }
  if (parameter_list[[CATEGORICAL_ENCODING]] %>% is.null) {
    stop("Required parameter \"" %>% paste0(CATEGORICAL_ENCODING, "\" not provided"))
  }
  if (parameter_list[[IMPUTATION_METHOD]] %>% is.null) {
    stop("Required parameter \"" %>% paste0(IMPUTATION_METHOD, "\" not provided"))
  }
  if (parameter_list[[CLASSIFIER_METHOD]] %>% is.null) {
    stop("Required parameter \"" %>% paste0(CLASSIFIER_METHOD, "\" not provided"))
  }


  if (!all(parameter_list[[CLASSIFIER_METHOD]] %in% c("xgboost", "rf", "lr"))) {
    stop("Required parameter \"" %>% paste0(CLASSIFIER_METHOD, "\" has value \"") %>% paste0(parameter_list[[CLASSIFIER_METHOD]]) %>% paste0("\" which is not one of 'rf', 'xgboost' or 'lr'"))
  }

  ### Sample imputation hp grids ###
  if (lean) {
    flog.pid.info("DESIGN_CHOICE Sampling imputation hyperparameter grids to %d rows to save computation time", SIMULATION_HP_SAMPLE_SIZE)
    mice_hyperparameter_grids <- lapply(mice_hyperparameter_grids, . %>% sample_max(size = SIMULATION_HP_SAMPLE_SIZE))
    other_hyperparameter_grids <- lapply(other_hyperparameter_grids, . %>% sample_max(size = SIMULATION_HP_SAMPLE_SIZE))
  }
  ### If a specific method chosen, use only that
  if (parameter_list[[IMPUTATION_METHOD]] == "all") {
    parameter_list[[IMPUTATION_METHOD]] <- c(names(mice_hyperparameter_grids), names(other_hyperparameter_grids), names(single_value_imputation_hyperparameter_grids))
  }
  mice_hyperparameter_grids <- mice_hyperparameter_grids[which(names(mice_hyperparameter_grids) %in% parameter_list[[IMPUTATION_METHOD]])]
  other_hyperparameter_grids <- other_hyperparameter_grids[which(names(other_hyperparameter_grids) %in% parameter_list[[IMPUTATION_METHOD]])]
  single_value_imputation_hyperparameter_grids <- single_value_imputation_hyperparameter_grids[which(names(single_value_imputation_hyperparameter_grids) %in% parameter_list[[IMPUTATION_METHOD]])]

  ### Read and process input data ###
  training_path <- normalizePath(training_path)
  flog.pid.info("INPUT Reading training data from delimited file at %s", training_path)
  training_data <- read.csv(training_path, row.names = 1)
  tryCatch({
    outcome_path <- normalizePath(outcome_path)
    flog.pid.info("INPUT Reading training outcomes from delimited file at %s", outcome_path)
    outcome <- read.csv(outcome_path)
  }, error = function(e) {
    flog.pid.debug("Could not open file %s", outcome_path)
    flog.pid.debug(e)
  })

  outcome <- factor(outcome[,2], levels = c(POSITIVE_LABEL, NEGATIVE_LABEL))
  flog.pid.info("PROGRESS Outcome levels: %s", paste0(levels(outcome), collapse = ", "))

  ### Parameter-dependent preprocessing ###
  ## Downsampling majority class
  futile.logger::flog.info("PARAMETER %s = %s", DOWNSAMPLING, parameter_list[[DOWNSAMPLING]])
  if (parameter_list[[DOWNSAMPLING]] == DOWNSAMPLING_ON) {
    futile.logger::flog.info("PARAMETER Performing downsampling of majority class")
    majority_class <- table(outcome)
    majority_class <- majority_class[which.max(majority_class)]
    minority_class <- table(outcome)
    minority_class <- minority_class[which.min(minority_class)]

    futile.logger::flog.info(paste0("PARAMETER Majority class is \"", names(majority_class), "\""))

    # Drop n majority class instances, where n is the number
    # by which majority class size exceeds minority class size
    drop_idx <- sample(which(outcome == names(majority_class)), majority_class - minority_class, replace = FALSE)
    futile.logger::flog.info(paste0("PARAMETER Dropping ", length(drop_idx), " instances of the majority class"))
    outcome <- outcome[-drop_idx]
    training_data <- training_data[-drop_idx,, drop = FALSE]
    futile.logger::flog.info(paste0("PARAMETER ", table(outcome) %>% capture.output))

  } else if (parameter_list[[DOWNSAMPLING]] == DOWNSAMPLING_OFF) {
    # Do nothing
  } else stop(
    paste0("Unknown value \"", parameter_list[[DOWNSAMPLING]], "\" for parameter \"", DOWNSAMPLING, "\"")
  )

  # Feature sampling
  flog.pid.info("PARAMETER %s = %s", FEATURE_SAMPLING_PERCENTAGE, parameter_list[[FEATURE_SAMPLING_PERCENTAGE]])
  feature_sampling_pct <- parameter_list[[FEATURE_SAMPLING_PERCENTAGE]]
  if (feature_sampling_pct != 1.0) {
    if (!(feature_sampling_pct %in% FEATURE_SAMPLING_PERCENTAGE_ALLOWED_VALUES)) {
      stop(
        paste0("Feature sampling percentage not in predefined allowed values")
      )
    }

    flog.pid.info("PARAMETER Performing feature sampling, keeping %f percent of features", feature_sampling_pct*100)
    all_features <- c(numeric_features, categorical_features)
    sampled_features <- sample(
      x = all_features,
      size = length(all_features) * feature_sampling_pct,
      replace = FALSE
    )

    sampled_cats_ix <- sampled_features %in% categorical_features
    sampled_nums_ix <- !(sampled_features %in% categorical_features)
    if (parameter_list[[CATEGORICAL_ENCODING]] == CATEGORICAL_AS_DUMMY && any(sampled_cats_ix)) {
      sampled_cats <- sampled_features[sampled_cats_ix]
      dummies <- lapply(sampled_cats, . %>% find_dummies(colnames(training_data)))
      dummies <- do.call(c, dummies)
      sampled_cols <- c(sampled_features[sampled_nums_ix], dummies)
    } else {
      sampled_cols <- sampled_features
    }
    flog.pid.info("PARAMETER Feature sampling drops the following features: %s", paste0(setdiff(c(numeric_features, categorical_features), sampled_features), collapse = ", "))
    training_data <- training_data[, sampled_cols, drop = FALSE]

  } else if (feature_sampling_pct == 1.0) {
    # Do nothing
  }

  # Training data sampling
  flog.pid.info("PARAMETER %s = %s", TRAINING_DATA_SAMPLING_PERCENTAGE, parameter_list[[TRAINING_DATA_SAMPLING_PERCENTAGE]])
  training_data_sampling_pct <- parameter_list[[TRAINING_DATA_SAMPLING_PERCENTAGE]]
  if (training_data_sampling_pct != 1.0) {
    if (!(training_data_sampling_pct %in% TRAINING_DATA_SAMPLING_PERCENTAGE_ALLOWED_VALUES)) {
      stop(
        paste0("Training data sampling percentage not in predefined allowed values")
      )
    }

    flog.pid.info("PARAMETER Performing training data sampling, keeping %f percentage of rows", training_data_sampling_pct*100)
    training_data_sampling_ix <- sample(
      x = 1:(NROW(training_data)),
      size = NROW(training_data) * training_data_sampling_pct,
      replace = FALSE
    )
    flog.pid.info("PARAMETER Training data sampling drops %d rows", NROW(training_data) - length(training_data_sampling_ix))
    training_data <- training_data[training_data_sampling_ix, , drop = FALSE]
    outcome <- outcome[training_data_sampling_ix]

  } else if (training_data_sampling_pct == 1.0) {
    # Do nothing
  }

  # Removal of features with near-zero variance
  flog.pid.info("PARAMETER %s = %s", NZV_CHECK, parameter_list[[NZV_CHECK]])
  if (parameter_list[[NZV_CHECK]] == NZV_CHECK_ON) {

    flog.pid.info("PARAMETER Removing features with near-zero variance")
    flog.pid.info("PARAMETER Uniqueness cutoff: %d %%", UNIQUENESS_CUTOFF_PERCENTAGE)
    nzv_features <- caret::nearZeroVar(training_data, saveMetrics = TRUE, uniqueCut = UNIQUENESS_CUTOFF_PERCENTAGE)
    flog.pid.info(paste0("PARAMETER ", capture.output(print(nzv_features[nzv_features$nzv, ]))))

    if (any(nzv_features$nzv)) {
      training_data <- training_data[, !nzv_features$nzv]
    }
  } else if (parameter_list[[NZV_CHECK]] == NZV_CHECK_OFF) {
    # Do nothing
  } else stop(
    paste0("Unknown value \"", parameter_list[[NZV_CHECK]], "\" for parameter \"", NZV_CHECK, "\"")
  )

  # Removal of highly correlated features
  flog.pid.info("PARAMETER %s = %s", CORRELATION_CHECK, parameter_list[[CORRELATION_CHECK]])
  if (parameter_list[[CORRELATION_CHECK]] == CORRELATION_CHECK_ON) {

    flog.pid.info("PARAMETER Removing highly correlated features:")
    correlations <- cor(training_data[, intersect(colnames(training_data), numeric_features)], use = "pairwise.complete.obs")
    correlations[is.na(correlations)] <- 0.0

    highly_correlated_features <- caret::findCorrelation(correlations, verbose = TRUE, names = TRUE)
    flog.pid.info(paste0("PARAMETER ", highly_correlated_features))

    if (highly_correlated_features %>% length > 0) {
      training_data <- training_data[, !colnames(training_data) %in% highly_correlated_features]
    }
  } else if (parameter_list[[CORRELATION_CHECK]] == CORRELATION_CHECK_OFF) {
    # Do nothing
  } else stop(
    paste0("Unknown value \"", parameter_list[[CORRELATION_CHECK]], "\" for parameter \"", CORRELATION_CHECK, "\"")
  )

  # Save final list of features
  final_features_path <- file.path(output_path, FILE_FINAL_FEATURES_RDS)
  futile.logger::flog.info("OUTPUT Writing final feature set to RDS file at %s", final_features_path)
  saveRDS(colnames(training_data), final_features_path)

  ### Imputation ###
  flog.pid.info("PROGRESS Imputation hyperparameter configuration counts:")

  # Log the number of hyperparameter configurations for each imputation method:
  if (length(mice_hyperparameter_grids) > 0) {
    flog.pid.info(paste0("PROGRESS", names(mice_hyperparameter_grids), ": ",  lapply(mice_hyperparameter_grids, nrow)))
  }
  if (length(other_hyperparameter_grids) > 0) {
    flog.pid.info(paste0("PROGRESS", names(other_hyperparameter_grids), ": ",  lapply(other_hyperparameter_grids, nrow)))
  }

  # MICE imputations
  flog.pid.info("PROGRESS Starting MICE imputation")
  if (!lean) {
    times <- IMPUTE_TIMES
  } else {
    times <- SIMULATION_IMPUTE_TIMES
  }
  flog.pid.info("DESIGN_CHOICE Imputing %d times, with max. %d iterations", times, MICE_ITERATIONS)
  mice_imputations <- group_impute(training_data, mice_hyperparameter_grids, seed, times = times, iterations = MICE_ITERATIONS)

  # Non-MICE imputations
  flog.pid.info("PROGRESS Starting non-MICE imputation")
  other_imputations <- group_impute(training_data, other_hyperparameter_grids, seed, times = times)

  # Single value imputations
  flog.pid.info("PROGRESS Starting single value imputation")
  single_value_imputations <- lapply(enumerate(single_value_imputation_hyperparameter_grids), function(method) {
    imputations <- list(`imp_hp_1` = list(completed_datasets = list(get(method$name)(training_data))))
    imputations <- lapply(imputations, function(hp_set) {
      timings <- hp_set %>% magrittr::extract2("completed_datasets") %>% magrittr::extract2(1) %>% attr(TIMING_ATTR)
      attr(hp_set, TIMING_ATTR) <- timings
      return(hp_set)
    })
    return(imputations)
  }) %>% magrittr::set_names(names(single_value_imputation_hyperparameter_grids))

  # List and drop imputation methods that failed completely
  imputations <- c(mice_imputations, other_imputations, single_value_imputations)
  flog.pid.info("PROGRESS Checking and dropping failed imputation methods")
  valid_methods <- check_method_results(imputations)
  if (all(valid_methods == FALSE)) {
    stop("No imputation method succeeded, cannot continue")
  }
  imputations <- imputations[valid_methods]

  ### Training classifier ###
  flog.pid.info("PROGRESS Starting classifier training")
  flog.pid.info("DESIGN_CHOICE Hyperparameter grid for RF:")
  flog.pid.info(paste0("DESIGN_CHOICE ", capture.output(print(RF_HYPERPARAMETER_GRID))))
  flog.pid.info("DESIGN_CHOICE Hyperparameter grid for XGBoost:")
  flog.pid.info(paste0("DESIGN_CHOICE ", capture.output(print(XGBOOST_HYPERPARAMETER_GRID))))

  flog.pid.info("PARAMETER %s = %s", HYPERPARAMETER_SEARCH_TYPE, parameter_list[[HYPERPARAMETER_SEARCH_TYPE]])
  if (parameter_list[[HYPERPARAMETER_SEARCH_TYPE]] == HYPERPARAMETER_SEARCH_TYPE_GRID) {
    flog.pid.info("PARAMETER Using grid search for hyperparameter optimization")
    search <- "grid"
  } else if (parameter_list[[HYPERPARAMETER_SEARCH_TYPE]] == HYPERPARAMETER_SEARCH_TYPE_RANDOM) {
    flog.pid.info("PARAMETER Using random search for hyperparameter optimization")
    search <- "random"
  } else stop(
    paste0("Unknown value \"", parameter_list[[HYPERPARAMETER_SEARCH_TYPE]], "\" for parameter \"", HYPERPARAMETER_SEARCH_TYPE, "\"")
  )

  all_hyperparameter_grids <- c(mice_hyperparameter_grids, other_hyperparameter_grids, single_value_imputation_hyperparameter_grids)
  write_models <- function(best_models, models_path, hps_path, imputers_path=NULL) {

    # glm models in R contain references to environments, but for prediction it doesn't seem that
    # the environment needs to be the exact one defined during training. Using a dummy `refhook`-argument
    # we can bypass saving the environments and save *a lot* of space (~ 50 Mb per model -> 7 Mb per model).
    # See https://stackoverflow.com/questions/54144239/how-to-use-saverds-refhook-parameter for an example of
    # using the `refhook`.

    futile.logger::flog.info("OUTPUT Writing chosen models to RDS file at %s", models_path)
    saveRDS(best_models$models, file = models_path, refhook = function(x) "")
    futile.logger::flog.info("OUTPUT Writing chosen hyperparameters to RDS file at %s", hps_path)
    saveRDS(best_models$hyperparams, file = hps_path)
    futile.logger::flog.info("OUTPUT Writing chosen imputer models to RDS file at %s", imputers_path)
    if (!is.null(imputers_path)) saveRDS(best_models$imputers, file = imputers_path)

  }
  train_models <- function(training_options, tr_function, classifier_name, imputations, outcome, classifier_grid, imputation_grids, tune_length) {
    flog.pid.info("DESIGN_CHOICE Using following options controlling training: ")
    flog.pid.info(paste0("DESIGN_CHOICE ", names(training_options), " = ", training_options))
    training_settings <- do.call(caret::trainControl, training_options)
    flog.pid.info("PROGRESS Starting classifier training: " %>% paste0(classifier_name))
    # Train on every completed dataset
    models <- loop_models(training_function = tr_function,
                             classifier_name = classifier_name,
                             imputations = imputations,
                             outcome = outcome,
                             control = training_settings,
                             grid = classifier_grid,
                             tunelength = tune_length,
                             seed = seed)
    bests <- select_best(models, imputations, imputation_grids)

    return(bests)

  }
  if("rf" %in% parameter_list[[CLASSIFIER_METHOD]]) {

    rf_training_options <- list(
      classProbs = TRUE,
      verboseIter = FALSE,
      method = "oob", # Use out-of-bag error estimate for model selection
      returnResamp = "final",
      allowParallel = FALSE,
      search = search
    )

    rf_bests <- train_models(
      rf_training_options,
      train_rf,
      "RF",
      imputations,
      outcome,
      if (search == "grid") RF_HYPERPARAMETER_GRID else NULL,
      all_hyperparameter_grids,
      nrow(RF_HYPERPARAMETER_GRID)
    )
       
    # Save run time information for imputers
    rf_runtimes_path <- file.path(output_path, FILE_RF_RUNTIMES_CSV)
    futile.logger::flog.info("OUTPUT Writing RF-linked imputation runtime measurements to delimited file at %s", rf_runtimes_path)
    write.csv(x = form_run_time_df(rf_bests$imputers, times_imputed = times), file = rf_runtimes_path)

    # Saving model
    write_models(
      rf_bests, 
      file.path(output_path, FILE_RF_CLASSIFIERS_RDS),
      file.path(output_path, FILE_RF_HP_CONFIGS_RDS),
      imputers_path = if (!lean) file.path(output_path, FILE_RF_IMPUTERS_RDS) else NULL
    )
  }

  flog.pid.info("PARAMETER %s = %s", CATEGORICAL_ENCODING, parameter_list[[CATEGORICAL_ENCODING]])

  # Skip LR and XGBoost if not using dummy features
  if (parameter_list[[CATEGORICAL_ENCODING]] == CATEGORICAL_AS_DUMMY) {

    if("xgboost" %in% parameter_list[[CLASSIFIER_METHOD]]) {

      xg_training_options <- list(
        classProbs = TRUE,
        verboseIter = FALSE,
        method = "cv",
        number = 10,
        allowParallel = FALSE,
        search = search
      )

      xg_bests <- train_models(
        xg_training_options,
        train_xgboost,
        "XGBoost",
        imputations,
        outcome,
        if (search == "grid") XGBOOST_HYPERPARAMETER_GRID else NULL,
        all_hyperparameter_grids,
        nrow(XGBOOST_HYPERPARAMETER_GRID) 
      )
         
      # Save run time information for imputers
      xg_runtimes_path <- file.path(output_path, FILE_XGBOOST_RUNTIMES_CSV)
      futile.logger::flog.info("OUTPUT Writing XGBoost-linked imputation runtime measurements to delimited file at %s", xg_runtimes_path)
      write.csv(x = form_run_time_df(xg_bests$imputers, times_imputed = times), file = xg_runtimes_path)

      # Saving model
      write_models(
        xg_bests, 
        file.path(output_path, FILE_XGBOOST_CLASSIFIERS_RDS),
        file.path(output_path, FILE_XGBOOST_HP_CONFIGS_RDS),
        imputers_path = if (!lean) file.path(output_path, FILE_XGBOOST_IMPUTERS_RDS) else NULL
      )
    }

    flog.pid.info("PARAMETER Categorical features are encoded as dummy variables, so XGBoost and LR training is possible")
    if ("lr" %in% parameter_list[[CLASSIFIER_METHOD]]) {
      lr_training_options <- list(
        classProbs = TRUE,
        verboseIter = FALSE,
        allowParallel = FALSE,
        search = search
      )

      lr_bests <- train_models(
        lr_training_options,
        train_lr,
        "LR",
        imputations,
        outcome,
        if (search == "grid") data.frame() else NULL,
        all_hyperparameter_grids,
        NULL
      )
         
      # Save run time information for imputers
      lr_runtimes_path <- file.path(output_path, FILE_LR_RUNTIMES_CSV)
      futile.logger::flog.info("OUTPUT Writing LR-linked imputation runtime measurements to delimited file at %s", lr_runtimes_path)
      write.csv(x = form_run_time_df(lr_bests$imputers, times_imputed = times), file = lr_runtimes_path)

      # Saving model
      write_models(
        lr_bests, 
        file.path(output_path, FILE_LR_CLASSIFIERS_RDS),
        file.path(output_path, FILE_LR_HP_CONFIGS_RDS),
        imputers_path = if (!lean) file.path(output_path, FILE_LR_IMPUTERS_RDS) else NULL
      )
    }
  } else if (parameter_list[[CATEGORICAL_ENCODING]] == CATEGORICAL_AS_FACTOR) {
    # XGBoost does not work with factors, and LR cannot deal with new factor
    # levels in test data (which occurs easily in CV), so we have to skip them.
    flog.pid.info("PARAMETER Categorical features are encoded as factors, so XGBoost and LR are skipped")
  }  else stop(
    paste0("Unknown value \"", parameter_list[[CATEGORICAL_ENCODING]], "\" for parameter \"", CATEGORICAL_ENCODING, "\"")
  )

  flog.pid.info("PROGRESS Finishing impute_and_train")
}
