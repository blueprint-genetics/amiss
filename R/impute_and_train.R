#' @importFrom magrittr %>%
#' @importFrom foreach %do%
impute_and_train <- function(training_path,
                             outcome_path,
                             output_path,
                             mice_hyperparameter_grids,
                             other_hyperparameter_grids,
                             single_value_imputation_hyperparameter_grids,
                             parameter_list,
                             cores, seed = 42, lean) {

  flog.pid.info("impute_and_train.R")
  flog.pid.info("PROGRESS Arguments: %s", paste0(list(training_path, outcome_path, output_path, cores, lean), collapse = ", "))

  output_path <- normalizePath(output_path, mustWork = FALSE)
  create_dir(output_path)
  flog.pid.info("OUTPUT Output root folder set to %s", output_path)
  
  if (parameter_list[[FEATURE_SAMPLING]] %>% is.null) {
    stop("Required parameter \"" %>% paste0(FEATURE_SAMPLING, "\" not provided"))
  }
  if (parameter_list[[FEATURE_SAMPLING_PERCENTAGE]] %>% is.null) {
    stop("Required parameter \"" %>% paste0(FEATURE_SAMPLING_PERCENTAGE, "\" not provided"))
  }
  if (parameter_list[[TRAINING_DATA_SAMPLING]] %>% is.null) {
    stop("Required parameter \"" %>% paste0(TRAINING_DATA_SAMPLING, "\" not provided"))
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
  
  if (lean) {
    flog.pid.info("DESIGN_CHOICE Sampling imputation hyperparameter grids to %d rows to save computation time", SIMULATION_HP_SAMPLE_SIZE)
    mice_hyperparameter_grids <- lapply(mice_hyperparameter_grids, . %>% sample_max(size = SIMULATION_HP_SAMPLE_SIZE))
    other_hyperparameter_grids <- lapply(other_hyperparameter_grids, . %>% sample_max(size = SIMULATION_HP_SAMPLE_SIZE))
  }

  flog.pid.info("PROGRESS Using %d cores", cores)

  if (!is.null(seed)) {
    flog.pid.info("DESIGN_CHOICE Using seed: %d", seed)
    set.seed(seed)
  }
  
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
  
  # Feature sampling
  flog.pid.info("PARAMETER %s = %s", FEATURE_SAMPLING, parameter_list[[FEATURE_SAMPLING]])
  flog.pid.info("PARAMETER %s = %s", FEATURE_SAMPLING_PERCENTAGE, parameter_list[[FEATURE_SAMPLING_PERCENTAGE]])
  feature_sampling_pct <- parameter_list[[FEATURE_SAMPLING_PERCENTAGE]]
  if (parameter_list[[FEATURE_SAMPLING]] == FEATURE_SAMPLING_ON) {
    if (!(feature_sampling_pct %in% FEATURE_SAMPLING_PERCENTAGE_ALLOWED_VALUES)) {
      stop(
        paste0("Training data sampling percentage not in predefined allowed values")
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
    
  } else  if (parameter_list[[FEATURE_SAMPLING]] == FEATURE_SAMPLING_OFF) {
    # Do nothing
  } else stop(
    paste0("Unknown value \"", parameter_list[[FEATURE_SAMPLING]], "\" for parameter \"", FEATURE_SAMPLING, "\"")
  )
  
  # Training data sampling
  flog.pid.info("PARAMETER %s = %s", TRAINING_DATA_SAMPLING, parameter_list[[TRAINING_DATA_SAMPLING]])
  flog.pid.info("PARAMETER %s = %s", TRAINING_DATA_SAMPLING_PERCENTAGE, parameter_list[[TRAINING_DATA_SAMPLING_PERCENTAGE]])
  training_data_sampling_pct <- parameter_list[[TRAINING_DATA_SAMPLING_PERCENTAGE]]
  if (parameter_list[[TRAINING_DATA_SAMPLING]] == TRAINING_DATA_SAMPLING_ON) {
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
    
  } else  if (parameter_list[[TRAINING_DATA_SAMPLING]] == TRAINING_DATA_SAMPLING_OFF) {
    # Do nothing
  } else stop(
    paste0("Unknown value \"", parameter_list[[TRAINING_DATA_SAMPLING]], "\" for parameter \"", TRAINING_DATA_SAMPLING, "\"")
  )

  ## Removal of problematic features

  # Some imputation methods cannot deal with features that have certain unwanted properties, and thus they must be removed prior to imputation.

  flog.pid.info("PARAMETER %s = %s", NZV_CHECK, parameter_list[[NZV_CHECK]])
  if (parameter_list[[NZV_CHECK]] == NZV_CHECK_ON) {
    ### Near-zero variance
    
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
  
  flog.pid.info("PARAMETER %s = %s", CORRELATION_CHECK, parameter_list[[CORRELATION_CHECK]])
  if (parameter_list[[CORRELATION_CHECK]] == CORRELATION_CHECK_ON) {
    ### Highly correlated features
    
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

  ## Imputation

  flog.pid.info("PROGRESS Imputation hyperparameter configuration counts:")
  # Check the number of hyperparameter configurations for each imputation method:
  flog.pid.info(paste0("PROGRESS", names(mice_hyperparameter_grids), ": ",  lapply(mice_hyperparameter_grids, nrow)))
  flog.pid.info(paste0("PROGRESS", names(other_hyperparameter_grids), ": ",  lapply(other_hyperparameter_grids, nrow)))

  ### MICE
  flog.pid.info("PROGRESS Starting MICE imputation")
  if (!lean) {
    times <- IMPUTE_TIMES
  } else {
    times <- SIMULATION_IMPUTE_TIMES
  }
  flog.pid.info("DESIGN_CHOICE Imputing %d times, with max. %d iterations", times, MICE_ITERATIONS)
  mice_imputations <- impute_over_grid(training_data, mice_hyperparameter_grids, seed, times = times, iterations = MICE_ITERATIONS)

  ### Non-MICE imputations
  flog.pid.info("PROGRESS Starting non-MICE imputation")
  other_imputations <- impute_over_grid(training_data, other_hyperparameter_grids, seed, times = times)

  ### Single value imputations
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

  ### List and drop imputation methods that failed completely
  imputations <- c(mice_imputations, other_imputations, single_value_imputations)
  flog.pid.info("PROGRESS Checking and dropping failed imputation methods")
  valid_methods <- check_method_results(imputations)
  if (all(valid_methods == FALSE)) {
    stop("No imputation method succeeded, cannot continue")
  }
  imputations <- imputations[valid_methods]

  ## Training classifier
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
  
  rf_training_options <- list(
    classProbs = TRUE,
    verboseIter = FALSE,
    method = "oob", # Use out-of-bag error estimate for model selection
    returnResamp = "final",
    allowParallel = FALSE,
    search = search
  )
  flog.pid.info("DESIGN_CHOICE Using following options controlling RF training: ")
  flog.pid.info(paste0("DESIGN_CHOICE ", names(rf_training_options), " = ", rf_training_options))
  rf_training_settings <- do.call(caret::trainControl, rf_training_options)
  xg_training_options <- list(
    classProbs = TRUE,
    verboseIter = FALSE,
    method = "cv",
    number = 10,
    allowParallel = FALSE,
    search = search
  )
  flog.pid.info("DESIGN_CHOICE Using following options controlling XGBoost training: ")
  flog.pid.info(paste0("DESIGN_CHOICE ", names(xg_training_options), " = ", xg_training_options))
  xg_training_settings <- do.call(caret::trainControl, xg_training_options)
  lr_training_options <- list(
    classProbs = TRUE,
    verboseIter = FALSE,
    allowParallel = FALSE,
    search = search
  )
  flog.pid.info("DESIGN_CHOICE Using following options controlling LR training: ")
  flog.pid.info(paste0("DESIGN_CHOICE ", names(lr_training_options), " = ", lr_training_options))
  lr_training_settings <- do.call(caret::trainControl, lr_training_options)

  flog.pid.info("PROGRESS Starting classifier training")
  # Train on every completed dataset
  flog.pid.info("PROGRESS Starting RF training")
  rf_models <- loop_models(training_function = train_rf,
                           classifier_name = "RF",
                           imputations = imputations,
                           outcome = outcome,
                           control = rf_training_settings,
                           grid = if(search == "grid") RF_HYPERPARAMETER_GRID else NULL,
                           tunelength = nrow(RF_HYPERPARAMETER_GRID),
                           seed = seed)
  flog.pid.info("PARAMETER %s = %s", CATEGORICAL_ENCODING, parameter_list[[CATEGORICAL_ENCODING]])
  if (parameter_list[[CATEGORICAL_ENCODING]] == CATEGORICAL_AS_DUMMY) {
    flog.pid.info("PARAMETER Categorical features are encoded as dummy variables, so XGBoost and LR training is possible")
    flog.pid.info("PROGRESS Starting XGBoost training")
    xg_models <- loop_models(training_function = train_xgboost,
                             classifier_name = "XGBoost",
                             imputations = imputations,
                             outcome = outcome,
                             control = xg_training_settings,
                             grid = if(search == "grid") XGBOOST_HYPERPARAMETER_GRID else NULL,
                             tunelength = nrow(XGBOOST_HYPERPARAMETER_GRID),
                             seed = seed)
    flog.pid.info("PROGRESS Starting LR training")
    lr_models <- loop_models(training_function = train_lr,
                             classifier_name = "LR",
                             imputations = imputations,
                             outcome = outcome,
                             control = lr_training_settings,
                             grid = if(search == "grid") data.frame() else NULL,
                             tunelength = NULL,
                             seed = seed)
  } else if (parameter_list[[CATEGORICAL_ENCODING]] == CATEGORICAL_AS_FACTOR) {
    # XGBoost does not work with factors, and LR cannot deal with new factor
    # levels in test data (which occurs easily in CV), so we have to skip them.
    flog.pid.info("PARAMETER Categorical features are encoded as factors, so XGBoost and LR are skipped")
    xg_models <- list(list(imp_hp_1 = list(NULL))) %>% set_names(parameter_list[[IMPUTATION_METHOD]])
    lr_models <- list(list(imp_hp_1 = list(NULL))) %>% set_names(parameter_list[[IMPUTATION_METHOD]])
  }  else stop(
    paste0("Unknown value \"", parameter_list[[CATEGORICAL_ENCODING]], "\" for parameter \"", CATEGORICAL_ENCODING, "\"")
  )

  ## Model selection
  flog.pid.info("PROGRESS Starting model selection")
  all_hyperparameter_grids <- c(mice_hyperparameter_grids, other_hyperparameter_grids, single_value_imputation_hyperparameter_grids)
  rf_bests <- select_best(rf_models, imputations, all_hyperparameter_grids)
  xg_bests <- select_best(xg_models, imputations, all_hyperparameter_grids)
  lr_bests <- select_best(lr_models, imputations, all_hyperparameter_grids)

  flog.pid.info("PROGRESS Saving data")
  # Save run time information for imputers
  rf_runtimes_path <- file.path(output_path, FILE_RF_RUNTIMES_CSV)
  futile.logger::flog.info("OUTPUT Writing RF-linked imputation runtime measurements to delimited file at %s", rf_runtimes_path)
  write.csv(x = form_run_time_df(rf_bests$imputers, times_imputed = times), file = rf_runtimes_path)
  xg_runtimes_path <- file.path(output_path, FILE_XGBOOST_RUNTIMES_CSV)
  futile.logger::flog.info("OUTPUT Writing XGBoost-linked imputation runtime measurements to delimited file at %s", xg_runtimes_path)
  write.csv(x = form_run_time_df(xg_bests$imputers, times_imputed = times), file = xg_runtimes_path)
  lr_runtimes_path <- file.path(output_path, FILE_LR_RUNTIMES_CSV)
  futile.logger::flog.info("OUTPUT Writing LR-linked imputation runtime measurements to delimited file at %s", lr_runtimes_path)
  write.csv(x = form_run_time_df(lr_bests$imputers, times_imputed = times), file = lr_runtimes_path)

  ## Saving model
  rf_models_path <- file.path(output_path, FILE_RF_CLASSIFIERS_RDS)
  futile.logger::flog.info("OUTPUT Writing chosen RF models to RDS file at %s", rf_models_path)
  saveRDS(rf_bests$models, file = rf_models_path)
  rf_imputers_path <- file.path(output_path, FILE_RF_IMPUTERS_RDS)
  futile.logger::flog.info("OUTPUT Writing chosen RF imputer models to RDS file at %s", rf_imputers_path)
  if (!lean) saveRDS(rf_bests$imputers, file = rf_imputers_path)
  rf_hps_path <- file.path(output_path, FILE_RF_HP_CONFIGS_RDS)
  futile.logger::flog.info("OUTPUT Writing chosen RF hyperparameters to RDS file at %s", rf_hps_path)
  saveRDS(rf_bests$hyperparams, file = rf_hps_path)

  xg_models_path <- file.path(output_path, FILE_XGBOOST_CLASSIFIERS_RDS)
  futile.logger::flog.info("OUTPUT Writing chosen XGBoost models to RDS file at %s", xg_models_path)
  saveRDS(xg_bests$models, file = xg_models_path)
  xg_imputers_path <- file.path(output_path, FILE_XGBOOST_IMPUTERS_RDS)
  futile.logger::flog.info("OUTPUT Writing chosen XGBoost imputer models to RDS file at %s", xg_imputers_path)
  if (!lean) saveRDS(xg_bests$imputers, file = xg_imputers_path)
  xg_hps_path <- file.path(output_path, FILE_XGBOOST_HP_CONFIGS_RDS)
  futile.logger::flog.info("OUTPUT Writing chosen XGBoost hyperparameters to RDS file at %s", xg_hps_path)
  saveRDS(xg_bests$hyperparams, file = xg_hps_path)
  
  # glm models in R contain references to environments, but for prediction it doesn't seem that
  # the environment needs to be the exact one defined during training. Using a dummy `refhook`-argument
  # we can bypass saving the environments and save *a lot* of space (~ 50 Mb per model -> 7 Mb per model).
  # See https://stackoverflow.com/questions/54144239/how-to-use-saverds-refhook-parameter for an example of
  # using the `refhook`.
  lr_models_path <- file.path(output_path, FILE_LR_CLASSIFIERS_RDS)
  futile.logger::flog.info("OUTPUT Writing chosen LR models to RDS file at %s", lr_models_path)
  saveRDS(lr_bests$models, file = lr_models_path, refhook = function(x) "")
  lr_imputers_path <- file.path(output_path, FILE_LR_IMPUTERS_RDS)
  futile.logger::flog.info("OUTPUT Writing chosen LR imputer models to RDS file at %s", lr_imputers_path)
  if (!lean) saveRDS(lr_bests$imputers, file = lr_imputers_path)
  lr_hps_path <- file.path(output_path, FILE_LR_HP_CONFIGS_RDS)
  futile.logger::flog.info("OUTPUT Writing chosen LR hyperparameters to RDS file at %s", lr_hps_path)
  saveRDS(lr_bests$hyperparams, file = lr_hps_path)

  final_features_path <- file.path(output_path, FILE_FINAL_FEATURES_RDS)
  futile.logger::flog.info("OUTPUT Writing final feature set to RDS file at %s", final_features_path)
  saveRDS(colnames(training_data), final_features_path)
  
  flog.pid.info("PROGRESS Done saving data")
  flog.pid.info("PROGRESS Finishing impute_and_train")
}
