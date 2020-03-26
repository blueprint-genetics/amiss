
library(purrr)
library(magrittr)
library(futile.logger)
library(caret)
library(ModelMetrics)
library(foreach)
library(doParallel)
library(doRNG)

impute_and_train <- function(training_path, outcome_path, output_path, cores, seed = 42, lean) {

  source("R/utils.R")
  source("R/imputation_definitions.R")
  source("R/training_functions.R")

  if (!dir.exists(output_path)) {
    dir_creation_success <- dir.create(output_path, showWarnings = TRUE)
    if (!dir_creation_success) {
      stop("Failed to create directory for saving output.")
    }
  }

  flog.appender(appender.tee(file.path(output_path, "04_impute_and_train.log")))

  flog.threshold(DEBUG)

  if (lean) {
    mice_hyperparameter_grids <- lapply(mice_hyperparameter_grids, . %>% sample_max(size = 8L))
    other_hyperparameter_grids <- lapply(other_hyperparameter_grids, . %>% sample_max(size = 8L))
    other_hyperparameter_grids$missForest <- NULL # missForest takes so long that it is not worth running in simulations
  }

  source("R/recursive_application.R")
  source("R/imputation.R")

  flog.pid.info("04_impute_and_train.log")
  flog.pid.info("Arguments: %s", paste0(list(training_path, outcome_path, output_path, cores, lean), collapse = ", "))

  flog.pid.info("Using %d cores", cores)
  registerDoParallel(cores)

  if(!is.null(seed)) {
    flog.pid.info("Using seed: %d", seed)
    set.seed(seed)
  }

  flog.pid.info("Reading data")
  training_data <- read.csv(training_path, row.names = 1, as.is = TRUE)
  tryCatch({
    outcome <- read.csv(outcome_path, as.is = TRUE)
  }, error = function(e) {
    flog.pid.debug("Could not open file %s", outcome_path)
    flog.pid.debug(e)
  })
  outcome <- factor(outcome[,2], levels = c("positive", "negative"))
  flog.pid.info("Outcome levels: %s", paste0(levels(outcome), collapse = ", "))

  ## Removal of problematic features

  # Some imputation methods cannot deal with features that have certain unwanted properties, and thus they must be removed prior to imputation.

  ### Near-zero variance

  flog.pid.info("Removing features with near-zero variance")
  cut_p <- 1
  flog.pid.info("Uniqueness cutoff: %d %%", cut_p)
  nzv_features <- caret::nearZeroVar(training_data, saveMetrics = TRUE, uniqueCut = cut_p)
  flog.pid.info(capture.output(print(nzv_features[nzv_features$nzv, ])))

  if (any(nzv_features$nzv)) {
    training_data <- training_data[, !nzv_features$nzv]
  }

  ### Highly correlated features

  flog.pid.info("Removing highly correlated features:")
  correlations <- cor(training_data, use = "pairwise.complete.obs")
  correlations[is.na(correlations)] <- 0.0

  highly_correlated_features <- caret::findCorrelation(correlations, verbose = TRUE, names = TRUE)
  flog.pid.info(highly_correlated_features)

  if (highly_correlated_features %>% length > 0) {
    training_data <- training_data[, !colnames(training_data) %in% highly_correlated_features]
  }

  ## Imputation

  flog.pid.info("Imputation hyperparameter configuration counts:")
  # Check the number of hyperparameter configurations for each imputation method:
  flog.pid.info(paste0(names(mice_hyperparameter_grids), ": ",  lapply(mice_hyperparameter_grids, nrow)))
  flog.pid.info(paste0(names(other_hyperparameter_grids), ": ",  lapply(other_hyperparameter_grids, nrow)))

  ### MICE
  flog.pid.info("Starting MICE imputation")
  if(!lean) {
    times <- 10
  } else {
    times <- 1
  }
  iters <- 10
  flog.pid.info("Imputing %d times, with max. %d iterations", times, iters)
  mice_imputations <- impute_over_grid(training_data, mice_hyperparameter_grids, seed, times = times, iterations = iters)

  ### Non-MICE imputations
  flog.pid.info("Starting non-MICE imputation")
  other_imputations <- impute_over_grid(training_data, other_hyperparameter_grids, seed, times = times)

  ### Single value imputations
  flog.pid.info("Starting single value imputation")
  single_value_imputations <- lapply(enumerate(single_value_imputation_hyperparameter_grids), function(method) {
    list(`imp_hp_1` = list(completed_datasets = list(get(method$name)(training_data))))
  }) %>% set_names(names(single_value_imputation_hyperparameter_grids))

  ### List and drop imputation methods that failed completely
  imputations <- c(mice_imputations, other_imputations, single_value_imputations)
  flog.pid.info("Checking and dropping failed imputation methods")
  valid_methods <- check_method_results(imputations)
  imputations <- imputations[valid_methods]

  ## Training classifier
  flog.pid.info("Starting classifier training")
  hyperparameter_grid <- data.frame(mtry = 1:5 * 8 - 1)
  flog.pid.info("Hyperparameter grid:")
  flog.pid.info(capture.output(print(hyperparameter_grid)))

  rf_training_settings <- trainControl(classProbs = TRUE,
                                       verboseIter = FALSE,
                                       method = "oob",  # Use out-of-bag error estimate for model selection
                                       returnResamp = "final",
                                       allowParallel = FALSE) # Don't use parallelization inside training loop; it will be done on a higher level
  lr_training_settings <- trainControl(classProbs = TRUE,
                                       verboseIter = FALSE,
                                       allowParallel = FALSE) # Don't use parallelization inside training loop; it will be done on a higher level

  # Train on every completed dataset
  rf_models <- loop_models(training_function = train_rf,
                           classifier_name = "RF",
                           imputations = imputations,
                           outcome = outcome,
                           control = rf_training_settings,
                           grid = hyperparameter_grid,
                           seed = seed)
  lr_models <- loop_models(training_function = train_lr,
                           classifier_name = "LR",
                           imputations = imputations,
                           outcome = outcome,
                           control = lr_training_settings,
                           grid = data.frame(),
                           seed = seed)

  ## Model selection
  flog.pid.info("Starting model selection")
  all_hyperparameter_grids <- c(mice_hyperparameter_grids, other_hyperparameter_grids, single_value_imputation_hyperparameter_grids)
  rf_bests <- select_best(rf_models, imputations, all_hyperparameter_grids)
  lr_bests <- select_best(lr_models, imputations, all_hyperparameter_grids)

  flog.pid.info("Saving data")
  # Save run time information for imputers
  write.csv(x = form_run_time_df(rf_bests$imputers), file = file.path(output_path, "rf_run_times.csv"))
  write.csv(x = form_run_time_df(lr_bests$imputers), file = file.path(output_path, "lr_run_times.csv"))

  ## Saving model
  saveRDS(rf_bests$models, file = file.path(output_path, "rf_classifiers.rds"))
  if (!lean) saveRDS(rf_bests$imputers, file = file.path(output_path, "rf_imputers.rds"))
  saveRDS(rf_bests$hyperparams, file = file.path(output_path, "rf_hp_configs.rds"))

  # glm models in R contain references to environments, but for prediction it doesn't seem that
  # the environment needs to be the exact one defined during training. Using a dummy `refhook`-argument
  # we can bypass saving the environments and save *a lot* of space (~ 50 Mb per model -> 7 Mb per model).
  # See https://stackoverflow.com/questions/54144239/how-to-use-saverds-refhook-parameter for an example of
  # using the `refhook`.
  saveRDS(lr_bests$models, file = file.path(output_path, "lr_classifiers.rds"), refhook = function(x) "")
  if (!lean) saveRDS(lr_bests$imputers, file = file.path(output_path, "lr_imputers.rds"))
  saveRDS(lr_bests$hyperparams, file = file.path(output_path, "lr_hp_configs.rds"))

  saveRDS(colnames(training_data), file.path(output_path, "final_features.rds"))
  flog.pid.info("Done saving data")
}
