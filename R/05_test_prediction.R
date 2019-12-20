
## Read data
library(magrittr)
library(futile.logger)
library(mice)
library(ModelMetrics)
library(caret)
library(stringr)
library(ggplot2)
library(gridExtra)
library(foreach)
library(doParallel)
library(doRNG)
library(DMwR)

flog.appender(appender.tee("05_test_prediction.log"))
flog.threshold(DEBUG)
flog.info("05_test_prediction.log")

source("R/recursive_application.R")
source("R/imputation_definitions.R")
source("R/imputation.R")

cores <- 3
flog.info("Using %d cores", cores)
registerDoParallel(cores)
seed <- 42
flog.info("Using seed: %d", seed)
set.seed(seed)

flog.info("Reading data")
test_data <- read.csv("contracted_test_data.csv", row.names = 1, as.is = TRUE)
outcome <- read.csv("test_outcomes.csv", as.is = TRUE)
outcome <- factor(outcome[,2], levels = c("positive", "negative"))

flog.info("Creating output directory")
results_dir_path <- "output/results/"
if (!dir.exists(results_dir_path)) {
  dir_creation_success <- dir.create(results_dir_path, showWarnings = TRUE)
  if (!dir_creation_success) {
    stop("Failed to create directory for saving results.")
  }
}

# Keep exactly those features that were kept in training data
flog.info("Reading features used in training")
final_features <- readRDS("output/final_features.rds")
test_data <- test_data[, final_features]


## Multiply impute the test set using the best hyperparameter configurations from the training set

flog.info("Reading best hyperparameter configurations for imputation methods")
rf_hyperparams <- readRDS("output/rf_hp_configs.rds")
lr_hyperparams <- readRDS("output/lr_hp_configs.rds")

flog.info("Starting imputation of test set")
times <- 5
iters <- 1
flog.info("For MICE methods, imputing %d times, with max. %d iterations", times, iters)

impute_w_hps <- function(data, hp_tree){

  imputations <- foreach(hps = enumerate(hp_tree), .options.RNG = seed) %dorng% {

    # The imputation parameters estimated from the training set should be used 
    # where possible.
    estimates <- attr(hps$value, "imputation_estimates")

    method <- hps$name
    flog.info("Imputing with method %s", method)
    if (method %in% names(mice_imputation_hyperparameters)) {

      run_mice(data, method, hps$value, times, iters)

    } else if (method == "bpca") {

      run_bpca(data, hps$value)

    } else if (method == "knnImputation") {

      run_knn(data, hps$value, old_data = estimates)

    } else if (method == "missingness_indicators") {

      remove_vector <- colnames(data) %in% estimates
      list(completed_datasets = list(`1` = missingness_indicators(data, remove_vector = remove_vector)))

    } else if (method %in% names(single_value_imputation_hyperparameter_grids)) {

      list(completed_datasets = list(`1` = reimpute(dataframe = data, value = estimates)))

    }
  }
  names(imputations) <- names(hp_tree)

  completions <- imputations %>% lapply(. %>% extract2(1))

  return(completions)
}
flog.info("Imputation of test set with best hyperparameter configurations for RF")
rf_completions <- impute_w_hps(test_data, rf_hyperparams)
flog.info("Imputation of test set with best hyperparameter configurations for LR")
lr_completions <- impute_w_hps(test_data, lr_hyperparams)


## Predict on test set completions using best classifier models

flog.info("Reading classifier models")
rf_models <- readRDS("output/rf_classifiers.rds")
lr_models <- readRDS("output/lr_classifiers.rds", refhook = function(x) .GlobalEnv)

prediction <- function(models, completions) {

  predictions <- lapply(names(models), function(method) {

    pred_per_model <- lapply(models[[method]], function(model) {

      pred_per_completion <- foreach(completed_dataset = completions[[method]], .options.RNG = seed) %dorng% {
        tryCatch({
          flog.info("Predicting using best model for %s", method)
          predict(model, completed_dataset, type = "prob")[,"positive", drop = TRUE]
        }, error = function(e) flog.debug(e))
      }

      names(pred_per_completion) <- paste0("imp_", seq_along(pred_per_completion))
      pred_per_completion

    })

    names(pred_per_model) <- paste0("model_", seq_along(pred_per_model))
    pred_per_model

  })
  names(predictions) <- names(models)
  return(predictions)
}

flog.info("Starting prediction by RF models")
rf_predictions <- prediction(rf_models, rf_completions)
flog.info("Starting prediction by LR models")
lr_predictions <- prediction(lr_models, lr_completions)

## Compute performance statistics on the test set
flog.info("Computing performance statistics")
performance_stats <- function(predictions) {

  confusion_matrices <- recursive_apply_numeric(predictions, function(pred) {
    pred <- factor(c("positive", "negative")[2 - (pred > 0.5)], c("positive", "negative"))
    caret::confusionMatrix(pred, outcome)
  })

  extract_stat <- function(stat) function(x) x %>% use_series("byClass") %>% extract(stat)

  positive_outcome_indicator <- as.integer(outcome == "positive")

  mcc <- recursive_apply_numeric(predictions, . %>% mcc(actual = positive_outcome_indicator, predicted = ., cutoff = 0.5))
  auc <- recursive_apply_numeric(predictions, . %>% auc(actual = positive_outcome_indicator, predicted = .))

  recursive_apply_cm <- function(x, fun) recursive_apply(x = x, fun = fun, x_class = "confusionMatrix")

  tp <- recursive_apply_cm(confusion_matrices, . %>% use_series("table") %>% extract("positive", "positive") %>% as.numeric %>% set_names("tp"))
  fp <- recursive_apply_cm(confusion_matrices, . %>% use_series("table") %>% extract("positive", "negative") %>% as.numeric %>% set_names("fp"))
  fn <- recursive_apply_cm(confusion_matrices, . %>% use_series("table") %>% extract("negative", "positive") %>% as.numeric %>% set_names("fn"))
  tn <- recursive_apply_cm(confusion_matrices, . %>% use_series("table") %>% extract("negative", "negative") %>% as.numeric %>% set_names("tn"))
  sensitivity <- recursive_apply_cm(confusion_matrices, extract_stat("Sensitivity"))
  specificity <- recursive_apply_cm(confusion_matrices, extract_stat("Specificity"))
  f1 <- recursive_apply_cm(confusion_matrices, extract_stat("F1"))
  precision <- recursive_apply_cm(confusion_matrices, extract_stat("Precision"))
  recall <- recursive_apply_cm(confusion_matrices, extract_stat("Recall"))

  perfs <- list(tp = tp,
                fp = fp,
                fn = fn,
                tn = tn,
                mcc = mcc,
                auc = auc,
                sensitivity = sensitivity,
                specificity = specificity,
                f1 = f1,
                precision = precision,
                recall = recall)

  return(perfs)

}
rf_perf <- performance_stats(rf_predictions)
lr_perf <- performance_stats(lr_predictions)


turn_table <- function(perf_tree) {

  tree_names <- recursive_apply_numeric(perf_tree, function(x, name_list) return(name_list), pass_node_names = TRUE)
  tree_names %<>% leaf_apply(. %>% paste0(collapse = ":"), docall = FALSE)
  tree_names %<>% unlist(use.names = FALSE)

  values <- perf_tree %>% unlist(use.names = FALSE)
  names(values) <- tree_names

  df <- lapply(names(values), function(name) {
    stringr::str_split(string = name, pattern = stringr::fixed(":"), simplify = TRUE)
  })
  df <- data.frame(do.call(rbind, df), value = values)

  colnames(df) <- c("method", "model_index", "test_realization", "value")

  return(df)
}

rf_tables <- lapply(rf_perf, turn_table)
lr_tables <- lapply(lr_perf, turn_table)

merge_tables <- function(tables) {
  perf_table <- Reduce(function(x, y) merge(x, y, by = c("method", "model_index", "test_realization")), tables)
  colnames(perf_table) <- c("method", "model_index", "test_realization", names(tables))
  perf_table
}
rf_perf_table <- merge_tables(rf_tables)
lr_perf_table <- merge_tables(lr_tables)


flog.info("Writing performance tables")
write.csv(x = rf_perf_table, file = paste0(results_dir_path, "rf_performance.csv"), row.names = FALSE)
write.csv(x = lr_perf_table, file = paste0(results_dir_path, "lr_performance.csv"), row.names = FALSE)


flog.info("Averaging performance tables")
aggregate_over_perf_table <- function(perf_table) {

  perf_stats <- perf_table[, !colnames(perf_table) %in% c("method", "model_index", "test_realization")]

  return(list(
    model_mean = aggregate(perf_stats, perf_table["method"], mean),
    model_sd = aggregate(perf_stats, perf_table["method"], sd),
    over_test_mean = aggregate(perf_stats, perf_table[c("method", "model_index")], mean),
    over_test_sd = aggregate(perf_stats, perf_table[c("method", "model_index")], sd),
    over_train_mean = aggregate(perf_stats, perf_table[c("method", "test_realization")], mean),
    over_train_sd = aggregate(perf_stats, perf_table[c("method", "test_realization")], sd)
  ))

}
rf_perf_aggregations <- aggregate_over_perf_table(rf_perf_table)
lr_perf_aggregations <- aggregate_over_perf_table(lr_perf_table)

flog.info("Writing averaged performance tables")
for (name in names(rf_perf_aggregations)) {
  write.csv(x = rf_perf_aggregations[[name]],
            file = paste0(results_dir_path, "rf_", name, ".csv"),
            row.names = FALSE)
}
for (name in names(lr_perf_aggregations)) {
  write.csv(x = lr_perf_aggregations[[name]],
            file = paste0(results_dir_path, "lr_", name, ".csv"),
            row.names = FALSE)
}



flog.info("Plotting and writing performance boxplots")
# RF MCC
rf_mcc_boxplots <- arrangeGrob(
  ggplot(rf_perf_table, aes(x = method, y = mcc)) + geom_boxplot() + ggtitle("MCC of random forest classifier"),
  ggplot(rf_perf_aggregations$over_test_mean, aes(x = method, y = mcc)) + geom_boxplot() + ggtitle("MCC of random forest classifier", subtitle = "Aggregated over test set realizations"),
  ggplot(rf_perf_aggregations$over_train_mean, aes(x = method, y = mcc)) + geom_boxplot() + ggtitle("MCC of random forest classifier", subtitle = "Aggregated over training set realizations")
)
ggsave(filename =  "rf_mcc_boxplots.pdf", plot = rf_mcc_boxplots, device = "pdf", path = results_dir_path, width = 210, height = 297, units = "mm")

# RF AUC-ROC
rf_roc_boxplots <- arrangeGrob(
  ggplot(rf_perf_table, aes(x = method, y = auc)) + geom_boxplot() + ggtitle("AUC-ROC of random forest classifier"),
  ggplot(rf_perf_aggregations$over_test_mean, aes(x = method, y = auc)) + geom_boxplot() + ggtitle("AUC-ROC of random forest classifier", subtitle = "Aggregated over test set realizations"),
  ggplot(rf_perf_aggregations$over_train_mean, aes(x = method, y = auc)) + geom_boxplot() + ggtitle("AUC-ROC of random forest classifier", subtitle = "Aggregated over training set realizations")
)
ggsave(filename =  "rf_roc_boxplots.pdf", plot = rf_roc_boxplots, device = "pdf", path = results_dir_path, width = 210, height = 297, units = "mm")

# LR MCC
lr_mcc_boxplots <- arrangeGrob(
  ggplot(lr_perf_table, aes(x = method, y = mcc)) + geom_boxplot() + ggtitle("MCC of logistic regression classifier"),
  ggplot(lr_perf_aggregations$over_test_mean, aes(x = method, y = mcc)) + geom_boxplot() + ggtitle("MCC of logistic regression classifier", subtitle = "Aggregated over test set realizations"),
  ggplot(lr_perf_aggregations$over_train_mean, aes(x = method, y = mcc)) + geom_boxplot() + ggtitle("MCC of logistic regression classifier", subtitle = "Aggregated over training set realizations")
)
ggsave(filename =  "lr_mcc_boxplots.pdf", plot = lr_mcc_boxplots, device = "pdf", path = results_dir_path, width = 210, height = 297, units = "mm")

# LR AUC-ROC
lr_roc_boxplots <- arrangeGrob(
  ggplot(lr_perf_table, aes(x = method, y = auc)) + geom_boxplot() + ggtitle("AUC-ROC of logistic regression classifier"),
  ggplot(lr_perf_aggregations$over_test_mean, aes(x = method, y = auc)) + geom_boxplot() + ggtitle("AUC-ROC of logistic regression classifier", subtitle = "Aggregated over test set realizations"),
  ggplot(lr_perf_aggregations$over_train_mean, aes(x = method, y = auc)) + geom_boxplot() + ggtitle("AUC-ROC of logistic regression classifier", subtitle = "Aggregated over training set realizations")
)
ggsave(filename =  "lr_roc_boxplots.pdf", plot = lr_roc_boxplots, device = "pdf", path = results_dir_path, width = 210, height = 297, units = "mm")

flog.info("Done")
