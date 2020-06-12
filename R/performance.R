library(ModelMetrics)
library(magrittr)
library(here)

source(here("R", "recursive_application.R"))
source(here("R", "constants.R"))

performance_stats <- function(predictions, outcome) {
  
  if (!is.factor(outcome)) stop("`outcome` must be a factor")

  outcome_lvls <- levels(outcome)
  if (length(outcome_lvls) != 2) stop("`outcome` must have exactly 2 levels")
  
  confusion_matrices <- recursive_apply_numeric(predictions, function(pred) {
    pred <- factor(outcome_lvls[2 - (pred > 0.5)], outcome_lvls)
    caret::confusionMatrix(pred, outcome)
  })

  extract_stat <- function(stat) function(x) x %>% use_series("byClass") %>% extract(stat)
  extract_accuracy <- function(x) x %>% use_series("overall") %>% extract("Accuracy")

  positive_outcome_indicator <- as.integer(outcome == outcome_lvls[1])

  brier <- recursive_apply_numeric(predictions, . %>% brier(actual = positive_outcome_indicator, predicted = .))
  mcc <- recursive_apply_numeric(predictions, . %>% mcc(actual = positive_outcome_indicator, predicted = ., cutoff = 0.5))
  auc <- recursive_apply_numeric(predictions, . %>% auc(actual = positive_outcome_indicator, predicted = .))

  recursive_apply_cm <- function(x, fun) recursive_apply(x = x, fun = fun, x_class = "confusionMatrix")

  tp <- recursive_apply_cm(confusion_matrices, . %>% use_series("table") %>% extract(outcome_lvls[1], outcome_lvls[1]) %>% as.numeric %>% set_names("tp"))
  fp <- recursive_apply_cm(confusion_matrices, . %>% use_series("table") %>% extract(outcome_lvls[1], outcome_lvls[2]) %>% as.numeric %>% set_names("fp"))
  fn <- recursive_apply_cm(confusion_matrices, . %>% use_series("table") %>% extract(outcome_lvls[2], outcome_lvls[1]) %>% as.numeric %>% set_names("fn"))
  tn <- recursive_apply_cm(confusion_matrices, . %>% use_series("table") %>% extract(outcome_lvls[2], outcome_lvls[2]) %>% as.numeric %>% set_names("tn"))
  
  accuracy <- recursive_apply_cm(confusion_matrices, extract_accuracy)
  sensitivity <- recursive_apply_cm(confusion_matrices, extract_stat("Sensitivity"))
  specificity <- recursive_apply_cm(confusion_matrices, extract_stat("Specificity"))
  f1 <- recursive_apply_cm(confusion_matrices, extract_stat("F1"))
  precision <- recursive_apply_cm(confusion_matrices, extract_stat("Precision"))

  perfs <- list(tp = tp,
                fp = fp,
                fn = fn,
                tn = tn,
                accuracy = accuracy,
                brier = brier,
                mcc = mcc,
                auc = auc,
                sensitivity = sensitivity,
                specificity = specificity,
                f1 = f1,
                precision = precision)

  return(perfs)

}

turn_table <- function(perf_tree) {

  tree_names <- get_tree_names(perf_tree, x_class = "numeric")

  values <- perf_tree %>% unlist(use.names = FALSE)
  names(values) <- tree_names

  df <- lapply(names(values), function(name) {
    stringr::str_split(string = name, pattern = stringr::fixed(":"), simplify = TRUE)
  })
  df <- data.frame(do.call(rbind, df), value = values)

  colnames(df) <- c(METHOD_COLUMN, MODEL_INDEX_COLUMN, TEST_COMPLETION_INDEX_COLUMN, "value")

  return(df)
}

merge_tables <- function(tables) {
  perf_table <- Reduce(function(x, y) merge(x, y, by = c(METHOD_COLUMN, MODEL_INDEX_COLUMN, TEST_COMPLETION_INDEX_COLUMN)), tables)
  colnames(perf_table) <- c(METHOD_COLUMN, MODEL_INDEX_COLUMN, TEST_COMPLETION_INDEX_COLUMN, names(tables))
  perf_table
}
