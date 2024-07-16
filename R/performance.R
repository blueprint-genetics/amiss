#' Compute performance statistics from predictions tree and ground truth vector
#'
#' @param predictions Tree of numeric vector containing predicted class probability for positive class
#' @param outcome Ground-truth vector
#'
#' @return A list that contains a tree for each performance metric, each tree's structure matching that of `predictions`.
#'
#' @importFrom magrittr %>%
#' @export
performance_stats <- function(predictions, outcome) {

  if (!is.factor(outcome)) stop("`outcome` must be a factor")

  outcome_lvls <- levels(outcome)
  if (length(outcome_lvls) != 2) stop("`outcome` must have exactly 2 levels")

  confusion_matrices <- recursive_apply_numeric(predictions, function(pred) {
    pred <- factor(outcome_lvls[2 - (pred > 0.5)], outcome_lvls)
    caret::confusionMatrix(pred, outcome)
  })

  extract_stat <- function(stat) function(x) x %>% magrittr::use_series("byClass") %>% magrittr::extract(stat)
  extract_accuracy <- function(x) x %>% magrittr::use_series("overall") %>% magrittr::extract("Accuracy")
  extract_tr_time <- function(x) x %>% attr("tr_time")
  extract_te_time <- function(x) x %>% attr("te_time")

  positive_outcome_indicator <- as.integer(outcome == outcome_lvls[1])

  brier <- recursive_apply_numeric(predictions, . %>% ModelMetrics::brier(actual = positive_outcome_indicator, predicted = .))
  mcc <- recursive_apply_numeric(predictions, . %>% ModelMetrics::mcc(actual = positive_outcome_indicator, predicted = ., cutoff = 0.5))
  auc <- recursive_apply_numeric(predictions, . %>% ModelMetrics::auc(actual = positive_outcome_indicator, predicted = .))

  tr_time <- recursive_apply_numeric(predictions, extract_tr_time)
  te_time <- recursive_apply_numeric(predictions, extract_te_time)

  recursive_apply_cm <- function(x, fun) recursive_apply(x = x, fun = fun, x_class = "confusionMatrix")

  tp <- recursive_apply_cm(confusion_matrices, . %>% magrittr::use_series("table") %>% magrittr::extract(outcome_lvls[1], outcome_lvls[1]) %>% as.numeric %>% magrittr::set_names("tp"))
  fp <- recursive_apply_cm(confusion_matrices, . %>% magrittr::use_series("table") %>% magrittr::extract(outcome_lvls[1], outcome_lvls[2]) %>% as.numeric %>% magrittr::set_names("fp"))
  fn <- recursive_apply_cm(confusion_matrices, . %>% magrittr::use_series("table") %>% magrittr::extract(outcome_lvls[2], outcome_lvls[1]) %>% as.numeric %>% magrittr::set_names("fn"))
  tn <- recursive_apply_cm(confusion_matrices, . %>% magrittr::use_series("table") %>% magrittr::extract(outcome_lvls[2], outcome_lvls[2]) %>% as.numeric %>% magrittr::set_names("tn"))

  accuracy <- recursive_apply_cm(confusion_matrices, extract_accuracy)
  sensitivity <- recursive_apply_cm(confusion_matrices, extract_stat("Sensitivity"))
  specificity <- recursive_apply_cm(confusion_matrices, extract_stat("Specificity"))
  f1 <- recursive_apply_cm(confusion_matrices, extract_stat("F1"))
  precision <- recursive_apply_cm(confusion_matrices, extract_stat("Precision"))

  perfs <- list(TP = tp,
                FP = fp,
                FN = fn,
                TN = tn,
                Accuracy = accuracy,
                Brier = brier,
                MCC = mcc,
                AUC = auc,
                Sensitivity = sensitivity,
                Specificity = specificity,
                F1 = f1,
                Precision = precision,
                tr_time = tr_time,
                te_time = te_time)

  return(perfs)

}

#' Transform a performance statistics tree to a flat table
#'
#' @param perf_tree List of trees, each representing a performance statistic
#'
#' @return A data.frame whose columns are
#' 1) imputation method name
#' 2) classifier index (relevant with multiple imputation; depends on completion of training set)
#' 3) test set index (relevant with multiple imputation)
#' 4) value of the leaf
#' @export
transform_perf_tree_to_table <- function(perf_tree) {

  tree_names <- get_tree_names(perf_tree, x_class = "numeric")

  values <- perf_tree %>% unlist(use.names = FALSE)
  names(values) <- tree_names

  if (!is.null(names(values))) {
    df <- lapply(names(values), function(name) {
      stringr::str_split(string = name, pattern = stringr::fixed(":"), simplify = TRUE)
    })
    df <- data.frame(do.call(rbind, df), value = values)
  } else{
    df <- data.frame()
  }

  if (ncol(df) == 0) {
    df <- data.frame(t(1:4))[FALSE,]
  }

  colnames(df) <- c(METHOD_COLUMN, MODEL_INDEX_COLUMN, TEST_COMPLETION_INDEX_COLUMN, "value")

  return(df)
}

#' Merge tables formed with `transform_perf_tree_to_table`
#'
#' @param tables List of tables formed with `transform_perf_tree_to_table`
#'
#' @return A wide-format data.frame containing data from all tables in input
#' @export
merge_tables <- function(tables) {

  perf_table <- tables[[1]]
  for (i in 2:length(tables)) {
    # The `suffixes`-argument is used only to silence warnings
    # by giving columns unique names until they are replaced
    # immediately afterwards.
    perf_table <- merge(x = perf_table,
                        y = tables[[i]],
                        by = c(
                          METHOD_COLUMN,
                          MODEL_INDEX_COLUMN,
                          TEST_COMPLETION_INDEX_COLUMN
                        ),
                        suffixes = c(paste0(c(".x", ".y"), i)))
  }
  if (NROW(perf_table) == 0) {
    perf_table <- rbind(perf_table, rep(NA, NCOL(perf_table)) %>% t)
  }
  colnames(perf_table) <- c(METHOD_COLUMN, MODEL_INDEX_COLUMN, TEST_COMPLETION_INDEX_COLUMN, names(tables))
  return(perf_table)
}
