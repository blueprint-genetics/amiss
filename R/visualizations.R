library(ggcorrplot)
library(tidyr)

#' Plot correlation matrix over missingness indicators
#'
#' @param data Input data on which to compute correlations.
#' @param features Features (column names) whose missingness indicators to compute correlations on.
#' @param title Title of the plot.
#'
#' @return A ggplot object containing the correlation plot
plot_missingness_correlations <- function(data, features, title) {

  miss_data <- is.na(data)
  missingness_corr <- cor(miss_data[, features])
  ggcorrplot(corr = missingness_corr, type = "lower", title = title)

}
#' Plot correlation matrix over observed values
#'
#' In cases where two features have no row on which both features are observed,
#' or if one feature is constant when both features are observed, the correlation
#' computation gives a missing value. In those cases, the missing values are
#' replaced by -2.0, since that both allows hierarchical clustering to compute a
#' distance matrix (currently not used) and causes ggcorrplot to
#' produce a gray square at the position of the strange value.
#'
#' @param data Input data on which to compute correlations.
#' @param features Features (column names) whose observed values to compute correlations on.
#' @param title Title of the plot.
#'
#' @return A ggplot object containing the correlation plot
plot_observed_correlations <- function(data, features, title) {

  corr <- cor(data[, features], use = "pairwise.complete.obs")
  # This makes hierarchical clustering work, and produces a gray square in the plot as desired
  corr[is.na(corr)] <- -2.0
  ggcorrplot(corr = corr, type = "lower", title = title)

}
#' Plot correlation matrix of missingness indicators versus observed values
#'
#' Diagonals are always NA, since the indicator is constant (== 1) for observations
#' where the feature is available. The same applies to feature combinations (A, B)
#' where A is always missing when B is missing; the missingness indicator for B will
#' be constant when A is available.
#'
#' In those cases, the missing values are
#' replaced by -2.0, since that both allows hierarchical clustering to compute a
#' distance matrix (currently not used) and causes ggcorrplot to
#' produce a gray square at the position of the strange value.
#'
#' @param data Input data on which to compute correlations.
#' @param features Features (column names) whose observed values to compute correlations on.
#' @param title Title of the plot.
#'
#' @return A ggplot object containing the correlation plot
plot_missingness_vs_observed_correlations <- function(data, features, title) {

  data <- data[, features]
  miss_data <- is.na(data)
  colnames(miss_data) <- paste0("miss_", colnames(miss_data))
  missingness_vs_value_corr <- cor(data, miss_data, use = "pairwise.complete.obs")
  # This makes hierarchical clustering work, and produces a gray square in the plot as desired
  missingness_vs_value_corr[is.na(missingness_vs_value_corr)] <- -2.0
  ggcorrplot(missingness_vs_value_corr, title = title)

}

doubleboxplot <- function(metric, rf_perf, lr_perf, per_consequence) {

  rf_perf %<>% gather("metric", "value", metric)
  lr_perf %<>% gather("metric", "value", metric)

  by <- if (per_consequence) c("method", "metric", "consequence") else c("method", "metric")
  rf_med <- aggregate(rf_perf[["value"]], by = rf_perf[, by, drop = FALSE], FUN = median)
  lr_med <- aggregate(lr_perf[["value"]], by = lr_perf[, by, drop = FALSE], FUN = median)

  double_boxplots <- ggplot() +
    geom_boxplot(data = lr_perf, aes(x=method, y=value, fill = "Logistic regression", color = "Logistic regression")) +
    geom_boxplot(data = rf_perf, aes(x = method, y = value, fill = "Random forest", color = "Random forest")) +
    geom_point(data = lr_med, aes(x = method, y = x, color = "Logistic regression"), shape = 18, size = 3) +
    geom_point(data = rf_med, aes(x = method, y = x,  color = "Random forest"), shape = 18, size = 3) +
    scale_y_continuous(breaks = scales::extended_breaks(n = 8)) +
    theme_bw() +
    xlab(label = NULL) +
    (if (length(metric) == 1) ylab(metric) else ylab("Value")) +
    coord_flip() +
    theme(legend.position = 'bottom', legend.direction = "vertical") +
    theme(text = element_text(size = 18)) +
    scale_fill_manual("Classifier", values = c(`Logistic regression` = "#DB607A", `Random forest` = "#AAAAAA")) +
    scale_color_manual("Classifier", aesthetics = c("color", "outlier.color"), values = c(`Logistic regression` = "#A82026", `Random forest` = "#555555"))
  if (per_consequence) {
    double_boxplots <- double_boxplots + facet_wrap(vars(consequence))
    if (metric == "AUC") {
        double_boxplots <- double_boxplots + scale_y_continuous(breaks = seq(0.65, 1.00, 0.05))
    }
  }
  if (length(metric) > 1) {
    double_boxplots <- double_boxplots + coord_flip(ylim = c(0.25, 1.0), clip = "on") + facet_wrap(vars(metric), scales = "fixed")
  }

  return(double_boxplots)
}

rename_methods <- function(perf) {
  perf$method <- factor(perf$method,
                        c("missForest", "bpca", "norm.predict", "pmm", "norm", "rf", "outlier_imp", "max_imp", "min_imp", "zero_imp", "knnImputation", "median_imp", "missingness_indicators", "mean_imp"),
                        c("missForest", "BPCA", "MICE Regr.", "MICE PMM", "MICE Bayes r.", "MICE RF", "Outlier", "Maximum", "Minimum", "Zero", "k-NN", "Median", "Missingness ind.", "Mean"))
  return(perf)
}
rename_consequences <- function(perf) {
  perf$consequence <- factor(perf$consequence,
                             perf$consequence %>% unique,
                             gsub(CONSEQUENCE_COLUMN %>% paste0("."), "", perf$consequence %>% unique))
  return(perf)
}

add_n_to_consequence_name <-  function(perf) {
  consequence_names <- unique(perf$consequence)
  n <- perf$TP + perf$FP + perf$FN + perf$TN
  pos <- perf$TP + perf$FN
  neg <- perf$FP + perf$TN
  df <- data.frame(n = n, pos = pos, neg = neg)
  df <- lapply(consequence_names, function(name) df[perf$consequence == name,][1,])
  df <- do.call(rbind, df)
  row.names(df) <- consequence_names

  stopifnot(all(sapply(consequence_names, function(name) all(n[perf$consequence == name] == df[name, "n"]))))

  new_consequence_names <- paste0(consequence_names, "\nN = ", df$n, ", positive = ", df$pos, ", negative = ", df$neg )
  perf$consequence <- factor(perf$consequence, consequence_names, new_consequence_names)

  return(perf)

}
