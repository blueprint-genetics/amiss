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

  rf_perf %<>% pivot_longer(cols = metric, names_to = "metric", values_to = "value")
  lr_perf %<>% pivot_longer(cols = metric, names_to = "metric", values_to = "value")

  by <- if (per_consequence) c("method", "metric", "consequence") else c("method", "metric")
  rf_med <- aggregate(rf_perf[["value"]], by = rf_perf[, by, drop = FALSE], FUN = median)
  lr_med <- aggregate(lr_perf[["value"]], by = lr_perf[, by, drop = FALSE], FUN = median)

  double_boxplots <- ggplot() +
    geom_boxplot(data = lr_perf, color = "#A82026", aes(x=method, y=value, fill = "Logistic regression"), outlier.color = "#DB607A" ) +
    geom_boxplot(data = rf_perf, aes(x = method, y = value, fill = "Random forest"), outlier.color = "#41444C" ) +
    geom_point(data = lr_med, color = "#A82026", aes(x = method, y = x, fill = "Logistic regression"), shape = 18, size = 3) +
    geom_point(data = rf_med, aes(x = method, y = x,  fill = "Random forest"), shape = 18, size = 3) +
    theme_bw() +
    xlab(label = NULL) +
    (if (length(metric) == 1) ylab(metric) else ylab("Value")) +
    coord_flip() +
    theme(legend.position = 'bottom', legend.direction = "vertical") +
    theme(text = element_text(size = 18)) +
    scale_fill_manual("Classifier", values = c("#DB607A", "#41444C", "#11222A"))
  if (per_consequence) {
    double_boxplots <- double_boxplots + facet_wrap(vars(consequence))
  }
  if (length(metric) > 1) {
    double_boxplots <- double_boxplots + coord_flip(ylim = c(0.25, 1.0), clip = "on") + facet_wrap(vars(metric), scales = "fixed")
  }

  return(double_boxplots)
}
