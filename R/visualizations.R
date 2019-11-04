library(ggcorrplot)

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
