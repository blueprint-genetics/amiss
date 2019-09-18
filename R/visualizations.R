library(ggcorrplot)

plot_missingness_correlations <- function(data, features, title) {
  miss_data <- is.na(data)
  missingness_corr <- cor(miss_data[, features])
  ggcorrplot(corr = missingness_corr, type = "lower", title = title)
}
plot_observed_correlations <- function(data, features, title) {
  corr <- cor(data[, features], use = "pairwise.complete.obs")
  # This makes hierarchical clustering work, and produces a gray square in the plot as desired
  corr[is.na(corr)] <- -2.0
  ggcorrplot(corr = corr, type = "lower", title = title)
}
plot_missingness_vs_observed_correlations <- function(data, features, title) {
  miss_data <- is.na(data)
  colnames(miss_data) <- paste0("miss_", colnames(miss_data))
  missingness_vs_value_corr <- cor(data, miss_data, use = "pairwise.complete.obs")
  # This makes hierarchical clustering work, and produces a gray square in the plot as desired
  missingness_vs_value_corr[is.na(missingness_vs_value_corr)] <- -2.0
  ggcorrplot(missingness_vs_value_corr, title = title)
}
