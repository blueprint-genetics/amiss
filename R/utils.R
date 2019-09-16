library(tidyr)

#' Partition a number range representing row numbers into `n` roughly equally-sized batches
#'
#' @param num_rows The number range to be divided is `1:num_rows`
#' @param batches Number of batches that the rows should be partitioned into
#'
#' @return List containing vectors of row numbers.
partition_rows_into_batches <- function(num_rows, batches = 100) {
  
  breakpoints <- seq(1, num_rows, length.out = batches + 1)
  # Keep only midpoints
  breakpoints <- breakpoints[2:batches]
  row_batch_indicator <- findInterval(1:num_rows, breakpoints)
  batch_list <- split(1:num_rows, row_batch_indicator)
  
  return(batch_list)
  
}

#' Form a tidy data.frame of correlations (optionally with hierarchical clustering)
#'
#' First and second columns are factors, together representing all the combinations of variables
#' (with levels ordered according to the clustering), and the third is numeric and contains a 
#' correlation for the two variables on that row.
#' 
#' @param data A data.frame.
#' @param cluster Whether to perform clustering or not.
#'
#' @return Tidy data.frame representing correlations. 
correlation_tidy <- function(data, cluster = FALSE) {
  
  # Compute correlations using available observations
  corr <- cor(data, use = "pairwise.complete.obs")
  
  if (cluster) {
    # Order variable names by hierarchical clustering (for ggplot2)
    variables <- colnames(data)
    clustering_order <- hclust(dist(corr))$order
    variables <- variables[clustering_order]
  }
  corr <- data.frame(x = row.names(corr), corr, stringsAsFactors = TRUE)
  corr %<>% gather(y, value, -x, factor_key = TRUE, na.rm = TRUE)
  
  if(cluster) {
    corr$x <- factor(corr$x, levels=variables)
    corr$y <- factor(corr$y, levels=variables)
  }
  
  return(corr)
}

#' Form a tidy data.frame of the correlations of missingness indicators 
#' (optionally with hierarchical clustering).
#'
#' @param data A data.frame.
#' @param cluster Whether to perform clustering or not.
#'
#' @return Tidy data.frame representing correlations between missingness indicators.
missingness_correlation_tidy <- function(data, cluster = FALSE) {
  
  miss_corr <- is.na(data) %>% data.frame
  # Keep only columns with missingness
  miss_corr <- miss_corr[, sapply(miss_corr, any), drop=FALSE]
  # Turn logical columns into integer columns
  miss_corr[,] <- miss_corr %>% sapply(as.integer)
  
  miss_corr %<>% cor
  miss_corr %<>% correlation_tidy(cluster = cluster)
  
  return(miss_corr)
}
