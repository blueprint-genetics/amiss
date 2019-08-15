library(tidyr)

divide_rows_into_batches <- function(num_rows, batches = 100) {
  
  breakpoints <- seq(1, num_rows, length.out = batches)
  row_batch_indicator <- findInterval(1:num_rows, breakpoints)
  batch_list <- split(1:num_rows, row_batch_indicator)
  
  return(batch_list)
  
}

correlation_tidy <- function(data, cluster = FALSE) {
  corr <- cor(data, use = "pairwise.complete.obs")
  if (cluster) {
    variables <- colnames(data)
    clustering_order <- hclust(dist(corr))$order
    corr <- data.frame(corr, stringsAsFactors = TRUE)
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

missingness_correlation_tidy <- function(data, cluster = FALSE) {
  
  miss_corr <- is.na(data) %>% data.frame
  # Keep only columns with missingness
  miss_corr <- miss_corr[, sapply(miss_corr, any), drop=FALSE]
  # Turn logical columns into integer columns
  miss_corr[,] <- miss_corr %>% sapply(as.integer)
  
  miss_corr %<>% cor
  miss_corr %<>% correlation_frame(cluster = cluster)
  
  return(miss_corr)
}
