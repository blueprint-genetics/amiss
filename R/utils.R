library(futile.logger)

flog.threshold(DEBUG)
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

find_dummies <- function(original_variable_name, name_list) {
  name_list[name_list %>% startsWith(original_variable_name) & !name_list %in% original_variable_name]
}

flog.pid.info <- function(msg, ...) {
  flog.info(paste0("pid=", Sys.getpid(), " ", msg), ...)
}
flog.pid.debug <- function(msg, ...) {
  flog.debug(paste0("pid=", Sys.getpid(), " ", msg), ...)
}

form_run_time_df <- function(imputers) {
  times <- map(.x = imputers, function(x) attr(x, "timing"))
  times_df <- do.call(rbind, times) %>% data.frame
  times_df$method <- row.names(times_df)
  times_df <- times_df %>% extract(c("method", "elapsed"))
  row.names(times_df) <- NULL

  # Stochastic methods are timed over the repetitions, so need to be divided by the number of repetitions (10)
  times_df[times_df$method %in% c("pmm", "norm.predict", "norm", "rf", "knnImpute", "missForest"), "elapsed"] %<>% `/`(10)

  return(times_df)
}
