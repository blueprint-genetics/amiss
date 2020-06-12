library(futile.logger)
library(here)

source(here("R", "constants.R"))

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

form_run_time_df <- function(imputers, times_imputed) {
  timing <- map(.x = imputers, function(x) attr(x, TIMING_ATTR))
  # Make sure that method and elapsed columns exist even if timing is empty
  timing_df <- do.call(rbind, timing) %>% data.frame
  timing_df$method <- row.names(timing_df)
  if(!is.null(timing_df$method) && !is.null(timing_df$elapsed)) {
    timing_df <- timing_df %>% extract(c("method", "elapsed"))
  } else {
    timing_df <- data.frame(method = character(0), elapsed = numeric(0))
  }
  row.names(timing_df) <- NULL

  # Stochastic methods are timed over the repetitions, so need to be divided by the number of repetitions
  timing_df[timing_df$method %in% c("pmm", "norm.predict", "norm", "rf", "knnImpute", "missForest"), "elapsed"] %<>% `/`(times_imputed)

  return(timing_df)
}

create_dir <- function(path) {
  if (!dir.exists(path)) {
    dir_creation_success <- dir.create(path, showWarnings = TRUE)
    if (!dir_creation_success) {
      stop("Failed to create directory for saving results.")
    }
  }
}

get_env_cores <- function() {
    cores <- Sys.getenv("AMISS_CORES")
    cores <- as.integer(cores)
    if(is.na(cores))
        cores <- 1
    return(cores)
}

