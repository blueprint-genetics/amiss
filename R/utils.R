#' Partition a number range representing row numbers into `n` roughly equally-sized batches
#'
#' @param num_rows The number range to be divided is `1:num_rows`
#' @param batches Number of batches that the rows should be partitioned into
#'
#' @return List containing vectors of row numbers.
#' @importFrom magrittr %>%
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
  futile.logger::flog.info(paste0("pid=", Sys.getpid(), " ", msg), ...)
}
flog.pid.debug <- function(msg, ...) {
  futile.logger::flog.debug(paste0("pid=", Sys.getpid(), " ", msg), ...)
}

form_run_time_df <- function(imputers, times_imputed) {
  timing <- purrr::map(.x = imputers, function(x) attr(x, TIMING_ATTR))
  # Make sure that method and elapsed columns exist even if timing is empty
  timing_df <- do.call(rbind, timing) %>% data.frame
  timing_df$method <- row.names(timing_df)
  if(!is.null(timing_df$method) && !is.null(timing_df$elapsed)) {
    timing_df <- timing_df %>% magrittr::extract(c("method", "elapsed"))
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
  if (is.na(cores))
      cores <- 1
  return(cores)
}

#' Generate a file prefix / folder name for a file / folder generated
#' with specific parameter values.
#'
#' @param combination
#' @param subset
#' @param parameter_separator
#' @param value_separator
#'
#' @return
#' @export
generate_file_prefix <- function(combination, subset = PREPROCESSING_PARAMETER_SUBSET, parameter_separator=".", value_separator="-") {
  relevant_parameters <- combination[names(combination) %in% subset] %>% sort
  prefix <- paste0(names(relevant_parameters), "-", relevant_parameters, collapse = parameter_separator)
  return(prefix)
}
decode_file_prefix <- function(path, parameter_separator=".", value_separator="-") {
  # Strip training "/"s
  p <- str_remove(path, "/$")
  p %<>% str_split(pattern = fixed("/")) %>% extract2(1) %>% tail(1)
  p %<>% str_split(pattern = fixed(parameter_separator), simplify = FALSE) %>% extract2(1)
  p %<>% str_split(pattern = fixed(value_separator))
  p %<>% sapply(FUN = function(x) list(x[2]) %>% set_names(x[1]))
  p <- do.call(list, p)
  
  return(p)
}
