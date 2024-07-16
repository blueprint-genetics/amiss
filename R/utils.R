#' Partition a number range representing row numbers into `n` roughly equally-sized batches
#'
#' @param num_rows The number range to be divided is `1:num_rows`
#' @param batches Number of batches that the rows should be partitioned into
#'
#' @return List containing vectors of row numbers.
#' @importFrom magrittr %>%
#' @export
partition_rows_into_batches <- function(num_rows, batches = 100) {

  breakpoints <- seq(1, num_rows, length.out = batches + 1)
  # Keep only midpoints
  breakpoints <- breakpoints[2:batches]
  row_batch_indicator <- findInterval(1:num_rows, breakpoints)
  batch_list <- split(1:num_rows, row_batch_indicator)

  return(batch_list)

}

#' Find dummy variables based on the variable names
#'
#' @param original_variable_name Name of the original variable from which dummy variables were generated
#' @param name_list List of variables
#' @return List of dummy derivatives of `original_variable_name`
#'
#' @export
find_dummies <- function(original_variable_name, name_list) {
  name_list[name_list %>% startsWith(original_variable_name) & !name_list %in% original_variable_name]
}

#' flog.info with process id prepended to message
#'
#' @export
flog.pid.info <- function(msg, ...) {
  futile.logger::flog.info(paste0("pid=", Sys.getpid(), " ", msg), ...)
}
#' flog.debug with process id prepended to message
#'
#' @export
flog.pid.debug <- function(msg, ...) {
  futile.logger::flog.debug(paste0("pid=", Sys.getpid(), " ", msg), ...)
}

#' dir.create wrapper
#'
#' @param path
#'
#' @return
#' @export
create_dir <- function(path) {
  if (!dir.exists(path)) {
    dir_creation_success <- dir.create(path, showWarnings = TRUE)
    if (!dir_creation_success) {
      stop("Failed to create directory for saving results.")
    }
  }
}

#' Generate a string for use as a folder name generated with specific parameter values
#'
#' @param combination List mapping parameter names to values
#' @param subset Character vector listing all parameters that should be included in the output
#' @param parameter_separator String with which parameters will be separated in the output
#' @param value_separator String with which parameter name and value will be separated in the output
#'
#' @return String generated from specified parameters and their values
#' @export
generate_parameter_dependent_name <- function(combination, subset = PREPROCESSING_PARAMETER_SUBSET, parameter_separator=".", value_separator="-") {
  relevant_parameters <- combination[names(combination) %in% subset]
  relevant_parameters <- relevant_parameters[names(relevant_parameters) %>% order]
  prefix <- paste0(names(relevant_parameters), "-", relevant_parameters, collapse = parameter_separator)
  return(prefix)
}

#' Decode folder name to form list mapping parameter names to values
#'
#' This is an "inverse" operation of `generate_parameter_dependent_path` in the sense that
#' it constructs the parameter name-value pairs from a string that was generated with
#' `generate_parameter_dependent_name`.
#'
#' @param path Path to decode
#' @param parameter_separator String that marks separation between different parameters
#' @param value_separator String that marks separation between parameter name and value
#'
#' @return List mapping parameter names to values
#' @export
decode_parameter_dependent_path <- function(path, parameter_separator=".", value_separator="-") {
  # Strip training "/"s
  p <- str_remove(path, "/$")
  p %<>% str_split(pattern = fixed("/")) %>% extract2(1) %>% tail(1)
  p %<>% str_split(pattern = fixed(parameter_separator), simplify = FALSE) %>% extract2(1)
  p %<>% str_split(pattern = fixed(value_separator))
  p %<>% sapply(FUN = function(x) list(x[2]) %>% set_names(x[1]))
  p <- do.call(list, p)

  return(p)
}

#' Form data.frame from imputer objects that have timing attribute stored
#'
#' @param imputers List of imputer objects
#' @param times_imputed Number of times imputed
#'
#' @return data.frame with timing information extracted
#' @export
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
