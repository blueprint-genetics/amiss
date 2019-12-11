library(magrittr)
library(ggplot2)
library(mice)
library(foreach)
library(doParallel)
library(doRNG)

source("R/simulation_definitions.R")

registerDoParallel(3)
seed <- 42

training_data <- read.csv("contracted_training_data.csv", as.is = TRUE, row.names = 1)

# Temporary, to make simulations take reasonable time when testing
repeats <- 3
ampute_params <- ampute_params[sample(1:NROW(ampute_params), 10), ]

directories <- sapply(1:repeats, function(i) {
  paste0("output/simulated_data/repeat_", i, "/")
})

dir_creation_success <- sapply(directories, function(d) {
  dir.create(d, showWarnings = TRUE, recursive = TRUE)
})
if (!all(dir_creation_success)) {
  stop("Failed to create director(y/ies) for saving output.")
}

# TODO: informative missingness simulation

ampute_per_pattern <- function(data, ...) {
  missingness_patterns <- data %>% is.na
  missingness_pattern_factor <- apply(missingness_patterns, MARGIN = 1, function(x) paste0(as.integer(x), collapse = "")) %>% factor
  data_per_mp <- split(data, missingness_pattern_factor)
  data_per_mp_amputed <- lapply(data_per_mp, function(td) {
    mp <- is.na(td[1,, drop = TRUE])
    complete_section <- td[, !mp, drop = FALSE]
    amp <- ampute(complete_section, std = FALSE)
    td[, !mp] <- amp$amp
    return(td)
  })
  amputed_data <- unsplit(data_per_mp_amputed, missingness_pattern_factor)
  return(amputed_data)
}

repetitions <- foreach(r = 1:repeats, .options.RNG = seed) %dorng% {
  for (params_i in 1:NROW(ampute_params)) {
    d <- do.call(
      ampute_per_pattern,
      c(list(training_data),
        ampute_params[params_i, , drop = TRUE])
    )
    # Save the files immediately to avoid aggregating them uselessly in memory
    attr(d, "params") <- ampute_params[params_i, , drop = TRUE]
    filename <- paste0(directories[[r]], paste0(attr(d, "params"), collapse = "_"), ".RDS")
    saveRDS(d, file = filename)
  }
  return(NULL)
}
