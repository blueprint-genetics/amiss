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
  amputs <- foreach(params = 1:NROW(ampute_params)) %do% {
    do.call(ampute_per_pattern, 
            c(list(training_data), 
              ampute_params[params, , drop = TRUE])
    )
  }
}

if (!dir.exists("output/simulated_data")) {
  dir_creation_success <- dir.create("output/simulated_data", showWarnings = TRUE)
  if (!dir_creation_success) {
    stop("Failed to create directory for saving output.")
  }
}
for (i in seq_along(amputs)) {
  attr(amputs[[i]], "params") <- ampute_params[i, , drop = TRUE]
  filename <- paste0("output/simulated_data/", paste0(attr(amputs[[i]], "params"), collapse = "_"), ".RDS")
  saveRDS(amputs[[i]], filename)
}
