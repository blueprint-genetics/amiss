library(magrittr)
library(ggplot2)
library(mice)
library(foreach)
library(doParallel)
library(doRNG)

source("R/simulation_definitions.R")
source("R/feature_definitions.R")
source("R/utils.R")

registerDoParallel(3)
seed <- 42

training_data <- read.csv("contracted_training_data.csv", as.is = TRUE, row.names = 1)

# Temporary, to make simulations take reasonable time when testing
# repeats <- 3
# ampute_params <- ampute_params[sample(1:NROW(ampute_params), 10), ]

# Create directories for each repetition
directories <- sapply(1:repeats, function(i) {
  paste0("output/simulated_data/repeat_", i, "/")
})

dir_creation_success <- sapply(directories, function(d) {
  dir.create(d, showWarnings = TRUE, recursive = TRUE)
})

# TODO: informative missingness simulation

ampute_per_pattern <- function(data, ...) {

  # We have to make sure that all dummy variables generated from a categorical
  # variable are set missing when one is set missing.
  # For this purpose, we need to find the sets of dummies that were generated from
  # the same categorical variable.
  dummy_sets <- lapply(categorical_features, . %>% find_dummies(colnames(data)))

  # Divide the data into sets of rows with the same missingness pattern
  missingness_patterns <- data %>% is.na
  missingness_pattern_factor <- apply(missingness_patterns, MARGIN = 1, function(x) paste0(as.integer(x), collapse = "")) %>% factor
  data_per_mp <- split(data, missingness_pattern_factor)

  data_per_mp_amputed <- lapply(data_per_mp, function(td) {

    # Each dataset now has only completely observed and completely missing columns.
    # Select only the complete variables to run `ampute` on.
    mp <- is.na(td[1,, drop = TRUE])
    complete_section <- td[, !mp, drop = FALSE]

    # Next ensure co-occurrence of missingness over same-source dummy variables
    # Get the missingness patterns that ampute generates by default for this complete set
    dry_amp <- ampute(complete_section, std = FALSE, run = FALSE)
    pats <- dry_amp$patterns
    for (dummy_set in dummy_sets) {
      if (any(dummy_set %in% colnames(complete_section))) {
        # If one exists in this pattern, they all should. Any prior missingness
        # should already be present in each, as they are from the same variable.
        stopifnot(all(dummy_set %in% colnames(complete_section)))

        pats[,] <- apply(pats, MARGIN = 1, function(row) {
          # if any dummy variable from this set is to be missing,
          # set all the others to missing too
          if (any(row[dummy_set] == 0)) {
            row[dummy_set] <- 0
          }
          return(row)
        })
      }
    }

    # After the changes due to dummy variables, there are likely duplicate patterns.
    pats <- unique(pats)

    # Finally run `ampute`
    amp <- ampute(complete_section, patterns = pats, std = FALSE)
    td[, !mp] <- amp$amp
    return(td)
  })

  # Once all subset has been "amputed", collect them back into a single dataset
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
