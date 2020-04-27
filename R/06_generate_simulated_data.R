library(magrittr)
library(ggplot2)
library(mice)
library(foreach)
library(doParallel)
library(doRNG)
library(futile.logger)
library(digest)
library(here)

source(here("R", "constants.R"))
source(here("R", "simulation_definitions.R"))
source(here("R", "feature_definitions.R"))
source(here("R", "utils.R"))

cores <- get_env_cores()

flog.appender(appender.tee(here("output", "06_generate_simulated_data.log")), name = "simulation_logger")
flog.threshold(DEBUG, name = "simulation_logger")

flog.pid.info("Using %d cores", cores,  name = "simulation_logger")
registerDoParallel(cores)

seed <- 42
flog.pid.info("Using seed %d", seed,  name = "simulation_logger")
set.seed(seed)

training_data <- read.csv(here("output", "data", FILE_PREPROCESSED_TRAINING_DATA_CSV), as.is = TRUE, row.names = 1)

flog.pid.info("Repeating %d times", repeats,  name = "simulation_logger")
flog.pid.info("Simulations configuration:",  name = "simulation_logger")
flog.pid.info(capture.output(print(ampute_params)),  name = "simulation_logger")

flog.pid.info("Creating directories for each repetition",  name = "simulation_logger")
directories <- sapply(1:repeats, function(i) {
  here("sim", "simulated_data", paste0("repeat_", i))
})

dir_creation_success <- sapply(directories, function(d) {
  dir.create(d, showWarnings = TRUE, recursive = TRUE)
})

ampute_per_pattern <- function(data, prop, ...) {

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

    # To achieve a significant number of additional missing values, we need to assign more
    # missing variables in the pattern, as there is exactly one by default.
    generate_pattern_vector <- function(complete_section, prop) {
      missing <- runif(n = NCOL(complete_section)) > prop
      # If all end up missing, reassign one randomly back to observed.
      if (all(missing)) {
        missing[sample(1:length(missing), size = 1)] <- 1
      }
      v <- as.integer(missing)
      return(v)
    }

    pats <- replicate(generate_pattern_vector(complete_section, prop), n = NROW(complete_section)) %>% t
    colnames(pats) <- colnames(complete_section)

    # Let's not simulate any missing values in the dummy variables, in accordance with the
    # decision to use an additional level to mean "missing" in dummy variables.
    for (dummy_set in dummy_sets) {
      if (any(dummy_set %in% colnames(complete_section))) {
        # If one exists in this pattern, they all should. Any prior missingness
        # should already be present in each, as they are from the same variable.
        stopifnot(all(dummy_set %in% colnames(complete_section)))

        pats[,] <- apply(pats, MARGIN = 1, function(row) {
          row[dummy_set] <- 1
          return(row)
        })
      }
    }

    # After the changes due to dummy variables and the randomness, there are likely duplicate patterns.
    # We don't necessarily want missing values on all rows, so let's use the (inverse) proportion of rows that were
    # left completely observed to determine the proportion of rows where we want missingness.
    all_observed_pats <- apply(pats, MARGIN = 1, function(x) all(x == 1))
    prop_miss_rows <-  1 - (sum(all_observed_pats) / nrow(pats))
    pats <- unique(pats)

    # Finally run `ampute`
    if(!prop_miss_rows == 0)
      amp <- ampute(complete_section, patterns = pats, std = FALSE, bycases = TRUE, prop = prop_miss_rows, ...)
    else amp <- list(amp = complete_section)

    td[, !mp] <- amp$amp
    return(td)
  })

  # Once all subset has been "amputed", collect them back into a single dataset
  amputed_data <- unsplit(data_per_mp_amputed, missingness_pattern_factor)

  return(amputed_data)
}

repetitions <- foreach(r = 1:repeats, .options.RNG = seed) %dorng% {
  flog.pid.info("Starting repetition %d", r, name = "simulation_logger")
  flog.pid.info("seed fingerprint: %s", sha1(.Random.seed), name = "simulation_logger")
  filenames <- lapply(1:NROW(ampute_params), function(params_i) {
    flog.pid.info("Producing dataset with parameters:", name = "simulation_logger")
    flog.pid.info(capture.output(print(ampute_params[params_i,,drop = TRUE])), name = "simulation_logger")
    d <- do.call(
      ampute_per_pattern,
      c(data = list(training_data),
        ampute_params[params_i, , drop = TRUE])
    )

    # Save the files immediately to avoid aggregating them uselessly in memory
    param_vect <- ampute_params[params_i, , drop = TRUE]
    filename <- paste0(directories[[r]], "/", paste0(param_vect, collapse = "_"), ".csv")
    flog.pid.info("Writing file %s", filename, name = "simulation_logger")
    write.csv(x = d, file = filename)
    flog.pid.info("Wrote file %s", filename, name = "simulation_logger")
    return(filename)
  })
  flog.pid.info("Finishing repetition %d")
  return(filenames)
}

repetitions <- repetitions %>% unlist
write.csv(repetitions, file = here("output", "sim", FILE_SIMULATED_FILE_LIST_CSV))

write(capture.output(sessionInfo()), here("output", "06_generate_simulated_data_sessioninfo.txt"))
