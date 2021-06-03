library(here)
library(magrittr)

source(here("R", "configuration.R"))

cmd_args <- commandArgs(trailingOnly = TRUE)

spec_filename           <- "experiment_spec.json"
parameter_grid_filename <- "parameter_grid.json"

spec_state <- FALSE
grid_state <- FALSE
for (arg in cmd_args) {
  if (arg == "--spec") spec_state <- TRUE
  else if (arg == "--parameter_grid") grid_state <- TRUE
  else {
    if (spec_state) {
      spec_filename <- arg
      spec_state <- FALSE
    }
    if (grid_state) {
      parameter_grid_filename <- arg
      grid_state <- FALSE
    }
  }
}

spec           <- get_config(here(spec_filename))
parameter_grid <- get_config(here(parameter_grid_filename))

stopifnot("n_combinations" %in% names(spec))

for (i in seq_len(spec$n_combinations)) {
  generate_parameter_combination(parameter_grid = parameter_grid) %>% write_config("combination_" %>% paste0(i,".json"))
}
