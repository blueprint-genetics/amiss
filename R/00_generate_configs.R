#' Step 00: generate parameter combination files
#'
#' This step produces parameter combination files from a parameter grid
#' JSON file.
#' 
#' @param spec_path Path to file defining experiment parameters
#' @param parameter_grid_path Path to file defining the parameter grid
#' to sample from
#'
#' @return
#' @export
S00_generate_configs <- function(spec_path = "experiment_spec.json", parameter_grid_path = "parameter_grid.json") {
  
  spec <- get_config(spec_path)
  parameter_grid <- get_config(parameter_grid_path)
  
  stopifnot("n_combinations" %in% names(spec))
  
  for (i in seq_len(spec$n_combinations)) {
    generate_parameter_combination(parameter_grid = parameter_grid) %>% write_config("combination_" %>% paste0(i,".json"))
  }
  
}
