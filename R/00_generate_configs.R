#' Script 00: generate parameter combination files
#'
#' @param spec_filename 
#' @param parameter_grid_filename 
#'
#' @return
#' @export
#'
#' @examples
S00_generate_configs <- function(spec_filename = "experiment_spec.json", parameter_grid_filename = "parameter_grid.json") {
  
  spec <- get_config(spec_filename)
  parameter_grid <- get_config(parameter_grid_filename)
  
  stopifnot("n_combinations" %in% names(spec))
  
  for (i in seq_len(spec$n_combinations)) {
    generate_parameter_combination(parameter_grid = parameter_grid) %>% write_config("combination_" %>% paste0(i,".json"))
  }
  
}
