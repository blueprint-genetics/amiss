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
  
  combinations <- lapply(seq_len(spec$n_combinations), function(i) {
    generate_parameter_combination(parameter_grid = parameter_grid)
  })
  lapply(seq_len(spec$n_combinations), function(i) {
    combinations[[i]] %>% write_config("combination_" %>% paste0(i,".json"))
  }) %>% invisible
  
  return(combinations)
  
}
#' Script 00: parse data using all unique combinations
#'
#' @param spec_filename 
#' @param parameter_grid_filename 
#'
#' @return
#' @export
#'
#' @examples
S00_parse_all <- function(vcf_filename,
                          cadd_snv_filename,
                          cadd_indel_filename,
                          output_root_dir,
                          parameters_list) {
  parameters_list <- lapply(parameters_list, function(parameters) {
    parameters[names(parameters) %in% PREPROCESSING_PARAMETER_SUBSET]
  })
  parameters_list <- unique(parameters_list)
  lapply(parameters_list,
         function(parameters) {
           S01_parse_vcf(
             vcf_filename = vcf_filename,
             cadd_snv_filename = cadd_snv_filename,
             cadd_indel_filename = cadd_indel_filename,
             output_root_dir = output_root_dir,
             parameters = parameters
           )
         })
}
