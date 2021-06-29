
#' Check if variants are missense
#'
#' @param variant A data.frame containing VEP-annotated variants (must have a "Consequence"-column)
#'
#' @return A logical vector with as many values as rows in `variant`. Each value is `TRUE` 
#' if "missense_variant" appears in the Consequence field, and `FALSE` otherwise.
missense <- function(variant) {
  
  stopifnot(class(variant) == "data.frame")
  
  grepl("missense_variant", variant[, "Consequence", drop = TRUE], fixed = TRUE)
}

#' Check whether variants are ClinGen-reviewed
#'
#' @param variant A data.frame containing VEP-annotated variants (must have a "CLNREVSTAT"-column)
#'
#' @return A logical vector with as many values as rows in `variant`. Each value is `TRUE` 
#' if the CLNREVSTAT field is either "reviewed_by_expert_panel" or "practice_guideline", 
#' and `FALSE` otherwise.
clingen <- function(variant) {
  
  stopifnot(class(variant) == "data.frame")
  
  variant[, "CLNREVSTAT", drop = TRUE] %in% c("reviewed_by_expert_panel", "practice_guideline")
}

#' Check whether variants have at least two stars in ClinVar
#'
#' @param variant A data.frame containing VEP-annotated variants (must have a "CLNREVSTAT"-column)
#'
#' @return A logical vector with as many values as rows in `variant`. Each value is `TRUE` 
#' if the CLNREVSTAT field is either "criteria_provided,_multiple_submitters,_no_conflicts"
#' or the variant passes the `clingen` filter, and `FALSE` otherwise.
twostar <- function(variant) {
  
  stopifnot(class(variant) == "data.frame")
  
  (variant[, "CLNREVSTAT", drop = TRUE] == c("criteria_provided,_multiple_submitters,_no_conflicts")) | clingen(variant)
}

#' Check whether variants have at least one star in ClinVar (not allowing conflicting classifications)
#'
#' @param variant A data.frame containing VEP-annotated variants (must have a "CLNREVSTAT"-column)
#'
#' @return A logical vector with as many values as rows in `variant`. Each value is `TRUE` 
#' if the CLNREVSTAT field is either "criteria_provided,_single_submitter"
#' or the variant passes the `twostar` filter, and `FALSE` otherwise.
onestar <- function(variant) {
  
  stopifnot(class(variant) == "data.frame")
  
  (variant[, "CLNREVSTAT", drop = TRUE] == c("criteria_provided,_single_submitter")) | twostar(variant)
}

#' Check whether transcripts are Ensembl-canonical
#'
#' @param variant A data.frame containing VEP-annotated variants (must have a "CANONICAL"-column)
#'
#' @return A logical vector with as many values as rows in `variant`. Each value is `TRUE` 
#' if the CANONICAL field is "YES", and `FALSE` otherwise.
canonical <- function(transcript) {
  
  stopifnot(class(transcript) == "data.frame")
  
  transcript[, "CANONICAL", drop = TRUE] == "YES"
}

#' Apply a set of filters to a data.frame
#'
#' @param filters List or vector of filters (functions that return 
#' a logical value for each row of a data.frame)
#' @param df A data.frame
#'
#' @return Subset of `df` that passes every filter
apply_filters <- function(filters, df) {
  
  # Compute outcome of every filter for df
  filter_vectors <- lapply(X = filters, FUN = function(x) { x(df) })
  
  # Compute AND pointwise over all filter results
  if (length(filters) > 1) { 
    combined_filter <- Reduce(f = `&`, filter_vectors)
  } 
  else {
    # If there is only one filter, remove the list-wrap
    combined_filter <- unlist(filter_vectors)
  }
  
  df <- df[combined_filter, ]
  
  return(df)
}
