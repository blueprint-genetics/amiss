
missense <- function(variant) {
  
  stopifnot(class(variant) == "data.frame")# nrow(variant)  == 1)
  
  variant[, "Consequence", drop = TRUE] == "missense_variant"
}

clingen <- function(variant) {
  
  stopifnot(class(variant) == "data.frame")#, nrow(variant)  == 1) 
  
  variant[, "CLNREVSTAT", drop = TRUE] %in% c("reviewed_by_expert_panel", "practice_guideline")
}



apply_filters <- function(filters, df) {
  
  filter_vectors <- lapply(X = filters, FUN = function(x) { x(df) })
  
  if (length(filters) > 1) { 
    combined_filter <- Reduce(f = `&`, filter_vectors)
  } 
  else {
    combined_filter <- unlist(filter_vectors)
  }
  
  df <- df[combined_filter, ]
  
  return(df)
}