library(tidyr)
library(stringr)
library(vcfR)
library(futile.logger)

spread_info_column <- function(vcf, row_limit=100) {
  # TODO: once code seems to work, remove row restriction
  # TODO: check for existence of multiple ALT alleles
  
  if (is.null(row_limit))
    row_limit <- nrow(vcf@fix)
    
  stopifnot(class(vcf) == "vcfR")
  
  # Separate locus and substitution from annotation data 
  idfiers <- data.frame(vcf@fix[1:row_limit, c("CHROM", "POS", "REF", "ALT")], stringsAsFactors = FALSE)
  #idfiers <- type.convert(idfiers, as.is = TRUE)
  info <- vcf@fix[1:row_limit, "INFO", drop = TRUE] 
  
  # The INFO column consists of key-value pairs separated by semicolons
  info_split <- stringr::str_split(string = info, pattern = fixed(";"))
  
  # Split strings `key=value` into pairs c(key, value) (i.e. character vectors of length 2)
  info_key_value_pair_lists <- lapply(X = info_split, 
                                      FUN = function(x) {
                                        stringr::str_split_fixed(string = x, pattern = fixed('='), n = 2)
                                      })
  
  # Form 2-column data.frames from the pairs in order to use tidyr::spread
  info_key_value_pair_lists <- lapply(info_key_value_pair_lists,
                                      function(x) {
                                        x <- data.frame(x, stringsAsFactors = FALSE)
                                        colnames(x) <- c("key", "value")
                                        return(x)
                                      })
  
  # Find all INFO keywords that appear in the dataset
  keys <- get_info_keys(vcf)
  
  # "Spread" the data from key-value pairs, i.e. transform from narrow to wide data
  info_key_value_pair_lists <- lapply(info_key_value_pair_lists,
                                      function(x)
                                        tidyr::spread(
                                          x,
                                          key = "key",
                                          value = "value",
                                          convert = TRUE
                                        ))

  # Are all just one observation, as they should be? 
  stopifnot(all(sapply(info_key_value_pair_lists, function(x) nrow(x) == 1)))
  
  row_idx = seq_along(info_key_value_pair_lists)
  
  # Combine the locus and substitution information with wide INFO data
  info_w_ids <- lapply(row_idx,
                       function(row_id) {
                         cbind(
                           idfiers[row_id, , drop = FALSE],
                           info_key_value_pair_lists[[row_id]]
                         )
                       })
  
  # Put NA in columns for which there was no key-value pair for this variant
  info_w_ids <- lapply(info_w_ids,
                       function(x) 
                       {
                         missing_cols <- setdiff(x = c("CHROM", "POS", "ALT", "REF", keys), y = colnames(x))
                         x[, missing_cols] <- NA
                         x
                       })
  
  # Combine the single-row data.frames into a single data.frame
  variant_data <- do.call(rbind, info_w_ids)
  
  return(variant_data)
  
}

get_info_keys <- function(vcf) {
  
  info_metadata <- vcfR::queryMETA(vcf, "INFO", nice = TRUE)
  keys <- lapply(info_metadata, function(x) x[1]) %>% unlist
  keys <- stringr::str_split(string = keys, pattern = fixed("INFO=ID="), n = 2)
  keys <- vapply(keys, function(x) x[2], character(1))
  
  return(keys)
}

get_vep_field_names <- function(vcf) {
  
  # Get CSQ keys from metadata
  csq_keys <- vcfR::queryMETA(vcf, 'CSQ', nice = TRUE)[[1]][4]
  # Drop the part of the string before the format definition
  csq_keys <- stringr::str_split(csq_keys, fixed("Format: "))[[1]][2] 
  # Produce character vector 
  csq_keys <- stringr::str_split(csq_keys, fixed("|"))[[1]]
  
  return(csq_keys)
}

split_vep_fields <- function(variant_dataframe, vep_field_names) {
  
  stopifnot(class(variant_dataframe) == "data.frame",
            class(vep_field_names)   == "character")
  
  # Split by transcript
  vep_data <- lapply(variant_dataframe[, "CSQ", drop = TRUE],
                     . %>% str_split(string = ., pattern = ',', n = Inf) %>% unlist)
  variant_dataframe$CSQ <- NULL
  
  transcript_nums <- lapply(vep_data, . %>% length)
  
  # Check that transcripts are unique
  vep_data <- lapply(vep_data, unique)
  unique_transcript_nums <- lapply(vep_data, . %>% length)
  stopifnot(mapply(FUN = function(x,y) x == y, transcript_nums, unique_transcript_nums))
  
  # Replicate rows so that each transcript gets its own row
  variant_dataframe <- variant_dataframe[rep(1:nrow(variant_dataframe), times = unlist(transcript_nums)), ]
  
  vep_data <- stringr::str_split_fixed(vep_data %>% unlist, fixed("|"), n = length(vep_field_names))
  colnames(vep_data) <- vep_field_names
  
  variant_dataframe <- data.frame(variant_dataframe, vep_data)
  
  return(variant_dataframe)

}

vcf_object_to_dataframe <- function(vcf, row_limit = 100, info_filters = NULL, vep_filters = NULL) {
  
  variant_df <- spread_info_column(vcf, row_limit = row_limit)

  
  if (!is.null(info_filters)) {
    variant_df <- apply_filters(info_filters, variant_df)
  }
  
  stopifnot(nrow(variant_df) > 0)
  
  variant_df <- split_vep_fields(variant_df, get_vep_field_names(vcf))
  
  if (!is.null(vep_filters)) {
    variant_df <- apply_filters(vep_filters, variant_df)
  }
  
  stopifnot(nrow(variant_df) > 0)
  
  variant_df <- type.convert(x = variant_df, as.is = TRUE, numerals = "allow.loss") # TODO: can the precision loss be avoided?
  
  return(variant_df)
  
}
