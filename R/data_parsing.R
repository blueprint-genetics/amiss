library(tidyr)
library(stringr)
library(vcfR)

source("R/utils.R")

#' Spread data stored in key-value pairs in the INFO column
#' 
#' Spread data stored in key-value pairs in the INFO column
#' so that every key is represented by a column containing the values.
#'
#' @param vcf A `vcfR` object.
#' @param row_indices The rows for which spreading should be done. Useful for running data preprocessing in batches to conserve memory.
#'
#' @return A data.frame otherwise similar to `vcf@fix`, but with the INFO column spread as described.
#'
spread_info_column <- function(vcf, row_indices) {
  # TODO: check for existence of multiple ALT alleles
  
  if (is.null(row_indices))
    row_indices <- 1:nrow(vcf@fix)
    
  stopifnot(class(vcf) == "vcfR")
  
  # Separate locus and substitution from annotation data 
  idfiers <- data.frame(vcf@fix[row_indices, c("CHROM", "POS", "REF", "ALT"), drop = FALSE], stringsAsFactors = FALSE)
  info <- vcf@fix[row_indices, "INFO", drop = TRUE] 
  
  # The INFO column consists of key-value pairs separated by semicolons
  info_split <- stringr::str_split(string = info, pattern = fixed(";"))
  
  # Split strings `key=value` into pairs c(key, value) (i.e. character vectors of length 2)
  info_key_value_pair_lists <- lapply(X = info_split, 
                                      FUN = function(x) {
                                        stringr::str_split_fixed(string = x, pattern = fixed('='), n = 2)
                                      })
  
  # Form 2-column data.frames from the pairs in order to use tidyr::spread
  info_key_value_pair_lists <- lapply(X = info_key_value_pair_lists,
                                      FUN = function(x) {
                                        x <- data.frame(x, stringsAsFactors = FALSE)
                                        colnames(x) <- c("key", "value")
                                        return(x)
                                      })
  
  # Find all INFO keywords that appear in the dataset
  keys <- get_info_keys(vcf)
  
  # "Spread" the data from key-value pairs, i.e. transform from narrow to wide data
  info_key_value_pair_lists <- lapply(X = info_key_value_pair_lists,
                                      FUN = function(x)
                                        tidyr::spread(
                                          x,
                                          key = "key",
                                          value = "value",
                                          convert = TRUE
                                        ))

  # Are all just one observation, as they should be? 
  stopifnot(all(sapply(X = info_key_value_pair_lists, FUN = function(x) nrow(x) == 1)))
  
  row_idx = seq_along(info_key_value_pair_lists)
  
  # Combine the locus and substitution information with wide INFO data
  info_w_ids <- lapply(X = row_idx,
                       FUN = function(row_id) {
                         cbind(
                           idfiers[row_id, , drop = FALSE],
                           info_key_value_pair_lists[[row_id]]
                         )
                       })
  
  # Put NA in columns for which there was no key-value pair for this variant
  info_w_ids <- lapply(X = info_w_ids,
                       FUN = function(x) 
                       {
                         missing_cols <- setdiff(x = c("CHROM", "POS", "ALT", "REF", keys), y = colnames(x))
                         x[, missing_cols] <- NA
                         x
                       })
  
  # Combine the single-row data.frames into a single data.frame
  variant_data <- do.call(rbind, info_w_ids)
  
  return(variant_data)
  
}

#' Get INFO keys
#' 
#' Parses the VCF metadata to find a list of possible keys that 
#' may occur in the INFO column.
#'
#' @param vcf A `vcfR` object
#'
#' @return List of keys that may occur in INFO
get_info_keys <- function(vcf) {
  
  stopifnot(class(vcf) == "vcfR")
  
  info_metadata <- vcfR::queryMETA(vcf, "INFO", nice = TRUE)
  keys <- lapply(X = info_metadata, FUN = function(x) x[1]) %>% unlist
  keys <- stringr::str_split(string = keys, pattern = fixed("INFO=ID="), n = 2)
  keys <- vapply(X = keys, FUN = function(x) x[2], FUN.VALUE = character(1))
  
  return(keys)
}

#' Get VEP field names
#' 
#' Parses the (VEP-annotated) VCF's metadata to find field names 
#' for fields added by VEP.
#'
#' @param vcf A `vcfR` object
#'
#' @return (Ordered) list of field names added by VEP
get_vep_field_names <- function(vcf) {
  
  stopifnot(class(vcf) == "vcfR")
  
  # Get CSQ keys from metadata
  csq_keys <- vcfR::queryMETA(vcf, 'CSQ', nice = TRUE)[[1]][4]
  # Drop the part of the string before the format definition
  csq_keys <- stringr::str_split(csq_keys, fixed("Format: "))[[1]][2] 
  # Produce character vector 
  csq_keys <- stringr::str_split(csq_keys, fixed("|"))[[1]]
  
  return(csq_keys)
}

#' Split VEP fields into their own columns and divides multi-transcript rows into multiple single-transcript rows
#' 
#' VEP stores its output in the `CSQ`-key in INFO.
#' The values are separated by `|` (vertical pipe character).
#' This function splits the values so that each field value on 
#' each row is stored in the column created for that field.
#' 
#' If there are multiple transcripts for the variant, each transcript 
#' has its values stored in CSQ so that each field is recorded 
#' as usual sequentially delimited by `|`, but then `,` delimits the
#' value strings that match each transcript.
#' 
#' This function duplicates rows so that every row has a single transcript.
#'
#' @param variant_dataframe A data.frame with a CSQ-column containing values output by VEP.
#' @param vep_field_names Ordered list of field names that match the values in the CSQ column.
#'
#' @return A data.frame 
split_vep_fields <- function(variant_dataframe, vep_field_names) {
  
  stopifnot(class(variant_dataframe) == "data.frame",
            class(vep_field_names)   == "character")
  
  # Split by transcript
  vep_data <- lapply(X = variant_dataframe[, "CSQ", drop = TRUE],
                     FUN = . %>% str_split(string = ., pattern = ',', n = Inf) %>% unlist)
  variant_dataframe$CSQ <- NULL
  
  transcript_nums <- sapply(X = vep_data, FUN = length)
  
  # Check that transcripts are unique
  vep_data <- lapply(X = vep_data, FUN = unique)
  unique_transcript_nums <- sapply(X = vep_data, FUN = length)
  stopifnot(transcript_nums == unique_transcript_nums)
  
  # Replicate rows so that each transcript gets its own row
  variant_dataframe <- variant_dataframe[rep(1:nrow(variant_dataframe), times = unlist(transcript_nums)), ]
  
  vep_data <- stringr::str_split_fixed(vep_data %>% unlist, fixed("|"), n = length(vep_field_names))
  colnames(vep_data) <- vep_field_names
  
  variant_dataframe <- data.frame(variant_dataframe, vep_data, stringsAsFactors = FALSE)
  
  return(variant_dataframe)

}

#' Split and match transcript-specific values from dbNSFP to the correct transcripts
#'
#' dbNSFP stores certain tool scores so that transcript-specific values are delimited
#' with `;` (which is converted into `&` by VEP) and ordered to match the Ensembl 
#' transcript ids stored in the `Ensembl_transcriptid` column.
#' 
#' This function matches transcript ids produced by VEP (in the `Feature` column) 
#' to those from dbNSFP (in `Ensembl_transcriptid` column), and then chooses for each `&`-delimited 
#' column and every row a single value matching the row's transcript id.
#'
#' @param variant_dataframe A data.frame containing the columns 
#'
#' @return a data.frame with the `&`-delimited columns processed 
#' to contain the value matching the row's transcript id.
split_dbnsfp_values <- function(variant_dataframe) {
  
  old_df <- variant_dataframe
  stopifnot(class(variant_dataframe) == "data.frame")
  
  process_columns <- c(
    "Ensembl_transcriptid", 
    "Ensembl_proteinid",
    "aapos",
    "SIFT_score",
    "SIFT_pred",
    "MutationTaster_score", # NOTE: transcripts may not match those from other tools
    "MutationTaster_pred",
    "FATHMM_score",
    "FATHMM_pred",
    "PROVEAN_score",
    "PROVEAN_pred"
  )
  
  # "&"-splits.
  # Note that the output is a list that contains columns (as lists) that contains rows, 
  # each of which is now a vector, since it may contain multiple values that were separated by '&'.
  # etsplits = list( col1 = list( row_1 = c("val1", "val2", ...), ... ), ... )
  etsplits <- lapply(X = variant_dataframe[, process_columns, drop = FALSE], 
                     FUN = function(x) stringr::str_split(string = x, pattern = fixed("&")))
  
  # For each row, try to match the transcript given by VEP (Feature-column)
  # to the vector of transcripts given by dbNSFP (Ensembl_transcriptid-column).
  # Output is a list whose elements are vectors that contain every match for the row.
  match_indices <- mapply(FUN = match,
                          x = variant_dataframe$Feature,
                          table = etsplits$Ensembl_transcriptid)
  
  # Next we pick a value matching the transcript for each of the columns, of course selecting
  # from the multiple values that were stored in dbNSFP.
  # There is an additional complication that stops us from simply picking the value purely by the index:
  # sometimes there is exactly one value, even though there are multiple transcripts. For MutationTaster,
  # this is somewhat expected, since its transcripts do not necessarily match rest of dbNSFP. For other tools,
  # this is probably meant to imply that the value is the same for each transcript. In such instances,
  # we must return the first (and only) value regardless of chosen transcript (otherwise we would return
  # a missing value). In addition, we must guard against picking a value at all when no transcript match has
  # been found.
  pick_value_for_transcript <- function(x, i) {
    if (length(x) == 1 && !is.na(i)) return(x[1])
    else return(x[i])
  }

  picked_values <- lapply(X = etsplits, 
                          FUN = function(column) 
                            mapply(
                              FUN = pick_value_for_transcript,
                              x = column,
                              i = match_indices,
                              SIMPLIFY = TRUE
                            )
                          )
  
  variant_dataframe[, process_columns] <- picked_values
  return(variant_dataframe)
  
}

#' Process data contained in a vcfR object into a data.frame usable for downstream analysis
#' 
#' This function takes a vcfR object and produces a data.frame 
#' that should be then directly usable for downstream analysis.
#' 
#' The function can be given two lists of filters in order to remove variants or transcripts
#' during the processing, so that memory and CPU time are not wasted on variants that would 
#' be filtered out later. 
#' 
#' The lists are distinguished by the phase of processing that they are applied in: 
#' `info_filters` are applied after INFO fields have been split (see documentation 
#' for `spread_info_column`), and `vep_filters` #' are applied later, after VEP fields have 
#' been split (see documentation for `split_vep_fields`).
#' 
#' The processing can be done in "batches", so that only a small portion of the data is 
#' processed at a time. This drastically reduces memory usage, since the full, unfiltered data
#' does not need to be kept in memory at any point.
#'
#' @param vcf A `vcfR` object
#' @param num_batches Number of batches to run the processing in
#' @param info_filters List of filter functions to apply to the rows
#' @param vep_filters List of filter functions to apply to the rows
#'
#' @return
vcf_object_to_dataframe <- function(vcf, num_batches = 100, info_filters = NULL, vep_filters = NULL) {

  # Form batches of row indices so that variants are split into `num_batches` equally-sized sets
  num_rows <- nrow(vcf@fix)
  batches <- partition_rows_into_batches(num_rows, num_batches)
  
  # Do processing batch-wise so that the full data is not expanded in memory before filtering
  # This should be trivial to parallelize
  process_batch <- function(batch, batch_i) {

    flog.debug("Batch " %>% paste0(batch_i))
    
    batch_df <- spread_info_column(vcf, row_indices = batch)
  
    if (!is.null(info_filters)) {
      batch_df <- apply_filters(info_filters, batch_df)
    }
    
    if (!nrow(batch_df) > 0) return(NULL)
    
    batch_df <- split_vep_fields(batch_df, get_vep_field_names(vcf))
    
    if (!is.null(vep_filters)) {
      batch_df <- apply_filters(vep_filters, batch_df)
    }
    
    if (!nrow(batch_df) > 0) return(NULL)
    
    batch_df <- split_dbnsfp_values(batch_df)
    
    if (!nrow(batch_df) > 0) return(NULL)
    
    return(batch_df)
    
  }

  batch_df_list <- mapply(FUN = process_batch, batch = batches, batch_i = 1:length(batches), SIMPLIFY = FALSE)
  
  # Remove NULLs from list
  batch_df_list <- batch_df_list[sapply(batch_df_list, function(x) !is.null(x))]

  # Combine into one data.frame
  vcf_df <- do.call(rbind, batch_df_list)
  
  vcf_df[vcf_df == ""] <- NA 
  vcf_df[vcf_df == "."] <- NA 
  
  vcf_df <- type.convert(x = vcf_df, as.is = TRUE, numerals = "allow.loss") # TODO: can the precision loss be avoided?
  
  return(vcf_df)
  
}
