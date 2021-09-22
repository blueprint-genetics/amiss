#' Step 01: parse annotated ClinVar VCF and CADD annotation files
#'
#' This step parses VEP-annotated VCF data into a matrix of values and merges
#' that matrix with additional annotations obtained from CADD's annotation
#' files.
#'
#' Parameters used by this step (presented via constant names; see
#' R/parameters.R for explicit string values):
#' - TRANSCRIPT, which determines whether or not only canonical transcripts
#' are retained
#' - CLASSIFICATION_QUALITY, which determines the lowest ClinVar quality tier
#' for which variants are retained
#' - RESTRICTION_MISSENSE, which determines whether only missense variants are
#' allowed
#'
#' @param vcf_filename Path to VEP-annotated ClinVar VCF
#' @param cadd_snv_filename Path to CADD SNV file
#' @param cadd_indel_filename Path to CADD indel file
#' @param output_root_dir Path to root output directory, in which the directory
#'   containing script outputs will be created
#' @param parameters List mapping parameter names to parameter values
#' @param parameters_path Path to file defining parameter combination to use if `parameters` is NULL
#'
#' @return Path to directory in which output files were written
#' @export
S01_parse_vcf <- function(
  vcf_filename,
  cadd_snv_filename,
  cadd_indel_filename,
  output_root_dir,
  parameters=NULL,
  parameters_path=NULL
) {
  futile.logger::flog.threshold(futile.logger::DEBUG)

  set.seed(10)

  output_path <- file.path(path.expand(output_root_dir))
  output_data_path <- file.path(output_path, "data")
  dir.create(output_path)
  dir.create(output_data_path)

  if (!file.exists(vcf_filename))
    stop(paste("Input VCF file", vcf_filename, "does not exist. Stopping."))
  if (!file.exists(cadd_snv_filename))
    stop(paste("Input CADD SNV annotation file", cadd_snv_filename, "does not exist. Stopping."))
  if (!file.exists(cadd_indel_filename))
    stop(paste("Input CADD indel annotation file", cadd_indel_filename, "does not exist. Stopping."))

  if(is.null(parameters)) {
    config <- get_config(parameters_path)
  } else {
    config <- parameters
  }
  parameter_dependent_path <- file.path(output_path, generate_file_prefix(config))
  dir.create(parameter_dependent_path)
  
  vcf <- vcfR::read.vcfR(vcf_filename)
  
  vep_filters <- c()
  info_filters <- c()
  
  # Transcript selection parameters
  if (config[[TRANSCRIPT]] == TRANSCRIPT_CANONICAL) {
     vep_filters <- c(vep_filters, canonical)
  } else if (config[[TRANSCRIPT]] == TRANSCRIPT_KEEP_ALL) {
     # Do nothing
  } else stop(
    paste0("Unknown value \"", config[[TRANSCRIPT]], "\" for parameter \"", TRANSCRIPT, "\"")
  )
  
  # Data quality parameters
  if (config[[CLASSIFICATION_QUALITY]] == CLASSIFICATION_QUALITY_CLINGEN) {
    info_filters <- c(info_filters, clingen)
  } else if (config[[CLASSIFICATION_QUALITY]] == CLASSIFICATION_QUALITY_TWOSTAR) {
    info_filters <- c(info_filters, twostar)
  } else if (config[[CLASSIFICATION_QUALITY]] == CLASSIFICATION_QUALITY_ONESTAR) {
    info_filters <- c(info_filters, onestar)
  } else stop(
    paste0("Unknown value \"", config[[CLASSIFICATION_QUALITY]], "\" for parameter \"", CLASSIFICATION_QUALITY, "\"")
  )
  
  if (config[[RESTRICTION_MISSENSE]] == MISSENSE_ONLY) {
    vep_filters <- c(vep_filters, missense)
  } else if (config[[RESTRICTION_MISSENSE]] == ALL) {
     # Do nothing
  } else stop(
    paste0("Unknown value \"", config[[RESTRICTION_MISSENSE]], "\" for parameter \"", RESTRICTION_MISSENSE, "\"")
  )
  
  vcf_df <- vcf_object_to_dataframe(vcf, num_batches = 100, info_filters = info_filters, vep_filters = vep_filters)
  vcf_df <- vcf_df[vcf_df$CLNSIG != "drug_response", ]
  stopifnot(all(vcf_df$Feature == vcf_df$Ensembl_transcriptid, na.rm = TRUE))

  #write.csv(vcf_df, file.path(parameter_dependent_path, FILE_FULL_CLINGEN_CSV))

  cadd_snv_data <- read.delim(cadd_snv_filename, skip = 1, as.is = TRUE)
  cadd_indel_data <- read.delim(cadd_indel_filename, skip = 1, as.is = TRUE)

  stopifnot(colnames(cadd_snv_data) == colnames(cadd_indel_data))

  cadd_data <- rbind(cadd_snv_data, cadd_indel_data)

  merged_data <- merge(x = cadd_data,
                       y = vcf_df,
                       all = FALSE,
                       by.x = c("X.Chrom", "Pos", "Ref", "Alt", "FeatureID"),
                       by.y = c("CHROM", "POS", "REF", "ALT", "Feature"))

  write.csv(file = file.path(parameter_dependent_path, FILE_MERGED_DATA_CSV), x = merged_data, row.names = FALSE)

  write(capture.output(sessionInfo()), file.path(parameter_dependent_path, "01_parse_vcf_sessioninfo.txt"))

  return(parameter_dependent_path)
}
