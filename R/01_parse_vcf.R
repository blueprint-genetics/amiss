#' Step 01: parse annotated ClinVar VCF and CADD annotation files
#'
#' @param vcf_filename
#' @param cadd_snv_filename
#' @param cadd_indel_filename
#' @param output_root_dir
#' @param parameters
#' @param parameters_path
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
#' @param parameters_path Path to file defining parameter combination to use
#' @param output_root_dir Path to root output directory, in which the directory
#'   containing script outputs will be created
#'
#' @return Path to directory in which output files were written
#' @export
S01_parse_vcf <- function(
  vcf_filename,
  cadd_snv_filename,
  cadd_indel_filename,
  output_root_dir,
  logs_dir = normalizePath("logs", mustWork = FALSE),
  parameters=NULL,
  parameters_path=NULL
) {

  ### Setup ###
  output_path <- normalizePath(output_root_dir, mustWork = FALSE)
  create_dir(output_path)
  create_dir(logs_dir)

  futile.logger::flog.appender(futile.logger::appender.tee(file.path(logs_dir, "01_parse_vcf.log")))
  futile.logger::flog.threshold(futile.logger::DEBUG)
  futile.logger::flog.info("START 01_parse_vcf.R")
  futile.logger::flog.info("OUTPUT Output root path set to %s", output_path)

  seed <- 10
  set.seed(seed)
  futile.logger::flog.info("DESIGN_CHOICE Setting seed to %d", seed)

  ### Input path checks ###
  if (!file.exists(vcf_filename))
    stop(paste("Input VCF file", vcf_filename, "does not exist. Stopping."))
  if (!file.exists(cadd_snv_filename))
    stop(paste("Input CADD SNV annotation file", cadd_snv_filename, "does not exist. Stopping."))
  if (!file.exists(cadd_indel_filename))
    stop(paste("Input CADD indel annotation file", cadd_indel_filename, "does not exist. Stopping."))

  ### Parameter processing ###
  if (is.null(parameters)) {
    parameters_path <- normalizePath(parameters_path)
    futile.logger::flog.info("INPUT Reading parameters from JSON file at %s", parameters_path)
    config <- get_config(parameters_path)
  } else {
    config <- parameters
  }

  if (config[[TRANSCRIPT]] %>% is.null) {
    stop("Required parameter \"" %>% paste0(TRANSCRIPT, "\" not provided"))
  }
  if (config[[CLASSIFICATION_QUALITY]] %>% is.null) {
    stop("Required parameter \"" %>% paste0(CLASSIFICATION_QUALITY, "\" not provided"))
  }
  if (config[[RESTRICTION_MISSENSE]] %>% is.null) {
    stop("Required parameter \"" %>% paste0(RESTRICTION_MISSENSE, "\" not provided"))
  }

  ### Define output path ###
  parameter_dependent_path <- file.path(output_path, generate_parameter_dependent_name(config))
  dir.create(parameter_dependent_path)
  futile.logger::flog.info("OUTPUT Parameter dependent path set to %s", parameter_dependent_path)

  ### Read input ###
  vcf_filename <- normalizePath(vcf_filename)
  futile.logger::flog.info("INPUT Reading annotated ClinVar variant data from VCF file at %s", vcf_filename)
  vcf <- vcfR::read.vcfR(vcf_filename)

  vep_filters <- c()
  info_filters <- c()

  # Transcript selection parameters
  futile.logger::flog.info("PARAMETER %s = %s", TRANSCRIPT, config[[TRANSCRIPT]])
  if (config[[TRANSCRIPT]] == TRANSCRIPT_CANONICAL) {
     vep_filters <- c(vep_filters, canonical)
  } else if (config[[TRANSCRIPT]] == TRANSCRIPT_KEEP_ALL) {
     # Do nothing
  } else stop(
    paste0("Unknown value \"", config[[TRANSCRIPT]], "\" for parameter \"", TRANSCRIPT, "\"")
  )

  # Data quality parameters
  futile.logger::flog.info("PARAMETER %s = %s", CLASSIFICATION_QUALITY, config[[CLASSIFICATION_QUALITY]])
  if (config[[CLASSIFICATION_QUALITY]] == CLASSIFICATION_QUALITY_CLINGEN) {
    info_filters <- c(info_filters, clingen)
  } else if (config[[CLASSIFICATION_QUALITY]] == CLASSIFICATION_QUALITY_TWOSTAR) {
    info_filters <- c(info_filters, twostar)
  } else if (config[[CLASSIFICATION_QUALITY]] == CLASSIFICATION_QUALITY_ONESTAR) {
    info_filters <- c(info_filters, onestar)
  } else stop(
    paste0("Unknown value \"", config[[CLASSIFICATION_QUALITY]], "\" for parameter \"", CLASSIFICATION_QUALITY, "\"")
  )

  # Missense restriction parameter
  futile.logger::flog.info("PARAMETER %s = %s", RESTRICTION_MISSENSE, config[[RESTRICTION_MISSENSE]])
  if (config[[RESTRICTION_MISSENSE]] == MISSENSE_ONLY) {
    vep_filters <- c(vep_filters, missense)
  } else if (config[[RESTRICTION_MISSENSE]] == ALL) {
     # Do nothing
  } else stop(
    paste0("Unknown value \"", config[[RESTRICTION_MISSENSE]], "\" for parameter \"", RESTRICTION_MISSENSE, "\"")
  )

  ### VCF data parsing ###
  futile.logger::flog.info("PROGRESS Processing VCF to a data.frame")
  vcf_df <- vcf_object_to_dataframe(vcf, num_batches = 100, info_filters = info_filters, vep_filters = vep_filters)
  futile.logger::flog.info("DESIGN_CHOICE Dropping variants with classification \"drug_response\"")
  vcf_df <- vcf_df[vcf_df$CLNSIG != "drug_response", ]
  stopifnot(all(vcf_df$Feature == vcf_df$Ensembl_transcriptid, na.rm = TRUE))

  ### Reading CADD data ###
  cadd_snv_filename <- normalizePath(cadd_snv_filename)
  futile.logger::flog.info("INPUT Reading CADD SNV data from delimited file at %s", cadd_snv_filename)
  cadd_snv_data <- read.delim(cadd_snv_filename, skip = 1, as.is = TRUE)
  cadd_indel_filename <- normalizePath(cadd_indel_filename)
  futile.logger::flog.info("INPUT Reading CADD indel data from delimited file at %s", cadd_indel_filename)
  cadd_indel_data <- read.delim(cadd_indel_filename, skip = 1, as.is = TRUE)

  futile.logger::flog.info("INPUT Checking that CADD files have matching columns")
  stopifnot(colnames(cadd_snv_data) == colnames(cadd_indel_data))

  ### Merging CADD data ###
  cadd_data <- rbind(cadd_snv_data, cadd_indel_data)

  futile.logger::flog.info("PROGRESS Merging CADD data to ClinVar variant data", cadd_indel_filename)
  merged_data <- merge(x = cadd_data,
                       y = vcf_df,
                       all = FALSE,
                       by.x = c("X.Chrom", "Pos", "Ref", "Alt", "FeatureID"),
                       by.y = c("CHROM", "POS", "REF", "ALT", "Feature"))

  ### Writing output ###
  merged_data_output_path <- file.path(parameter_dependent_path, FILE_MERGED_DATA_CSV)
  futile.logger::flog.info("OUTPUT Writing merged data to delimited file at %s", merged_data_output_path)
  write.csv(file = merged_data_output_path, x = merged_data, row.names = FALSE)

  sessioninfo_path <- file.path(parameter_dependent_path, "01_parse_vcf_sessioninfo.txt")
  futile.logger::flog.info("OUTPUT Writing session information to text file at %s", sessioninfo_path)
  write(capture.output(sessionInfo()), sessioninfo_path)

  futile.logger::flog.info("DONE 01_parse_vcf.R")
  return(parameter_dependent_path)
}
