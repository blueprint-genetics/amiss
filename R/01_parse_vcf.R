
#' Script 01: parse ClinVar VCF and CADD annotation files
#'
#' @param vcf_filename
#' @param cadd_snv_filename
#' @param cadd_indel_filename
#' @param output_root_dir
#'
#' @return
#' @export
S01_parse_vcf <- function(
  vcf_filename,
  cadd_snv_filename,
  cadd_indel_filename,
  output_root_dir
) {
  futile.logger::flog.threshold(futile.logger::DEBUG)

  set.seed(10)

  output_path <- file.path(path.expand(output_root_dir), "output")
  output_data_path <- file.path(output_path, "data")
  dir.create(output_path)
  dir.create(output_data_path)

  if (!file.exists(vcf_filename))
    stop(paste("Input VCF file", vcf_filename, "does not exist. Stopping."))
  if (!file.exists(cadd_snv_filename))
    stop(paste("Input CADD SNV annotation file", cadd_snv_filename, "does not exist. Stopping."))
  if (!file.exists(cadd_indel_filename))
    stop(paste("Input CADD indel annotation file", cadd_indel_filename, "does not exist. Stopping."))

  vcf <- vcfR::read.vcfR(vcf_filename)

  vcf_df <- vcf_object_to_dataframe(vcf, num_batches = 100, info_filters = c(clingen), vep_filters = c(canonical))
  vcf_df <- vcf_df[vcf_df$CLNSIG != "drug_response", ]

  stopifnot(all(vcf_df$Feature == vcf_df$Ensembl_transcriptid, na.rm = TRUE))

  write.csv(vcf_df, file.path(output_data_path, FILE_FULL_CLINGEN_CSV))

  cadd_snv_data <- read.delim(cadd_snv_filename, skip = 1, as.is = TRUE)
  cadd_indel_data <- read.delim(cadd_indel_filename, skip = 1, as.is = TRUE)

  stopifnot(colnames(cadd_snv_data) == colnames(cadd_indel_data))

  cadd_data <- rbind(cadd_snv_data, cadd_indel_data)

  merged_data <- merge(x = cadd_data,
                       y = vcf_df,
                       all = FALSE,
                       by.x = c("X.Chrom", "Pos", "Ref", "Alt", "FeatureID"),
                       by.y = c("CHROM", "POS", "REF", "ALT", "Feature"))

  write.csv(file = file.path(output_data_path, FILE_MERGED_DATA_CSV), x = merged_data, row.names = FALSE)

  write(capture.output(sessionInfo()), file.path(output_path, "01_parse_vcf_sessioninfo.txt"))

  return(output_data_path)
}
