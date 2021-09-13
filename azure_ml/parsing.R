#library(azuremlsdk)
#library(optparse)
library(here)
library(amiss)

## Parameters --------------------------------------------------------------

args <- commandArgs(trailingOnly = TRUE)
cat(args)

data_folder <- as.character(args[2])

quality <- as.character(args[10])
log_metric_to_run("quality", quality)

restriction <- as.character(args[12])
log_metric_to_run("restriction", restriction)

transcript <- as.character(args[14])
log_metric_to_run("transcript", transcript)

set.seed(10)

dir.create(here("outputs"))
dir.create(here("outputs", "data"))

## Unzip VCF file if it's in .gz format
vcf_filename <- here(data_folder, as.character(args[4]))

if (endsWith(vcf_filename, ".gz")){
  system(paste0("gunzip ", vcf_filename))
  vcf_filename <- sub(".gz", "", vcf_filename)
}
cadd_snv_filename <- here(data_folder, as.character(args[6]))
cadd_indel_filename <- here(data_folder, as.character(args[8]))

## Check that files exist
if (!file.exists(vcf_filename))
  stop(paste("Input VCF file", vcf_filename, "does not exist. Stopping."))
if (!file.exists(cadd_snv_filename))
  stop(paste("Input CADD SNV annotation file", cadd_snv_filename, "does not exist. Stopping."))
if (!file.exists(cadd_indel_filename))
  stop(paste("Input CADD indel annotation file", cadd_indel_filename, "does not exist. Stopping."))

S01_parse_vcf(
  vcf_filename,
  cadd_snv_filename,
  cadd_indel_filename,
  parameters = list(quality = quality, restriction = restriction, transcript = transcript),
  output_root_dir = here("outputs")
)

log_metric_to_run("dummy", 1)
