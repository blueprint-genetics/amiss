library(azuremlsdk)
library(optparse)
library(here)
library(amiss)

## Parameters --------------------------------------------------------------

option_list <-
  list(
    make_option("--data_folder", action = "store"),
    make_option("--vcf_filename", action = "store"),
    make_option("--cadd_snv_filename", action = "store"),
    make_option("--cadd_indel_filename", action = "store"),
    make_option("--quality", action = "store"),
    make_option("--restriction", action = "store"),
    make_option("--transcript", action = "store")
  )
args <- parse_args(OptionParser(option_list))

cat(args)

data_folder <- as.character(args$data_folder)

quality <- as.character(args$quality)
log_metric_to_run("quality", quality)

restriction <- as.character(args$restriction)
log_metric_to_run("restriction", restriction)

transcript <- as.character(args$transcript)
log_metric_to_run("transcript", transcript)

set.seed(10)

dir.create(here("outputs"))
dir.create(here("outputs", "data"))

## Unzip VCF file if it's in .gz format
vcf_filename <- here(data_folder, as.character(args$vcf_filename))

if (endsWith(vcf_filename, ".gz")){
  system(paste0("gunzip ", vcf_filename))
  vcf_filename <- sub(".gz", "", vcf_filename)
}
cadd_snv_filename <- here(data_folder, as.character(args$cadd_snv_filename))
cadd_indel_filename <- here(data_folder, as.character(args$cadd_indel_filename))

## Check that files exist
if (!file.exists(vcf_filename))
  stop(paste("Input VCF file", vcf_filename, "does not exist. Stopping."))
if (!file.exists(cadd_snv_filename))
  stop(paste("Input CADD SNV annotation file", cadd_snv_filename, "does not exist. Stopping."))
if (!file.exists(cadd_indel_filename))
  stop(paste("Input CADD indel annotation file", cadd_indel_filename, "does not exist. Stopping."))

## Step 1 - Parse VCF
print("### RUNNING STEP 1 - Parse VCF\n")
S01_parse_vcf(
  vcf_filename,
  cadd_snv_filename,
  cadd_indel_filename,
  parameters = list(quality = quality,
                    restriction = restriction,
                    transcript = transcript),
  output_root_dir = here("outputs")
)

log_metric_to_run("dummy", 1)
