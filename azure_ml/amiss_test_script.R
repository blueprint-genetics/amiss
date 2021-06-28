library(azuremlsdk)
library(optparse)
library(here)
library(amiss)

## Parameters --------------------------------------------------------------

args <- commandArgs(trailingOnly = TRUE)
cat(args)

data_folder <- as.character(args[2])

n_folds <- as.integer(args[4])

categorical <- as.character(args[12])
log_metric_to_run("categorical", categorical)

imputation <- as.character(args[14])
log_metric_to_run("imputation", imputation)

quality <- as.character(args[16])
log_metric_to_run("quality", quality)

restriction <- as.character(args[18])
log_metric_to_run("restriction", restriction)

transcript <- as.character(args[20])
log_metric_to_run("transcript", transcript)

vus_inclusion <- as.character(args[22])
log_metric_to_run("vus_inclusion", vus_inclusion)

set.seed(10)

dir.create(here("outputs"))
dir.create(here("outputs", "data"))

# vcf_filename <- here(data_folder, "clinvar_20190624.vep.vcf_head_10000")
# cadd_snv_filename <- here(data_folder, "CADD_clingen.tsv")
# cadd_indel_filename <- here(data_folder, "CADD_clingen_indel.tsv")

## Unzip VCF file if it's in .gz format
vcf_filename <- here(data_folder, as.character(args[6]))

if (endsWith(vcf_filename, ".gz")){
  system(paste0("gunzip ", vcf_filename))
  vcf_filename <- sub(".gz", "", vcf_filename)
}
cadd_snv_filename <- here(data_folder, as.character(args[8]))
cadd_indel_filename <- here(data_folder, as.character(args[10]))

list.files(path = data_folder)


## Write Parameter JSON
parameter_json = paste0('{',
                        '"transcript": "', transcript,'", ',
                        '"quality": "', quality,'", ',
                        '"restriction": "', restriction,'", ',
                        '"vus_inclusion": "', vus_inclusion,'", ',
                        '"categorical": "', categorical,'", ',
                        '"imputation": "', imputation,'" ',
                        '}')
print(parameter_json)

session_params_path = "session_parameters.json"
param_file <- file(session_params_path)
writeLines(parameter_json, param_file)
close(param_file)

amiss:::get_config(session_params_path)

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
  parameters_path = here(session_params_path),
  output_root_dir = here("outputs")
  )

list.files(path = here("outputs"))
list.files(path = here("outputs", "data"))

## Step 2 - Preprocess Data
print("### RUNNING STEP 2 - Preprocess Data\n")

gc()

S02_preprocess_data(
  parsed_data_path = here("outputs", "data"),
  parameters_path = session_params_path,
  output_path = here("outputs")
  )

list.files(path = here("outputs"))
list.files(path = here("outputs", "data"))

## Create parallel backend
library(doParallel)
no_cores <- detectCores()
cl <- makeCluster(no_cores)
#clusterExport(cl, c(""))
registerDoParallel(cl)

## Step 11 - Cross-Validation
print("### RUNNING STEP 11 - Cross-Validation\n")

gc()

S11_cross_validation(
  preprocessed_data_path = here("outputs"),
  output_path = here("outputs"),
  parameters_path = session_params_path,
  n_folds = n_folds
  )

list.files(path = here("outputs"))
list.files(path = here("outputs", "output"))

## Get MCC metrics
mcc_lr <- mean(read.csv(here("outputs", "cv_lr_results.csv"), header = TRUE)$MCC)
mcc_rf <- mean(read.csv(here("outputs", "cv_rf_results.csv"), header = TRUE)$MCC)

if (mcc_rf >= mcc_lr){
  mcc <- mcc_rf
} else {
  mcc <- mcc_lr
}

log_metric_to_run("mcc", mcc)
