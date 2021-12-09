library(azuremlsdk)
library(optparse)
library(here)
library(amiss)

## Parameters --------------------------------------------------------------

option_list <-
  list(
    make_option("--n_folds", action = "store"),
    make_option("--categorical", action = "store"),
    make_option("--imputation", action = "store"),
    make_option("--quality", action = "store"),
    make_option("--restriction", action = "store"),
    make_option("--transcript", action = "store"),
    make_option("--vus_inclusion", action = "store"),
    make_option("--downsampling", action = "store"),
    make_option("--nonzero_variance_check", action = "store"),
    make_option("--correlation_check", action = "store"),
    make_option("--hyperparameter_search_type", action = "store"),
    make_option("--training_data_sampling", action = "store"),
    make_option("--training_data_sampling_percentage", action = "store"),
    make_option("--feature_sampling", action = "store"),
    make_option("--feature_sampling_percentage", action = "store")
  )
args <- parse_args(OptionParser(option_list = option_list))

n_folds <- as.integer(args$n_folds)

categorical <- as.character(args$categorical)
log_metric_to_run("categorical", categorical)

imputation <- as.character(args$imputation)
log_metric_to_run("imputation", imputation)

quality <- as.character(args$quality)
log_metric_to_run("quality", quality)

restriction <- as.character(args$restriction)
log_metric_to_run("restriction", restriction)

transcript <- as.character(args$transcript)
log_metric_to_run("transcript", transcript)

vus_inclusion <- as.character(args$vus_inclusion)
log_metric_to_run("vus_inclusion", vus_inclusion)

downsampling <- as.character(args$downsampling)
log_metric_to_run("downsampling", downsampling)

nonzero_variance_check <- as.character(args$nonzero_variance_check)
log_metric_to_run("nonzero_variance_check", nonzero_variance_check)

correlation_check <- as.character(args$correlation_check)
log_metric_to_run("correlation_check", correlation_check)

correlation_check <- as.character(args$correlation_check)
log_metric_to_run("correlation_check", correlation_check)

hyperparameter_search_type <- as.character(args$hyperparameter_search_type)
log_metric_to_run("hyperparameter_search_type", hyperparameter_search_type)

training_data_sampling <- as.character(args$training_data_sampling)
log_metric_to_run("training_data_sampling", training_data_sampling)

feature_sampling <- as.character(args$feature_sampling)
log_metric_to_run("feature_sampling", feature_sampling)

training_data_sampling_percentage <- as.numeric(args$training_data_sampling_percentage)
log_metric_to_run("training_data_sampling_percentage", training_data_sampling_percentage)

feature_sampling_percentage <- as.numeric(args$feature_sampling_percentage)
log_metric_to_run("feature_sampling_percentage", feature_sampling_percentage)
set.seed(10)

dir.create(here("outputs"))
dir.create(here("outputs", "data"))

## Write Parameter JSON
parameter_json = paste0('{',
                        '"transcript": "', transcript,'", ',
                        '"quality": "', quality,'", ',
                        '"restriction": "', restriction,'", ',
                        '"vus_inclusion": "', vus_inclusion,'", ',
                        '"categorical": "', categorical,'", ',
                        '"imputation": "', imputation,'", ',
                        '"nonzero_variance_check": "', nonzero_variance_check, '", ',
                        '"correlation_check": "', correlation_check, '", ',
                        '"hyperparameter_search_type": "', hyperparameter_search_type, '", ',
                        '"training_data_sampling": "', training_data_sampling, '", ',
                        '"feature_sampling": "', feature_sampling, '", ',
                        '"training_data_sampling_percentage": ', training_data_sampling_percentage, ', ',
                        '"feature_sampling_percentage": ', feature_sampling_percentage, ', ',
                        '"downsampling": "', downsampling, '"',
                        '}')
print(parameter_json)

session_params_path = "session_parameters.json"
param_file <- file(session_params_path)
writeLines(parameter_json, param_file)
close(param_file)

## Step 2 - Preprocess Data
print("### RUNNING STEP 2 - Preprocess Data\n")

gc()

S02_preprocess_data(
  parsed_data_path = here("data", amiss::generate_parameter_dependent_name(
    list(
      transcript = transcript,
      quality = quality,
      restriction = restriction
    )
  )), 
  parameters_path = session_params_path,
  output_path = here("outputs")
)

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

## Get MCC metrics
mcc_lr <- mean(read.csv(here("outputs", "cv_lr_results.csv"), header = TRUE)$MCC, na.rm = TRUE)
mcc_rf <- mean(read.csv(here("outputs", "cv_rf_results.csv"), header = TRUE)$MCC, na.rm = TRUE)
mcc_xg <- mean(read.csv(here("outputs", "cv_xg_results.csv"), header = TRUE)$MCC, na.rm = TRUE)

mcc <- max(mcc_lr, mcc_rf, mcc_xg, na.rm = TRUE)
log_metric_to_run("mcc", mcc)
