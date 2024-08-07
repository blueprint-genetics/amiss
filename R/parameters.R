TRANSCRIPT <- "transcript"
TRANSCRIPT_KEEP_ALL <- "keep_all"
TRANSCRIPT_CANONICAL <- "canonical"

CLASSIFICATION_QUALITY <- "quality"
CLASSIFICATION_QUALITY_CLINGEN <- "clingen"
CLASSIFICATION_QUALITY_TWOSTAR <- "twostar"
CLASSIFICATION_QUALITY_ONESTAR <- "onestar"

VUS_INCLUSION <- "vus_inclusion"
VUS_AS_BENIGN <- "benign"
VUS_AS_PATHOGENIC <- "pathogenic"
VUS_EXCLUDE <- "exclude"

CATEGORICAL_ENCODING <- "categorical"
CATEGORICAL_AS_FACTOR <- "factor"
CATEGORICAL_AS_DUMMY <- "dummy"

RESTRICTION_MISSENSE <- "restriction"
MISSENSE_ONLY <- "missense"
ALL <- "all"

IMPUTATION_METHOD <- "imputation"

DOWNSAMPLING <- "downsampling"
DOWNSAMPLING_ON <- "on"
DOWNSAMPLING_OFF <- "off"

#' @export
PREPROCESSING_PARAMETER_SUBSET <- c(TRANSCRIPT, CLASSIFICATION_QUALITY, RESTRICTION_MISSENSE)

HYPERPARAMETER_SEARCH_TYPE <- "hyperparameter_search_type"
HYPERPARAMETER_SEARCH_TYPE_GRID <- "grid"
HYPERPARAMETER_SEARCH_TYPE_RANDOM <- "random"

NZV_CHECK <- "nonzero_variance_check"
NZV_CHECK_ON <- "on"
NZV_CHECK_OFF <- "off"

CORRELATION_CHECK <- "correlation_check"
CORRELATION_CHECK_ON <- "on"
CORRELATION_CHECK_OFF <- "off"

TRAINING_DATA_SAMPLING_PERCENTAGE <- "training_data_sampling_percentage"
TRAINING_DATA_SAMPLING_PERCENTAGE_ALLOWED_VALUES <- c(0.33, 0.66, 1.0)

FEATURE_SAMPLING_PERCENTAGE <- "feature_sampling_percentage"
FEATURE_SAMPLING_PERCENTAGE_ALLOWED_VALUES <- c(0.33, 0.66, 1.0)
