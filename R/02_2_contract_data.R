
## Feature selection

# The features are selected to be values from tools in dbNSFP that are not themselves already metapredictors. E.g. MetaSVM and Eigen are thus filtered out. From CADD annotations, features are chosen by using some intuition of whether they might be usable by the classifier.
library(magrittr)
library(futile.logger)

source("R/feature_definitions.R")
source("R/utils.R")

flog.appender(appender.tee("02_2_contract_data.log"))

flog.info("02_2_contract_data.R")
flog.info("Reading data")
training_set <- read.csv("preprocessed_training_data.csv", row.names = 1, as.is = TRUE)
test_set <- read.csv("preprocessed_test_data.csv", row.names = 1, as.is = TRUE)

flog.info("Dropping original categorical variables")
drop_original_categorical_features <- function(data, categorical_features) {

  columns <- colnames(data)
  features_with_categorical_variable_prefix <- lapply(categorical_features, . %>% find_dummies(columns))

  dummy_features <- unique(unlist(features_with_categorical_variable_prefix))
  features <- c(numeric_features, dummy_features)
  data <- data[, features, drop = FALSE]

  return(data)
}

training_set <- drop_original_categorical_features(training_set, categorical_features)
test_set <- drop_original_categorical_features(test_set, categorical_features)

flog.info("Writing files")
training_set <- write.csv(training_set, "contracted_training_data.csv", row.names = TRUE)
test_set <- write.csv(test_set, "contracted_test_data.csv", row.names = TRUE)
flog.info("Done writing files")
