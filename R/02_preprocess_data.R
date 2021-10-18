#' Step 02: Preprocess parsed and merged data from step 1.
#'
#' This step preprocesses the data from step 1. This includes:
#' - Encoding of labels
#' - Training/test split
#' - Categorical feature encoding
#' - Removal of variants from predicted consequence classes that are very
#' infrequent
#' - A-priori imputation, i.e. constant imputation with known correct values
#' - (Pre-specified) selection of features
#' 
#' Parameters used by this step (presented via constant names; see
#' R/parameters.R for explicit string values):
#' - VUS_INCLUSION, which specifies whether VUS should be included in positive
#'  or negative labels or excluded
#' - CATEGORICAL_ENCODING, which specifies whether categorical features should
#' be encoded as dummy variables
#'
#' @param parsed_data_path Path to parsed data directory produced in step 01
#' @param parameters_path Path to parameter combination file
#' @param output_path Path to directory in which to write output files
#' @param seed Seed value
#'
#' @return Path to directory in which output files were written
#' @export
S02_preprocess_data <- function(parsed_data_path, parameters_path, output_path, seed = 10) {
  
  output_path <- normalizePath(output_path, mustWork = TRUE)
  futile.logger::flog.appender(futile.logger::appender.tee(file.path(output_path, "02_preprocess_data.log")))
  futile.logger::flog.info("START 02_preprocess_data.R")
  
  parameters_path <- normalizePath(parameters_path)
  futile.logger::flog.info("INPUT Reading parameters from JSON file at %s", parameters_path)
  config <- get_config(parameters_path)
  
  if (config[[VUS_INCLUSION]] %>% is.null) {
    stop("Required parameter \"" %>% paste0(VUS_INCLUSION, "\" not provided"))
  }
  if (config[[CATEGORICAL_ENCODING]] %>% is.null) {
    stop("Required parameter \"" %>% paste0(CATEGORICAL_ENCODING, "\" not provided"))
  }
  if (config[[DOWNSAMPLING]] %>% is.null) {
    stop("Required parameter \"" %>% paste0(DOWNSAMPLING, "\" not provided"))
  }
  
  futile.logger::flog.info("DESIGN_CHOICE Using seed: %d", seed)
  set.seed(seed)
  
  merged_data_path <- parsed_data_path %>% normalizePath %>% file.path(FILE_MERGED_DATA_CSV)
  futile.logger::flog.info("INPUT Reading annotated ClinVar variant data from VCF file at %s", merged_data_path)
  merged_data <- read.csv(merged_data_path, as.is = TRUE)
  futile.logger::flog.info("INPUT Rows: %d", nrow(merged_data))
  
  ## Name data rows by strings identifying variants
  rownames(merged_data) <- form_variant_ids(merged_data)
  
  ## Drop duplicates
  futile.logger::flog.info("DESIGN_CHOICE Dropping duplicates")
  merged_data_duplicates <- duplicated(merged_data[, c(numeric_features, categorical_features)])
  futile.logger::flog.info("DESIGN_CHOICE Number of duplicated rows: %d", sum(merged_data_duplicates))
  
  merged_data <- merged_data[!merged_data_duplicates, ]
  
  # The response variable (i.e. outcome variable) is processed into 0 (negative) or 1 (positive).
  futile.logger::flog.info("DESIGN_CHOICE Encoding outcome")
  positive_classes <- c("Likely_pathogenic", "Pathogenic", "Pathogenic,_drug_response", 
                        "Pathogenic/Likely_pathogenic,_drug_response", "Pathogenic/Likely_pathogenic", 
                        "Pathogenic,_other", "Pathogenic/Likely_pathogenic,_risk_factor", 
                        "Pathogenic/Likely_pathogenic,_other", "Pathogenic,_other,_risk_factor", "association")
  negative_classes <- c("Benign", "Likely_benign", "Benign/Likely_benign", "Benign/Likely_benign,_other", 
                        "Likely_benign,_other", "Benign/Likely_benign,_risk_factor", 
                        "Benign/Likely_benign,_drug_response", "Benign,_risk_factor", 
                        "Benign/Likely_benign,_protective", "Benign/Likely_benign,_drug_response,_risk_factor",  
                        "Benign/Likely_benign,_Affects", "Benign,_other", "Likely_benign,_drug_response,_other", 
                        "Likely_benign,_risk_factor", "Benign,_drug_response", "Likely_benign,_other,_risk_factor", 
                        "Benign/Likely_benign,_protective,_risk_factor", "Benign/Likely_benign,_association", "protective")
  futile.logger::flog.info("DESIGN_CHOICE Positive classes: %s", paste0(positive_classes, collapse = ", "))
  futile.logger::flog.info("DESIGN_CHOICE Negative classes: %s", paste0(negative_classes, collapse = ", "))
  
  uncertain_classes <- c("Uncertain_significance", "Uncertain_significance,_other", "Uncertain_significance,_association", "other", "risk_factor", "association_not_found", "Affects")
  
  vus <- merged_data$CLNSIG %in% uncertain_classes
  futile.logger::flog.info("PROGRESS Number of VUS variants: %d", sum(vus))
  futile.logger::flog.info("PARAMETER %s = %s", VUS_INCLUSION, config[[VUS_INCLUSION]])
  if (config[[VUS_INCLUSION]] == VUS_AS_BENIGN) {
    negative_classes <- c(negative_classes, uncertain_classes)
  } else if (config[[VUS_INCLUSION]] == VUS_AS_PATHOGENIC) {
    positive_classes <- c(positive_classes, uncertain_classes)
  } else if (config[[VUS_INCLUSION]] == VUS_EXCLUDE) {
    futile.logger::flog.info("PARAMETER Dropping VUS variants")
    merged_data <- merged_data[!vus, ]
  } else stop(
    paste0("Unknown value \"", config[[VUS_INCLUSION]], "\" for parameter \"", VUS_INCLUSION, "\"")
  )
  
  futile.logger::flog.info("DESIGN_CHOICE Coding positive outcomes as 1 and negative outcomes as 0")
  outcome <- code_labels(merged_data$CLNSIG, positive_classes, negative_classes)
  
  ## Data split
  futile.logger::flog.info("PROGRESS Splitting data to training and test sets")
  split_percentage <- 0.7
  futile.logger::flog.info("DESIGN_CHOICE Split percentage: %f", split_percentage)
  data_split <- split_train_test(merged_data, split_percentage)
  
  training_set <- data_split$training_set
  futile.logger::flog.info("PROGRESS Number of training set variants: %d", nrow(training_set))
  test_set <- data_split$test_set
  futile.logger::flog.info("PROGRESS Number of test set variants: %d", nrow(test_set))
  
  training_outcome <- outcome[data_split$index]
  names(training_outcome) <- row.names(training_set)
  futile.logger::flog.info("PROGRESS Outcome distribution:")
  futile.logger::flog.info(paste0("PROGRESS ", table(training_outcome) %>% capture.output))
  
  test_outcome <- outcome[-data_split$index]
  names(test_outcome) <- row.names(test_set)
  futile.logger::flog.info(paste0("PROGRESS ", table(test_outcome) %>% capture.output))
  
  training_set_path <- file.path(output_path, FILE_TRAINING_DATA_CSV)
  futile.logger::flog.info("PROGRESS Writing out intermediate version of training data to delimited file at %s", training_set_path)
  write.csv(file = training_set_path, x = training_set, row.names = FALSE)
  
  test_set_path <- file.path(output_path, FILE_TEST_DATA_CSV)
  futile.logger::flog.info("PROGRESS Writing out intermediate version of training data to delimited file at %s", test_set_path)
  write.csv(file = test_set_path, x = test_set, row.names = FALSE)
  
  ### Dummy variables
  
  # Categorical variables are processed into sets of dummy variables. Note that here each category is represented by a dummy variable.
  # An extra category corresponding a missing value is represented as another dummy variable; this is one strategy
  # for handling missing values in categorical variables. This approach is equivalent to the missingness indicator method. We choose
  # to fix this choice for simplicity, since many imputation methods make no sense on dummy variables (e.g. regression imputation).
  
  futile.logger::flog.info("DESIGN_CHOICE Getting rid of categorical variable values in test set that are not present in training set by setting them to missing")
  test_set[,categorical_features] <- mapply(FUN = function(x, y) {
      x[!(x %in% y)] <- NA
      return(x)
    },
    test_set[, categorical_features, drop = FALSE],
    training_set[, categorical_features, drop = FALSE]
  )
  
  futile.logger::flog.info("PARAMETER %s = %s", CATEGORICAL_ENCODING, config[[CATEGORICAL_ENCODING]])
  if (config[[CATEGORICAL_ENCODING]] == CATEGORICAL_AS_DUMMY) {
    
    futile.logger::flog.info("PARAMETER Encoding categorical variables via dummy variables")
    training_dummy_categoricals <- dummify_categoricals(training_set[, categorical_features, drop = FALSE])
    test_dummy_categoricals <- dummify_categoricals(test_set[, categorical_features, drop = FALSE])
    
    # Next, the new dummy variables are bound to the `data.frame`. We keep also the original categorical variables, since they are easier
    # to use for certain statistics computations.
    futile.logger::flog.info("PARAMETER Binding dummy variables to data")
    training_set <- cbind(
      training_set,
      training_dummy_categoricals
    )
    
    test_set <- cbind(
      test_set,
      test_dummy_categoricals
    )
  } else if (config[[CATEGORICAL_ENCODING]] == CATEGORICAL_AS_FACTOR) {
    futile.logger::flog.info("PARAMETER Adding \"MISSING\" as factor level in place of actual missing values")
    # Even though when written as CSV missing values get written as strings "NA", this is still useful if we replace
    # the "NA" string with something else, as then we won't mix up numerical NA and categorical NA when reading the file.
    any_missing <- training_set[, categorical_features] %>% sapply(. %>% is.na %>% any)
    any_missing <- any_missing | test_set[, categorical_features] %>% sapply(. %>% is.na %>% any)
    
    training_set[, categorical_features[any_missing]]  %<>% lapply(function(x) {
      x[is.na(x)] <- "MISSING"
      return(x)
    })
    test_set[, categorical_features[any_missing]]  %<>% lapply(function(x) {
      x[is.na(x)] <- "MISSING"
      return(x)
    })
  } else stop(
    paste0("Unknown value \"", config[[CATEGORICAL_ENCODING]], "\" for parameter \"", CATEGORICAL_ENCODING, "\"")
  )
  
  ## Class distribution per consequence
  
  # Next, check whether all consequences have both positive and negative examples.
  futile.logger::flog.info("PROGRESS Checking consequence-dependent class imbalance")
  futile.logger::flog.info(paste0("PROGRESS ", capture.output(table_with_margin(training_set$Consequence.x, training_set$CLNSIG, useNA = "always") %>% as.data.frame %>% print)))
  futile.logger::flog.info(paste0("PROGRESS ", capture.output(table_with_margin(training_set$Consequence.x, training_outcome, useNA = "always") %>% as.data.frame %>% print)))
  
  ### Using a priori information to impute by constants
  
  # Some variables have missing values that can be imputed with sensible default values using *a priori* information.
  # For example, `motifEHIPos` is a variable that indicates whether the variant is highly informative to an overlapping motif.
  # It is set to `NA` when there are no overlapping motifs. We can decide to impute the variable with `FALSE`, which is
  # equivalent to defining that a variant is never highly informative to the inexistent motif. The information provided by the
  # `NA` should be encoded in a different variable, which indicates whether the variant overlaps any motif. This actually exists
  # as the variable `motifECount`, which contains the count of overlapping motifs, and `NA` when one does not exist. Again, it
  # makes sense to impute this variable with `0`.
  
  futile.logger::flog.info("DESIGN_CHOICE Performing a priori imputation")
  training_set <- a_priori_impute(training_set, default_imputations)
  test_set <- a_priori_impute(test_set, default_imputations)
  
  ## Feature selection 
  # The features are selected to be values from tools in dbNSFP that are not themselves already metapredictors. E.g. MetaSVM and Eigen are thus filtered out. From CADD annotations, features are chosen by using some intuition of whether they might be usable by the classifier. 
  # Only dummy variable versions of categorical features are kept here.
  futile.logger::flog.info("DESIGN_CHOICE Performing feature selection")
  if (config[[CATEGORICAL_ENCODING]] == CATEGORICAL_AS_DUMMY) {
    training_set <- select_features(training_set, numeric_features, categorical_features)
    test_set <- select_features(test_set, numeric_features, categorical_features)
  } else if (config[[CATEGORICAL_ENCODING]] == CATEGORICAL_AS_FACTOR) {
    training_set <- training_set[,c(numeric_features, categorical_features)]
    test_set <- test_set[,c(numeric_features, categorical_features)]
  }
  
  ## Downsampling majority class
  futile.logger::flog.info("PARAMETER %s = %s", DOWNSAMPLING, config[[DOWNSAMPLING]])
  if (config[[DOWNSAMPLING]] == DOWNSAMPLING_ON) {
    futile.logger::flog.info("PARAMETER Performing downsampling of majority class")
    majority_class <- table(training_outcome)
    majority_class <- majority_class[which.max(majority_class)]
    minority_class <- table(training_outcome)
    minority_class <- minority_class[which.min(minority_class)]
    
    futile.logger::flog.info(paste0("PARAMETER Majority class is \"", names(majority_class), "\""))
    
    # Drop n majority class instances, where n is the number 
    # by which majority class size exceeds minority class size
    drop_idx <- sample(which(training_outcome == names(majority_class)), majority_class - minority_class)
    futile.logger::flog.info(paste0("PARAMETER Dropping ", length(drop_idx), " instances of the majority class"))
    training_outcome <- training_outcome[-drop_idx]
    training_set <- training_set[-drop_idx,, drop = FALSE]
    futile.logger::flog.info(paste0("PARAMETER ", table(training_outcome) %>% capture.output))
     
  } else if (config[[DOWNSAMPLING]] == DOWNSAMPLING_OFF) {
    # Do nothing
  } else stop(
    paste0("Unknown value \"", config[[DOWNSAMPLING]], "\" for parameter \"", DOWNSAMPLING, "\"")
  )
  
  # Finally, write out the processed data CSV file.
  training_set_path <- file.path(output_path, FILE_PREPROCESSED_TRAINING_DATA_CSV)
  futile.logger::flog.info("OUTPUT Writing fully preprocessed training set data to %s", training_set_path)
  write.csv(training_set, training_set_path, row.names = TRUE)
  training_outcome_path <- file.path(output_path, FILE_TRAINING_OUTCOMES_CSV)
  futile.logger::flog.info("OUTPUT Writing fully preprocessed training set outcomes to %s", training_outcome_path)
  write.csv(training_outcome, training_outcome_path, row.names = TRUE)
  test_set_path <- file.path(output_path, FILE_PREPROCESSED_TEST_DATA_CSV)
  futile.logger::flog.info("OUTPUT Writing fully preprocessed test set data to %s", test_set_path)
  write.csv(test_set, test_set_path, row.names = TRUE)
  test_outcome_path <- file.path(output_path, FILE_TEST_OUTCOMES_CSV)
  futile.logger::flog.info("OUTPUT Writing fully preprocessed test set outcomes to %s", test_outcome_path)
  write.csv(test_outcome, test_outcome_path, row.names = TRUE)
  
  sessioninfo_path <- file.path(output_path, "02_preprocess_data_sessioninfo.txt")
  futile.logger::flog.info("OUTPUT Writing session information to text file at %s", sessioninfo_path)
  write(capture.output(sessionInfo()), sessioninfo_path)
  futile.logger::flog.info("PROGRESS Done writing files")
  
  futile.logger::flog.info("DONE 02_preprocess_data.R")
  return(output_path)
}
