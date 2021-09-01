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
  futile.logger::flog.appender(futile.logger::appender.tee(file.path(output_path, "02_preprocess_data.log")))
  futile.logger::flog.info("02_preprocess_data.R")
  
  config <- get_config(parameters_path)
  
  futile.logger::flog.info("Using seed: %d", seed)
  set.seed(seed)
  futile.logger::flog.info("Reading data")
  merged_data <- read.csv(file.path(parsed_data_path, FILE_MERGED_DATA_CSV), as.is = TRUE)
  futile.logger::flog.info("Rows: %d", nrow(merged_data))
  
  ## Name data rows by strings identifying variants
  rownames(merged_data) <- form_variant_ids(merged_data)
  
  ## Drop duplicates
  futile.logger::flog.info("Dropping duplicates")
  merged_data_duplicates <- duplicated(merged_data[, c(numeric_features, categorical_features)])
  futile.logger::flog.info("Number of duplicated rows: %d", sum(merged_data_duplicates))
  
  merged_data <- merged_data[!merged_data_duplicates, ]
  
  # The response variable (i.e. outcome variable) is processed into 0 (negative) or 1 (positive).
  futile.logger::flog.info("Encoding outcome")
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
  futile.logger::flog.info("Positive classes: %s", paste0(positive_classes, collapse = ", "))
  futile.logger::flog.info("Negative classes: %s", paste0(negative_classes, collapse = ", "))
  
  uncertain_classes <- c("Uncertain_significance", "Uncertain_significance,_other", "Uncertain_significance,_association", "other", "risk_factor", "association_not_found", "Affects")
  if (config[[VUS_INCLUSION]] == VUS_AS_BENIGN) {
    negative_classes <- c(negative_classes, uncertain_classes)
  } else if (config[[VUS_INCLUSION]] == VUS_AS_PATHOGENIC) {
    positive_classes <- c(positive_classes, uncertain_classes)
  } else if (config[[VUS_INCLUSION]] == VUS_EXCLUDE) {
    futile.logger::flog.info("Dropping VUS variants")
    vus <- merged_data$CLNSIG %in% uncertain_classes
    futile.logger::flog.info("Number of VUS variants: %d", sum(vus))
    merged_data <- merged_data[!vus, ]
  } else stop(
    paste0("Unknown value \"", config[[VUS_INCLUSION]], "\" for parameter \"", VUS_INCLUSION, "\"")
  )
  
  outcome <- code_labels(merged_data$CLNSIG, positive_classes, negative_classes)
  
  ## Data split
  futile.logger::flog.info("Splitting data to training and test sets")
  split_percentage <- 0.7
  futile.logger::flog.info("Split percentage: %f", split_percentage)
  data_split <- split_train_test(merged_data, split_percentage)
  
  training_set <- data_split$training_set
  futile.logger::flog.info("Number of training set variants: %d", nrow(training_set))
  test_set <- data_split$test_set
  futile.logger::flog.info("Number of test set variants: %d", nrow(test_set))
  
  training_outcome <- outcome[data_split$index]
  names(training_outcome) <- row.names(training_set)
  futile.logger::flog.info(table(training_outcome) %>% capture.output)
  
  test_outcome <- outcome[-data_split$index]
  names(test_outcome) <- row.names(test_set)
  futile.logger::flog.info(table(test_outcome) %>% capture.output)
  
  write.csv(file = file.path(output_path, FILE_TRAINING_DATA_CSV), x = training_set, row.names = FALSE)
  write.csv(file = file.path(output_path, FILE_TEST_DATA_CSV), x = test_set, row.names = FALSE)
  
  ### Dummy variables
  
  # Categorical variables are processed into sets of dummy variables. Note that here each category is represented by a dummy variable.
  # An extra category corresponding a missing value is represented as another dummy variable; this is one strategy
  # for handling missing values in categorical variables. This approach is equivalent to the missingness indicator method. We choose
  # to fix this choice for simplicity, since many imputation methods make no sense on dummy variables (e.g. regression imputation).
  futile.logger::flog.info("Encoding categorical variables via dummy variables")
  if (config[[CATEGORICAL_ENCODING]] == CATEGORICAL_AS_DUMMY) {
    training_dummy_categoricals <- dummify_categoricals(training_set[, categorical_features, drop = FALSE])
    test_dummy_categoricals <- dummify_categoricals(test_set[, categorical_features, drop = FALSE])
    
    # If some dummy variables are present on the test set but not on the training set, the classifier cannot learn to use them and thus
    # should just be removed. If some dummy variables are present on the training set but not on the test set, the classifier may still
    # benefit from the additional training information, and a constant zero variable should be created on the test set to indicate lack
    # of belonging to that class.
    
    # Since the latter scenario does not apply in our case, the implementation beyond checking for it is skipped.
    futile.logger::flog.info("Checking that dummy variables match between training and test sets")
    training_dummy_names <- training_dummy_categoricals %>% colnames
    test_dummy_names <- test_dummy_categoricals %>% colnames
    
    if (!setequal(training_dummy_names, test_dummy_names)) {
      
      not_in_test_set <- setdiff(training_dummy_names, test_dummy_names)
      not_in_training_set <- setdiff(test_dummy_names, training_dummy_names)
      
      if (length(not_in_test_set) > 0) {
        futile.logger::flog.info(not_in_test_set %>% paste0(collapse = ", ") %>% paste0(" not in the test set"))
      }
      
      if (length(not_in_training_set) > 0) {
        futile.logger::flog.info(not_in_training_set %>% paste0(collapse = ", ") %>% paste0(" not in the training set; removing"))
        test_dummy_categoricals[, not_in_training_set] <- NULL
      }
    }
    
    # Next, the new dummy variables are bound to the `data.frame`. We keep also the original categorical variables, since they are easier
    # to use for certain statistics computations.
    futile.logger::flog.info("Binding dummy variables to data")
    training_set <- cbind(
      training_set,
      training_dummy_categoricals
    )
    
    test_set <- cbind(
      test_set,
      test_dummy_categoricals
    )
  } else if (config[[CATEGORICAL_ENCODING]] == CATEGORICAL_AS_FACTOR) {
    # Do nothing
  } else stop(
    paste0("Unknown value \"", config[[CATEGORICAL_ENCODING]], "\" for parameter \"", CATEGORICAL_ENCODING, "\"")
  )
  
  ## Class distribution per consequence
  
  # Next, check whether all consequences have both positive and negative examples.
  futile.logger::flog.info("Checking consequence-dependent class imbalance")
  futile.logger::flog.info(capture.output(table_with_margin(training_set$Consequence.x, training_set$CLNSIG, useNA = "always") %>% as.data.frame %>% print))
  futile.logger::flog.info(capture.output(table_with_margin(training_set$Consequence.x, training_outcome, useNA = "always") %>% as.data.frame %>% print))
  
  # There is significant class imbalance seen when conditioning on the outcome. One might consider e.g. removing all stop-gain variants
  # in the test set to avoid biasing the result. Since the training / test split was random, the distributions should be similar in the
  # test set, and thus any stop-gain variants will likely also all be pathogenic in the test set. The model will then might learn to guess
  # correctly looking only at the consequence, something that could also be programmed deterministically and thus is not interesting in a
  # machine-learning perspective.
  
  # Thus we remove variants from categories with very few examples (< 5 %) in either positive or negative category.
  futile.logger::flog.info("Removing variants with consequences that have high class imbalance")
  unbalanced_conseqs <- detect_imbalanced_consequence_classes(training_set$Consequence.x, training_outcome, 0.05)
  tr_variants_w_unbalanced_class <- training_set$Consequence.x %in% rownames(unbalanced_conseqs)
  futile.logger::flog.info("Training variants with high consequence dependent class imbalance: %d", sum(tr_variants_w_unbalanced_class))
  training_set <- training_set[!tr_variants_w_unbalanced_class, ]
  training_outcome <- training_outcome[!tr_variants_w_unbalanced_class]
  
  te_variants_w_unbalanced_class <- test_set$Consequence.x %in% rownames(unbalanced_conseqs)
  futile.logger::flog.info("Test variants with high consequence dependent class imbalance: %d", sum(te_variants_w_unbalanced_class))
  test_set <- test_set[!te_variants_w_unbalanced_class, ]
  test_outcome <- test_outcome[!te_variants_w_unbalanced_class]
  futile.logger::flog.info("Remaining variants in training set: %d", nrow(training_set))
  futile.logger::flog.info("Remaining variants in test set: %d", nrow(test_set))
  
  write.csv(training_set, file.path(output_path, FILE_TRAINING_DATA_FOR_STATS_CSV), row.names = TRUE)
  
  ### Using a priori information to impute by constants
  
  # Some variables have missing values that can be imputed with sensible default values using *a priori* information.
  # For example, `motifEHIPos` is a variable that indicates whether the variant is highly informative to an overlapping motif.
  # It is set to `NA` when there are no overlapping motifs. We can decide to impute the variable with `FALSE`, which is
  # equivalent to defining that a variant is never highly informative to the inexistent motif. The information provided by the
  # `NA` should be encoded in a different variable, which indicates whether the variant overlaps any motif. This actually exists
  # as the variable `motifECount`, which contains the count of overlapping motifs, and `NA` when one does not exist. Again, it
  # makes sense to impute this variable with `0`.
  
  futile.logger::flog.info("Performing a priori imputation")
  training_set <- a_priori_impute(training_set, default_imputations)
  test_set <- a_priori_impute(test_set, default_imputations)
  
  ## Feature selection 
  # The features are selected to be values from tools in dbNSFP that are not themselves already metapredictors. E.g. MetaSVM and Eigen are thus filtered out. From CADD annotations, features are chosen by using some intuition of whether they might be usable by the classifier. 
  # Only dummy variable versions of categorical features are kept here.
  futile.logger::flog.info("Performing feature selection")
  training_set <- select_features(training_set, numeric_features, categorical_features)
  test_set <- select_features(test_set, numeric_features, categorical_features)
  
  ## Downsampling majority class
  if (config[[DOWNSAMPLING]] == DOWNSAMPLING_ON) {
    futile.logger::flog.info("Performing downsampling of majority class")
    majority_class <- table(training_outcome)
    majority_class <- majority_class[which.max(majority_class)]
    minority_class <- table(training_outcome)
    minority_class <- minority_class[which.min(minority_class)]
    
    futile.logger::flog.info(paste0("Majority class is \"", names(majority_class), "\""))
    
    # Drop n majority class instances, where n is the number 
    # by which majority class size exceeds minority class size
    drop_idx <- sample(which(training_outcome == names(majority_class)), majority_class - minority_class)
    futile.logger::flog.info(paste0("Dropping ", length(drop_idx), " instances of the majority class"))
    training_outcome <- training_outcome[-drop_idx]
    training_set <- training_set[-drop_idx,, drop = FALSE]
    futile.logger::flog.info(table(training_outcome) %>% capture.output)
     
  } else if (config[[DOWNSAMPLING]] == DOWNSAMPLING_OFF) {
    # Do nothing
  } else stop(
    paste0("Unknown value \"", config[[DOWNSAMPLING]], "\" for parameter \"", DOWNSAMPLING, "\"")
  )
  
  # Finally, write out the processed data CSV file.
  futile.logger::flog.info("Writing files")
  write.csv(training_set, file.path(output_path, FILE_PREPROCESSED_TRAINING_DATA_CSV), row.names = TRUE)
  write.csv(test_set, file.path(output_path, FILE_PREPROCESSED_TEST_DATA_CSV), row.names = TRUE)
  write.csv(training_outcome, file.path(output_path, FILE_TRAINING_OUTCOMES_CSV), row.names = TRUE)
  write.csv(test_outcome, file.path(output_path, FILE_TEST_OUTCOMES_CSV), row.names = TRUE)
  
  write(capture.output(sessionInfo()), file.path(output_path, "02_preprocess_data_sessioninfo.txt"))
  futile.logger::flog.info("Done writing files")
  
  return(output_path)
}
