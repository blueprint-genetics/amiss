library(magrittr)
library(futile.logger)

flog.appender(appender.tee("02_preprocess_data.log"))
flog.info("02_preprocess_data.R")

seed <- 10
flog.info("Using seed: %d", seed)
set.seed(seed)

source("R/preprocessing.R")

flog.info("Reading data")
merged_data <- read.csv("merged_data.csv", as.is = TRUE)
flog.info("Rows: %d", nrow(merged_data))

source("R/feature_definitions.R")
source("R/recursive_application.R")

## Name data rows by strings identifying variants
rownames(merged_data) <- form_variant_ids(merged_data)

## Drop duplicates
flog.info("Dropping duplicates")
merged_data_duplicates <- duplicated(merged_data[, c(numeric_features, categorical_features)])
flog.info("Number of duplicated rows: %d", sum(merged_data_duplicates))

merged_data <- merged_data[!merged_data_duplicates, ]

## Drop VUSes

flog.info("Dropping VUS variants")
vus <- merged_data$CLNSIG == "Uncertain_significance"
flog.info("Number of VUS variants: %d", sum(vus))
merged_data <- merged_data[!vus, ]

## Data split
flog.info("Splitting data to training and test sets")
split_percentage <- 0.7
flog.info("Split percentage: %f", split_percentage)
data_split <- split_train_test(merged_data, split_percentage)

training_set <- data_split$training_set
flog.info("Number of training set variants: %d", nrow(training_set))
test_set <- data_split$test_set
flog.info("Number of test set variants: %d", nrow(test_set))

write.csv(file = "training_data.csv", x = training_set, row.names = FALSE)
write.csv(file = "test_data.csv", x = test_set, row.names = FALSE)

## Process variables

### Coding response

# The response variable (i.e. outcome variable) is processed into 0 (negative) or 1 (positive).

flog.info("Encoding outcome")
positive_classes <- c("Likely_pathogenic", "Pathogenic", "Pathogenic,_drug_response", "Pathogenic/Likely_pathogenic,_drug_response")
negative_classes <- c("Benign", "Likely_benign")
flog.info("Positive classes: %s", paste0(positive_classes, collapse = ", "))
flog.info("Negative classes: %s", paste0(negative_classes, collapse = ", "))

training_outcome <- code_labels(training_set$CLNSIG, positive_classes, negative_classes)
names(training_outcome) <- row.names(training_set)
flog.info(table(training_outcome) %>% capture.output)

test_outcome <- code_labels(test_set$CLNSIG, positive_classes, negative_classes)
names(test_outcome) <- row.names(test_set)
flog.info(table(test_outcome) %>% capture.output)

### Using a priori information to impute by constants

# Some variables have missing values that can be imputed with sensible default values using *a priori* information.
# For example, `motifEHIPos` is a variable that indicates whether the variant is highly informative to an overlapping motif.
# It is set to `NA` when there are no overlapping motifs. We can decide to impute the variable with `FALSE`, which is
# equivalent to defining that a variant is never highly informative to the inexistent motif. The information provided by the
# `NA` should be encoded in a different variable, which indicates whether the variant overlaps any motif. This actually exists
# as the variable `motifECount`, which contains the count of overlapping motifs, and `NA` when one does not exist. Again, it
# makes sense to impute this variable with `0`.

flog.info("Performing a priori imputation")
training_set <- a_priori_impute(training_set, default_imputations)
test_set <- a_priori_impute(test_set, default_imputations)

### Dummy variables

# Categorical variables are processed into sets of dummy variables. Note that here each category is represented by a dummy variable.
# An extra category corresponding a missing value is represented as another dummy variable; this is one strategy
# for handling missing values in categorical variables. This approach is equivalent to the missingness indicator method. We choose
# to fix this choice for simplicity, since many imputation methods make no sense on dummy variables (e.g. regression imputation).
flog.info("Encoding categorical variables via dummy variables")
training_dummy_categoricals <- dummify_categoricals(training_set[, categorical_features, drop = FALSE])
test_dummy_categoricals <- dummify_categoricals(test_set[, categorical_features, drop = FALSE])

# If some dummy variables are present on the test set but not on the training set, the classifier cannot learn to use them and thus
# should just be removed. If some dummy variables are present on the training set but not on the test set, the classifier may still
# benefit from the additional training information, and a constant zero variable should be created on the test set to indicate lack
# of belonging to that class.

# Since the latter scenario does not apply in our case, the implementation beyond checking for it is skipped.
flog.info("Checking that dummy variables match between training and test sets")
training_dummy_names <- training_dummy_categoricals %>% colnames
test_dummy_names <- test_dummy_categoricals %>% colnames

if (!setequal(training_dummy_names, test_dummy_names)) {

  not_in_test_set <- setdiff(training_dummy_names, test_dummy_names)
  not_in_training_set <- setdiff(test_dummy_names, training_dummy_names)

  if (length(not_in_test_set) > 0) {
    flog.info(not_in_test_set %>% paste0(collapse = ", ") %>% paste0(" not in the test set"))
  }

  if (length(not_in_training_set) > 0) {
     flog.info(not_in_training_set %>% paste0(collapse = ", ") %>% paste0(" not in the training set; removing"))
     test_dummy_categoricals[, not_in_training_set] <- NULL
  }
}

# Next, the new dummy variables are bound to the `data.frame`. We keep also the original categorical variables, since they are easier
# to use for certain statistics computations.
flog.info("Binding dummy variables to data")
training_set <- cbind(
  training_set,
  training_dummy_categoricals
)

test_set <- cbind(
  test_set,
  test_dummy_categoricals
)

## Class distribution per consequence

# Next, check whether all consequences have both positive and negative examples.
flog.info("Checking consequence-dependent class imbalance")
flog.info(capture.output(table_with_margin(training_set$Consequence.x, training_set$CLNSIG, useNA = "always") %>% as.data.frame %>% print))
flog.info(capture.output(table_with_margin(training_set$Consequence.x, training_outcome, useNA = "always") %>% as.data.frame %>% print))

# There is significant class imbalance seen when conditioning on the outcome. One might consider e.g. removing all stop-gain variants
# in the test set to avoid biasing the result. Since the training / test split was random, the distributions should be similar in the
# test set, and thus any stop-gain variants will likely also all be pathogenic in the test set. The model will then might learn to guess
# correctly looking only at the consequence, something that could also be programmed deterministically and thus is not interesting in a
# machine-learning perspective.

# Thus we remove variants from categories with very few examples (< 5 %) in either positive or negative category.
flog.info("Removing variants with consequences that have high class imbalance")
unbalanced_conseqs <- detect_imbalanced_consequence_classes(training_set$Consequence.x, training_outcome, 0.05)
tr_variants_w_unbalanced_class <- training_set$Consequence.x %in% rownames(unbalanced_conseqs)
flog.info("Training variants with high consequence dependent class imbalance: %d", sum(tr_variants_w_unbalanced_class))
training_set <- training_set[!tr_variants_w_unbalanced_class, ]
training_outcome <- training_outcome[!tr_variants_w_unbalanced_class]

te_variants_w_unbalanced_class <- test_set$Consequence.x %in% rownames(unbalanced_conseqs)
flog.info("Test variants with high consequence dependent class imbalance: %d", sum(te_variants_w_unbalanced_class))
test_set <- test_set[!te_variants_w_unbalanced_class, ]
test_outcome <- test_outcome[!te_variants_w_unbalanced_class]
flog.info("Remaining variants in training set: %d", nrow(training_set))
flog.info("Remaining variants in training set: %d", nrow(test_set))

## Feature selection 
# The features are selected to be values from tools in dbNSFP that are not themselves already metapredictors. E.g. MetaSVM and Eigen are thus filtered out. From CADD annotations, features are chosen by using some intuition of whether they might be usable by the classifier. 
# Only dummy variable versions of categorical features are kept here.
flog.info("Performing feature selection")
training_set_contracted <- select_features(training_set, numeric_features, categorical_features)
test_set_contracted <- select_features(test_set, numeric_features, categorical_features)

# Finally, write out the processed data CSV file.
flog.info("Writing files")
write.csv(training_set, "preprocessed_training_data.csv", row.names = TRUE)
write.csv(test_set, "preprocessed_test_data.csv", row.names = TRUE)
write.csv(training_set_contracted, "contracted_training_data.csv", row.names = TRUE)
write.csv(test_set_contracted, "contracted_test_data.csv", row.names = TRUE)
write.csv(training_outcome, "training_outcomes.csv", row.names = TRUE)
write.csv(test_outcome, "test_outcomes.csv", row.names = TRUE)

write(capture.output(sessionInfo()), "02_preprocess_data_sessioninfo.txt")
flog.info("Done writing files")


