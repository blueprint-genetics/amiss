#' Partition dataset into a training and a test set
#'
#' @param dataframe A data.frame that will be partitioned into two.
#' @param proportion Proportional size of training set compared to test set
#'
#' @return Two-element list containing the training and test sets.
#' @importFrom magrittr %<>%
split_train_test <- function(dataframe, proportion) {

  stopifnot(class(dataframe) == "data.frame")
  stopifnot(nrow(dataframe) > 2)
  stopifnot(is.numeric(proportion))
  stopifnot(length(proportion) == 1)
  stopifnot(proportion < 1.0)
  stopifnot(proportion > 0.0)

  # Compute vector answering "does this element belong to the training set?"
  # Draw from a uniform distribution between 0 and 1,
  # and threshold so that approximately `proportion` rows will
  # be assigned to the training set
  index <- runif(nrow(dataframe), min = 0, max = 1) < proportion

  datasplit <- list(
    training_set = dataframe[index, ],
    test_set = dataframe[!index, ],
    index = index
  )

  return(datasplit)
}

#' Code ClinVar classifications as positive and negative
#'
#' @param class_vector Character vector containing ClinVar classifications
#' @param positive_classes Character vector designating the classifications that are considered positive
#' @param negative_classes Character vector designating the classifications that are considered negative
#'
#' @return Character vector containing "positive" for each pathogenic classification and
#' "negative" for each non-pathogenic classification.
code_labels <- function(class_vector, positive_classes, negative_classes) {

  stopifnot(class(class_vector) == "character")
  stopifnot(length(class_vector) > 0)

  # If classification is not any of these, raise and error
  undefined_class_ind <- !class_vector %in% c(positive_classes, negative_classes)
  if (any(undefined_class_ind)) {
    error_msg <- "Undefined class(es) in computing numeric labels: " %>% paste0(paste0(unique(class_vector[undefined_class_ind]), collapse=", "))
    stop(error_msg)
  }

  positive_class_indicator <- ifelse(class_vector %in% positive_classes, "positive", "negative")

  return(positive_class_indicator)
}

#' Convert categorical variables into sets of dummy variables
#'
#' @param data A data.frame containing only character columns
#'
#' @return A data.frame containing dummy variables for each original column
dummify_categoricals <- function(data) {

  if (!is.data.frame(data)) stop("`data` must be a data.frame")
  # Check that all columns are of type character
  if (!all(sapply(data, is.character))) stop("All columns in `data` must be character vectors")

  data <- lapply(data, function(col) {
    # Find all unique values that appear in this column
    col <- addNA(col)
    lvls <- col %>% unique %>% na.omit

    # Form a dummy variable for each unique value of original column
    dummies <- lapply(lvls, function(value) {
       as.integer(col == value)
    })
    dummies <- data.frame(dummies, fix.empty.names = FALSE)

    # Note that here we set the column names to equal the possible values,
    # without reference to the original variable name. However, since lapply
    # retains the names of its input, the data.frame call at the end correctly
    # builds the column names to be of the form "variable_name.value".
    colnames(dummies) <- lvls

    return(dummies)
  }) %>% data.frame

  return(data)
}

#' From ID strings for variants
#'
#' @param data Variant data
#'
#' @return A vector containing ID strings generated from corresponding variants input
form_variant_ids <- function(data) {

  if (!is.data.frame(data)) stop("`data` must be a data.frame")

  id_cols <- c("X.Chrom", "Pos", "Ref", "Alt", "FeatureID", CONSEQUENCE_COLUMN)

  return(
    apply(
      data[, id_cols],
      MARGIN = 1,
      function(x) paste0(x, collapse = ":")
    )
  )

}

#' Impute with specific values that can be considered appropriate for the feature a-priori
#'
#' @param data data.frame with columns corresponding to names in default_imputations
#' @param default_imputations A list mapping column names to a value with which the column should be imputed
#'
#' @return Same as `data` but with specified columns imputed by specified values
a_priori_impute <- function(data, default_imputations) {

  if (!is.data.frame(data)) stop("`data` must be a data.frame")
  if (!is.list(default_imputations)) stop("`default_imputations` must be a list")
  #if (!all(sapply(names(default_imputations), FUN = function(x) is.numeric(default_imputations[[x]]) == is.numeric(data[[x]])))) stop("Imputed values must match data by class")
  #if (!all(sapply(names(default_imputations), FUN = function(x) is.character(default_imputations[[x]]) == is.character(data[[x]])))) stop("Imputed values must match data by class")
  #if (any(sapply(default_imputations, is.factor))) stop("Please pass categorical variables as character vectors")
  #if (any(sapply(data, is.factor))) stop("Please pass categorical variables as character vectors")

  for (col in enumerate(default_imputations)) {
    miss_ind <- is.na(data[, col$name])
    data[miss_ind, col$name] <- rep(col$value, sum(miss_ind))
  }

  return(data)

}

#' Same as base `table` but with margin sums added
#'
#' @param ... Arguments to pass to `table`
#'
#' @return Output of `table` with margin sums added
table_with_margin <- function(...) {
  tabl <- table(...)
  tabl %<>% cbind(ALL_ = rowSums(tabl))
  tabl %<>% rbind(ALL_ = colSums(tabl))
  return(tabl)
}

#' Select features from data where categorical features have been dummy-coded
#'
#' This function keeps all numeric features listed in `numeric_features` as well as
#' features whose names have been derived from those listed in `categorical_features`
#' by use of `dummify_categoricals`. All other columns are dropped.
#'
#' @param data A data.frame with features listed in the latter arguments, as well as
#' those generated via `dummify_categoricals` from those listed in `categorical_features`.
#' @param numeric_features Character vector of numeric features to keep
#' @param categorical_features Character vector of categorical features from which dummy
#' features have been generated
#'
#' @return A data.frame derived from `data` with columns as described above
select_features_after_dummy_coding <- function(data, numeric_features, categorical_features) {

  if (!is.vector(numeric_features)) stop("`numeric_features` must be a vector")
  if (!is.character(numeric_features)) stop("`numeric_features` must be a character vector")
  if (!is.vector(categorical_features)) stop("`categorical_features` must be a vector")
  if (!is.character(categorical_features)) stop("`categorical_features` must be a character vector")
  if (!all(numeric_features %in% colnames(data))) stop("`numeric_features` must be a subset of `data` column names")
  if (!all(categorical_features %in% colnames(data))) stop("`categorical_features` must be a subset of `data` column names")
  if (!all(sapply(data[numeric_features], is.numeric))) stop("Non-numeric columns included in `numeric_features`")
  if (!all(sapply(data[categorical_features], is.character))) stop("Non-character columns included in `categorical_features`")

  columns <- colnames(data)
  features_with_categorical_variable_prefix <- lapply(categorical_features, . %>% find_dummies(columns))

  dummy_features <- unique(unlist(features_with_categorical_variable_prefix))
  features <- c(numeric_features, dummy_features)
  data <- data[, features, drop = FALSE]

  return(data)
}
