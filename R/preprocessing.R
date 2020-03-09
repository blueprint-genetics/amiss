library(magrittr)

#' Partition dataset into a training and a test set
#'
#' @param dataframe A data.frame that will be partitioned into two.
#' @param proportion Proportional size of training set compared to test set
#'
#' @return Two-element list containing the training and test sets.
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

  row.names(dataframe) <- NULL

  datasplit <- list(
    training_set = dataframe[index, ],
    test_set = dataframe[!index, ]
  )

  return(datasplit)
}

missingness_indicators <- function(dataframe) {

  stopifnot(class(dataframe) == "data.frame")

  miss_ind <- is.na(dataframe)

  return(miss_ind)
}

#' Compute numeric labels from ClinVar classifications
#'
#' @param class_vector Character vector containing ClinVar classifications
#' @param positive_classes Character vector designating the classifications that are considered positive
#' @param negative_classes Character vector designating the classifications that are considered negative
#'
#' @return Numeric vector containing 1.0 for each pathogenic classification and
#' 0.0 for each non-pathogenic classification.
compute_numeric_labels <- function(class_vector, positive_classes, negative_classes) {

  stopifnot(class(class_vector) == "character")
  stopifnot(length(class_vector) > 0)

  # If classification is not any of these, raise and error
  undefined_class_ind <- !class_vector %in% c(positive_classes, negative_classes)
  if (any(undefined_class_ind)) {
    error_msg <- "Undefined class(es) in computing numeric labels: " %>% paste0(unique(class_vector[undefined_class_ind]))
    stop(error_msg)
  }

  positive_class_indicator <- ifelse(class_vector %in% positive_classes, "positive", "negative")

  return(positive_class_indicator)
}

#' Convert categorical variables into sets of dummy variables
#'
#' @param dataframe A data.frame containing only character columns
#'
#' @return A data.frame containing dummy variables for each original column
dummify_categoricals <- function(dataframe) {

  stopifnot(class(dataframe) == "data.frame")
  # Check that all columns are of type character
  stopifnot(dataframe %>% sapply(is.character) %>% all)

  dataframe <- lapply(dataframe, function(col) {
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

  return(dataframe)
}

form_variant_ids <- function(data) {

  if (!is.data.frame(data)) stop("`data` must be a data.frame")

  id_cols <- c("X.Chrom", "Pos", "Ref", "Alt", "FeatureID")

  return(
    apply(
      data[, id_cols],
      MARGIN = 1,
      function(x) paste0(x, collapse = ":")
    )
  )

}

a_priori_impute <- function(data, default_imputations) {

  if (!is.data.frame(data)) stop("`data` must be a data.frame")
  if (!is.list(default_imputations)) stop("`default_imputations` must be a list")

  for (col in enumerate(default_imputations)) {
    miss_ind <- is.na(data[, col$name])
    data[miss_ind, col$name] <- rep(col$value, sum(miss_ind))
  }

  return(data)

}

table_with_margin <- function(...) {
  tabl <- table(...)
  tabl %<>% cbind(ALL_ = rowSums(tabl))
  tabl %<>% rbind(ALL_ = colSums(tabl))
  return(tabl)
}

detect_imbalanced_consequence_classes <- function(data, outcome) {

  if (!is.data.frame(data)) stop("`data` must be a data.frame")
  if (!is.vector(outcome)) stop("`outcome` must be a vector")
  if (!is.character(outcome)) stop("`outcome` must be a character vector")

  class_distribution <- table_with_margin(data$Consequence.x, outcome, useNA = "always") %>% as.data.frame
  prop_pathg <- class_distribution[,"positive"]/(class_distribution[,"negative"] + class_distribution[,"positive"])
  unbalanced_conseqs <- class_distribution[(prop_pathg < 0.05 | prop_pathg > 0.95) & !is.na(prop_pathg), ]

  return(unbalanced_conseqs)

}