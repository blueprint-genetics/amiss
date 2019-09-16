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
#'
#' @return Numeric vector containing 1.0 for each pathogenic classification and 
#' 0.0 for each non-pathogenic classification.
compute_numeric_labels <- function(class_vector) {
  
  stopifnot(class(class_vector) == "character")
  stopifnot(length(class_vector) > 0)
  
  positive <- c("Likely_pathogenic", "Pathogenic", "Pathogenic,_drug_response", "Pathogenic/Likely_pathogenic,_drug_response")
  negative <- c("Benign", "Likely_benign", "Uncertain_significance")
  
  # If classification is not any of these, raise and error
  undefined_class_ind <- !class_vector %in% c(positive, negative)
  if (any(undefined_class_ind)) {
    error_msg <- "Undefined class(es) in computing numeric labels: " %>% paste0(unique(class_vector[undefined_class_ind]))
    flog.error(error_msg)
    stop(error_msg)
  }
  
  positive_class <- ifelse(class_vector %in% positive, 1.0, 0.0)
  
  return(positive_class)
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
    lvls <- unique(col) %>% na.omit
    
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
