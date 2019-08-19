
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


