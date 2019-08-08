
split_train_test <- function(dataframe, proportion) {
  
  stopifnot(class(dataframe) == "data.frame")
  stopifnot(nrow(dataframe) > 2)
  stopifnot(is.numeric(proportion))
  stopifnot(length(proportion) == 1)
  stopifnot(proportion < 1.0)
  stopifnot(proportion > 0.0)
  
  index <- runif(nrow(dataframe), min = 0, max = 1) < proportion
  
  row.names(dataframe) <- NULL
  
  datasplit <- list(
    training_set = dataframe[index, ],
    test_set = dataframe[!index, ]
  )
  
  return(datasplit)
}


