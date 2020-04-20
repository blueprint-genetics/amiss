library(magrittr)
source("R/recursive_application.R")

compute_rmse <- function(imputer_path, orig_data_path, simu_data_path, output_filename) {

  imputer <- readRDS(file = imputer_path, refhook = function(x) .GlobalEnv)
  orig_data <- read.csv(orig_data_path, row.names = 1, as.is = TRUE)
  simu_data <- read.csv(simu_data_path, row.names = 1, as.is = TRUE)

  na_prop <- function(data) {
    sum(is.na(data)) / (NCOL(data) * NROW(data))
  }
  simu_data_na_prop <- na_prop(simu_data)

  orig_sds   <- sapply(orig_data, . %>% sd(na.rm = TRUE))
  orig_means <- sapply(orig_data, . %>% na.omit %>% mean)

  orig_miss <- is.na(orig_data)
  simu_miss <- is.na(simu_data)
  miss_diff <- simu_miss & !orig_miss
  colnames(miss_diff) <- colnames(orig_miss)

  # Note that in simulations, only one completed dataset is created per method
  comp_data <- lapply(imputer, function(m) m[[1]]$trainingData)
  # Caret seems to add the outcome also into the data.frame, so let's only keep features
  comp_data <- lapply(comp_data, function(data) data <- data[, colnames(data) %>% intersect(colnames(orig_data))])

  orig_scaled <- scale(orig_data, center = orig_means, scale = orig_sds)

  rmse_per_method <- lapply(names(comp_data), function(method) {

    comp_colnames <- colnames(comp_data[[method]])
    comp_scaled <- comp_data[[method]]
    comp_scaled <- scale(comp_data[[method]], center = orig_means[comp_colnames], scale = orig_sds[comp_colnames])
    # RMSE doesn't make sense for missingness indicators
    if (method == "missingness_indicators") {
      rmse_per_col <- rep(NA, NCOL(comp_data))
    } else {
      rmse_per_col <- sapply(colnames(comp_scaled), function(column) {
        e <- orig_scaled[miss_diff[, column], column] - comp_scaled[miss_diff[,column], column]
        return(sqrt(mean(e*e)))
      }) %>% set_names(colnames(comp_scaled))
    }
    return(rmse_per_col)

  }) %>% set_names(names(comp_data))
  rmse_per_method <- do.call(rbind, rmse_per_method)
  rmse_per_method <- cbind(rmse_per_method, na_prop = simu_data_na_prop)

  write.csv(x = rmse_per_method, file = output_filename)
}

