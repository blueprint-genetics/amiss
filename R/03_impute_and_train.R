library(magrittr)
library(futile.logger)
library(caret)
library(mice)

training_data <- read.csv("preprocessed_training_data.csv", as.is = TRUE)

nearzerovariance <- caret::nearZeroVar(training_data, saveMetrics = TRUE)

training_data <- training_data[, -nearzerovariance$nzv]

correlations <- cor(training_data, use = "pairwise.complete.obs")
correlations[is.na(correlations)] <- 0.0
highly_correlated_variables <- caret::findCorrelation(correlations)

training_data <- training_data[, -highly_correlated_variables]

outcome_col_index <- which(colnames(training_data) == "outcome")
outcome <- training_data$outcome

training_data <- training_data[, -outcome_col_index]
imputed_training_data <- mice::mice(data = training_data)

completed_training_data <- mice::complete(imputed_training_data, action = 1)

hyperparameter_grid <- data.frame(mtry = 1:10 * 4 - 1)

training_settings <- trainControl(classProbs = TRUE,
                                  verboseIter = TRUE,
                                  method = "oob",
                                  returnResamp = "all")

train_args <- list(x = completed_training_data,
                   y = outcome,
                   method = "rf",
                   preProcess = c("center", "scale"),
                   trControl = training_settings,
                   tuneGrid = hyperparameter_grid)

rf_model <- do.call(caret::train, train_args)
