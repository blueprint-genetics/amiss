library(futile.logger)
library(testthat)

source("R/training_functions.R")
flog.threshold(DEBUG)

library(caret)

set.seed(1)
mock_data <- data.frame(
  a = 1:10/10,
  b = 10:1/10,
  c = c(0.1, 0.6, 0.22, 0.1, 0.2, 0.1, 0.8, 0.8, 0.0, 1),
  d = c(rep("a", 5), rep("b", 5)),
  o = factor(c("a", rep("b", 3), "a", rep("a", 3), "b", "b"), c("a", "b"))
) 
mock_data <- rbind(mock_data,mock_data, mock_data)
rf_model <- caret::train(mock_data[1:15,-5], mock_data[1:15,5], "rf", trControl = trainControl(classProbs = TRUE, method = "none"))
lr_model <- caret::train(mock_data[1:15,-5], mock_data[1:15,5], "glm", trControl = trainControl(classProbs = TRUE, method = "none"))
test_that("extract_mcc_performance produces correct output", {
  rf_mcc <- extract_mcc_performance(model = rf_model)
  lr_mcc <- extract_mcc_performance(model = lr_model)
  
  expect_equal(length(rf_mcc), 1)
  expect_equal(length(lr_mcc), 1)
  expect_true(is.numeric(rf_mcc))
  expect_true(is.numeric(lr_mcc))
})
test_that("extract_mcc_performance fails correctly", {
  expect_error(extract_mcc_performance(c("a", "b", "a")), "`model` must be a caret `train` object")
})

#inf_NULLs
test_that("inf_NULLs produces correct output", {
  expect_equal(inf_NULLs(x = list(a = NULL, b = NULL, NaN, 1.2, 1.3)),
               list(a = -Inf, b = -Inf, -Inf, 1.2, 1.3))
})
test_that("inf_NULLs fails correctly", {
  expect_error(inf_NULLs(x = c(1, NULL, 2)), "must be a list")
  expect_error(inf_NULLs(x = list(a = list(1,2,NA))), "all numeric or NULL components")
  expect_error(inf_NULLs(x = list(a = "a")), "all numeric or NULL components")
})

mock_training_function <- function(data, outcome, control, grid) {return(data)}
mock_tr <- mock_data[, -5]
mock_imps <- list(
  a = list(
    hp_row_1 = list(imputations = NULL, completed_datasets = list(`1` = mock_tr, `2` = mock_tr))
  ), 
  b = list(
    hp_row_1 = list(imputations = NULL, completed_datasets = list(`1` = mock_tr, `2` = mock_tr)),
    hp_row_2 = list(imputations = NULL, completed_datasets = list(`1` = mock_tr, `2` = mock_tr))
  )
)
mock_output <- data.frame(X1 = c("a", "a", "b", "b", "b", "b"), 
                          X2 = c("hp_row_1", "hp_row_1", "hp_row_1", "hp_row_1", "hp_row_2" ,"hp_row_2"),
                          X3 = c("1","2","1","2","1","2"),
                          stringsAsFactors = FALSE)
mock_output$values <- rep(list(mock_tr), 6)

#loop_models
test_that("loop_models produces correct output", {
  output <- loop_models(mock_training_function, classifier_name = "mock", imputations = mock_imps, control = trainControl(), grid = data.frame("grid"), outcome = mock_data[,5], seed = 1)
  output_df <- tree_as_df(tree = output, x_class = "data.frame") # Much easier to test via dfs rather than by checking equivalence of trees
  expect_equivalent(output_df, mock_output)
})
test_that("loop_models fails correctly", {
  expect_error(loop_models(mock_training_function, classifier_name = 1, imputations = mock_imps, control = trainControl(), grid = data.frame("grid"), outcome = mock_data[,5], seed = 1),
               "must be a character vector")
  expect_error(loop_models(mock_training_function, classifier_name = "a", imputations = mock_imps, control = trainControl(), grid = list(a = c(1,2)), outcome = mock_data[,5], seed = 1),
               "must be a data.frame")
  expect_error(loop_models(mock_training_function, classifier_name = "a", imputations = mock_imps, control = trainControl(), grid = data.frame("grid"), outcome = as.character(mock_data[,5]), seed = 1),
               "must be a factor")
  expect_error(loop_models(mock_training_function, classifier_name = "a", imputations = mock_imps, control = trainControl(), grid = data.frame("grid"), outcome = mock_data[,5], seed = NULL),
               "numeric vector of length 1")
})

test_that("sample_max produces correct output", {
  set.seed(1)
  expect_equivalent(sample_max(data.frame(a = 1:10), 4L), data.frame(a = c(9,4,7,1)))
  expect_equal(sample_max(x = data.frame(a = 1:8), size = 10L), data.frame(a = 1:8))
})
test_that("sample_max fails correctly", {
  expect_error(sample_max(x = list(a = 1:4, b = 1:8), size = 4L), "must be a data.frame")
  expect_error(sample_max(x = data.frame(a = 1:8), size = -1L), "invalid 'size' argument")
})

mock_model_tree <- list(knnImputation = list(imp_hp_1 = list(`1` = rf_model, `2` = lr_model), imp_hp_2 = list(`1` = lr_model, `2` = lr_model)),
                        zero_imp = list(imp_hp_1 = list(`1` = rf_model, `2` = lr_model)))

test_that("get_model_performance_estimates produces correct output", {
  expect_equal(get_model_performance_estimates(models = mock_model_tree),
               list(knnImputation = list(imp_hp_1 = list(`1` = 1, `2` = 0.6000992), imp_hp_2 = list(`1` = 0.6000992, `2` = 0.6000992)),
                    zero_imp = list(imp_hp_1 = list(`1` = 1, `2` = 0.6000992))))
})
test_that("get_best_model_index produces correct output", {
  mock_perf_tree <- list(knnImputation = list(imp_hp_1 = list(`1` = 1, `2` = 0.6), imp_hp_2 = list(`1` = 0.9, `2` = 0.9)),
                         zero_imp = list(imp_hp_1 = list(`1` = 0, `2` = 0.6), imp_hp_2 = list(`1` = 0.1, `2` = 0.1)))
  
  expect_equal(get_best_model_index(mock_perf_tree),
               c(knnImputation = 2, zero_imp = 1))
})
test_that("select_from_tree produces correct output", {
  
  expect_equal(select_from_tree(tree = mock_model_tree, ix = c(knnImputation = 2, zero_imp = 1)),
               list(knnImputation = list(`1` = lr_model, `2` = lr_model),
                    zero_imp = list(`1` = rf_model, `2` = lr_model)))
})
mock_data_w_missingness <- mock_data
mock_data_w_missingness[c(2,3,6,8,14,15, 20,22, 30), c(2)] <- NA

source("R/imputation.R")
mock_data_imputed_knn <- impute_with_hyperparameters(mock_data_w_missingness, 
                                                    method_name = "knn",
                                                    impute_function = run_knn, 
                                                    hyperparameters = data.frame(k = 1L:2L),
                                                    seed = 1, times = 2)
mock_hp_tree <- list(knnImputation = data.frame(k=1L:2L), zero_imp = data.frame("zero_imp"))
mock_imp_tree <- list(
  knnImputation = mock_data_imputed_knn,
  zero_imp = list(imp_hp_1 = list(completed_datasets = list(`1` = zero_imp(mock_data_w_missingness))))
)

selected_hps <- select_hyperparams(hyperparams = mock_hp_tree, imputers = mock_imp_tree, ix = c(knnImputation = 2, zero_imp = 1))
test_that("select_hyperparams produces correct output", {
  expect_equivalent(selected_hps,
                    list(knnImputation = data.frame(k = 2), zero_imp = data.frame("zero_imp")))
})

bind_imp_expected_output <- list(knnImputation = data.frame(k = 1L:2L)[2,,drop = FALSE], zero_imp = data.frame("zero_imp"))
attr(bind_imp_expected_output$knnImputation, IMPUTATION_REUSE_PARAMETERS) <- mock_imp_tree$knnImputation$imp_hp_2$completed_datasets[[1]]
attr(bind_imp_expected_output$zero_imp, IMPUTATION_REUSE_PARAMETERS) <- attr(mock_imp_tree$zero_imp$imp_hp_1$completed_datasets$`1`, IMPUTATION_REUSE_PARAMETERS)
test_that("bind_imp_parameters_for_reuse produces correct output", {
  observed_output <- bind_imp_parameters_for_reuse(hyperparams = selected_hps, imputers = select_from_tree(mock_imp_tree, c(knnImputation = 2, zero_imp = 1)))
  expect_equal(observed_output, bind_imp_expected_output)
})

# test_that("select_best produces correct output", {
#   # This test would really benefit from mocking the functions called inside select_best.
#   expected_output <- list(models = list(knnImputation = list(`1` = rf_model, `2` = rf_model), zero_imp = list(`1` = lr_model, `2` = lr_model)),
#                           imputers = list(knnImputation = mock_imp_tree$knnImputation, zero_imp = mock_imp_tree$zero_imp),
#                           hyperparams = bind_imp_expected_output)
#   observed_output <- select_best(mock_model_tree, mock_imp_tree, mock_hp_tree)
# 
#   expect_equal(observed_output, expected_output)
# })
test_that("train_lr produces succeeds", {
  expect_s3_class(train_lr(data = mock_data[,-5], outcome = mock_data[,5], control = trainControl(), grid = data.frame()), "train")
})
test_that("train_rf produces succeeds", {
  expect_s3_class(train_rf(data = mock_data[,-5], outcome = mock_data[,5], control = trainControl(), grid = data.frame(mtry = 2)), "train")
})
