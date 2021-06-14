mock_data <- data.frame(
  a = 1:10/10,
  b = c(5:1/10, 6:10/10),
  c = c(0.1, 0.6, 0.22, 0.1, 0.2, 0.1, 0.8, 0.8, 0.0, 1),
  d = c(rep("a", 5), rep("b", 5)),
  o = factor(c("a", rep("b", 3), "a", rep("a", 3), "b", "b"), c("a", "b"))
)
mock_data_w_missingness <- mock_data
mock_data_w_missingness[c(2,3,6,8), c(2)] <- NA

mock_imp_tree <- list(
  knnImputation = list(imp_hp_1 = list(completed_datasets = list(`1` = mock_data[,-5]))),
  zero_imp = list(imp_hp_1 = list(completed_datasets = list(`1` = zero_imp(mock_data_w_missingness[,-5]))))
)
mock_imp_tree_knn_failed <- list(
  knnImputation = list(imp_hp_1 = list(NULL)),
  zero_imp = list(imp_hp_1 = list(completed_datasets = list(`1` = zero_imp(mock_data_w_missingness[,-5]))))
)

test_that("check_method_results succeeds on no empty imputations", {
  expect_silent(check_method_results(mock_imp_tree))
})
test_that("check_method_results succeeds on no empty imputations", {
  expect_false(all(check_method_results(mock_imp_tree_knn_failed)))
})

test_that("impute_over_grid produces imputed datasets", {
  observed_output <- impute_over_grid(data = mock_data_w_missingness,
                                      hyperparameter_grids = list(
                                        knnImputation = data.frame(k = 1:2),
                                        pmm = data.frame(donors = 1, ridge = 0.001, matchtype = 1)
                                      ),
                                      seed = 1,
                                      times = 2,
                                      iterations = 2)
  expect_true(
    all(purrr::map_depth(observed_output,
                  .depth = 2,
                  . %>% magrittr::extract2("completed_datasets") %>% is.null) %>% unlist %>% `!`
    )
  )
  expect_equal(
    purrr::map_depth(observed_output, .depth = 2,
              . %>% magrittr::extract2("completed_datasets") %>% sapply(is.data.frame)) %>% unlist %>% sum,
    4
  )
})

test_that("impute_with_hyperparameters produces imputed datasets", {

  observed_output <- impute_with_hyperparameters(mock_data_w_missingness,
                                                       method_name = "knn",
                                                       impute_function = run_knn,
                                                       hyperparameters = data.frame(k = 1L:2L),
                                                       seed = 1, times = 2)
  expect_true(
    all(
      purrr::map_depth(observed_output,
                  .depth = 1,
                  . %>% magrittr::extract2("completed_datasets") %>% is.null) %>% unlist %>% `!`
    )
  )
  expect_equal(purrr::map_depth(observed_output, .depth = 3, is.data.frame) %>% unlist %>% sum, 2)

})

test_that("method_to_function works", {
  expect_identical(method_to_function(method = "pmm"), run_mice_pmm)
})
test_that("method_to_function fails correctly", {
  expect_identical(method_to_function(method = "inexistent"), null_method)
  expect_error(method_to_function(method = null_method), "EXPR must be a length 1 vector")
})

mi_expected_output <- data.frame(
  a = 1:10/10,
  b = c(0.5, 0, 0, 0.2, 0.1, 0, 0.7, 0,0.9,1.0),
  c = c(0.1, 0.6, 0.22, 0.1, 0.2, 0.1, 0.8, 0.8, 0.0, 1),
  d = c(rep("a", 5), rep("b", 5)),
  o = factor(c("a", rep("b", 3), "a", rep("a", 3), "b", "b"), c("a", "b")),
  a_missing = c(FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE),
  b_missing = c(FALSE,TRUE,TRUE,FALSE,FALSE,TRUE,FALSE,TRUE,FALSE,FALSE)
)
mi_observed_output <- missingness_indicators(data = mock_data_w_missingness)
test_that("missingness_indicators produces missingness indicators", {
  expect_equivalent(mi_observed_output, mi_expected_output)
})

test_that("missingness_indicator produces the same columns on a separate set", {
  # This test describes a situation where a separate set would actually lead to more unique missingness columns.
  # It is an unfortunate situation, but cannot be avoided unless we specifically want to keep *all* indicator columns
  # regardless of their equality.

  modified_mock_data <- mock_data_w_missingness
  modified_mock_data[1, "a"] <- NA # Since a, c and d were all observed on the training set and thus equal, only the one describing a was kept.
  observed_output <- missingness_indicators(modified_mock_data, remove_vector = attr(mi_observed_output, IMPUTATION_REUSE_PARAMETERS))
  expected_output <- mi_expected_output %>% (function(x) {
    # Now a will have a missing value, and implicitly the data looks like c and d would have as well, even if it isn't true.
    x$a[1] <- 0
    x$a_missing[1] <- TRUE
    return(x)
  })
  expect_equivalent(observed_output, expected_output)
  expect_true(observed_output$a_missing[1])
})

test_that("produce_outlier produces correct output", {
  expect_equal(produce_outlier(x = 1:10), 90) # 10 * (10-9)
})

test_that("produce_outlier fails correctly", {
  expect_error(produce_outlier(x = c("a", "b", "c")), "must be a numeric vector") # 10 * (10-9)
  expect_error(produce_outlier(x = list(1:9)), "must be a numeric vector") # 10 * (10-9)
  expect_error(produce_outlier(x = data.frame(a = 1:9, b = 11:19)), "must be a numeric vector") # 10 * (10-9)
})

test_that("reimpute produces correct output", {
  expected_output <- mock_data[,1:2]
  expected_output[c(2,3,6,8), c(2)] <- 10
  expect_equal(reimpute(dataframe = mock_data_w_missingness[,1:2], value = list(a = 0, b = 10)),
               expected_output)
})
test_that("reimpute fails correctly", {
  expect_error(reimpute(dataframe = mock_data_w_missingness[,1:2], value = list(a = 0, b = 0, f = 10)), "do not match") # extra name f
  expect_error(reimpute(dataframe = mock_data_w_missingness[,1:2], value = list(a = 0)), "do not match") # missing name b
})

test_that("run_bpca imputes", {
  expect_true(
    run_bpca(data = mock_data_w_missingness[,1:2], hyperparams = list(nPcs = 1))$
      completed_datasets$`1` %>% is.null %>% `!`
  )
})

mock_data_w_missingness_2 <- mock_data_w_missingness
mock_data_w_missingness_2$a[4] <- NA # apparently knnImpute crashes if only one column has missing values
mock_data_w_missingness_2$b <- c(2,4,6,NA,10,9,7,5,3,1)
mock_data_w_missingness_2$c <- c(4,2,2,2,10,NA,7,NA,3,1)
  #mock_data_w_missingness_2 <- rbind(mock_data_w_missingness_2, mock_data_w_missingness_2, mock_data_w_missingness_2)
test_that("run_knn imputes", {
  expect_true(
    run_knn(data = mock_data_w_missingness_2[,1:2], hyperparams = list(k = 1))$
      completed_datasets$`1` %>% is.null %>% `!`
  )
  expect_true(
    run_knn(data = mock_data_w_missingness_2[,1:2], hyperparams = list(k = 1))$
      completed_datasets$`1` %>% is.na %>% `!` %>% all
  )
})
test_that("run_mice_pmm imputes", {

  expect_true(
    run_mice_pmm(data = mock_data_w_missingness_2[,1:3], hyperparameters = list(donors = 1), times = 1, iterations = 1)$
      completed_datasets$`1` %>% is.null %>% `!`
  )
  expect_true(
    run_mice_pmm(data = mock_data_w_missingness_2[,1:3], hyperparameters = list(donors = 1), times = 1, iterations = 1)$
      completed_datasets$`1` %>% is.na %>% `!` %>% all
  )
})

test_that("run_missforest imputes", {

  expect_true(
    run_missforest(data = mock_data_w_missingness_2[,1:3], hyperparams = list(ntree = 3), times = 1, iterations = 1)$
      completed_datasets$`1` %>% is.null %>% `!`
  )
  expect_true(
    run_missforest(data = mock_data_w_missingness_2[,1:3], hyperparams = list(ntree = 3), times = 1, iterations = 1)$
      completed_datasets$`1` %>% is.na %>% `!` %>% all
  )
})
test_that("mean_imp produces correct output", {
  expected_output <- mock_data_w_missingness[,1:2]
  expected_output$b[expected_output$b %>% is.na] <- mean(na.omit(expected_output$b))
  expect_equivalent(mean_imp(mock_data_w_missingness[,1:2]), expected_output)
})
test_that("median_imp produces correct output", {
  expected_output <- mock_data_w_missingness[,1:2]
  expected_output$b[expected_output$b %>% is.na] <- median(na.omit(expected_output$b))
  expect_equivalent(median_imp(mock_data_w_missingness[,1:2]), expected_output)
})
test_that("outlier_imp produces correct output", {
  expected_output <- mock_data_w_missingness[,1:2]
  expected_output$b[expected_output$b %>% is.na] <- (max(na.omit(expected_output$b)) - min(na.omit(expected_output$b)))*10
  expect_equivalent(outlier_imp(mock_data_w_missingness[,1:2]), expected_output)
})
test_that("zero_imp produces correct output", {
  expected_output <- mock_data_w_missingness[,1:2]
  expected_output$b[expected_output$b %>% is.na] <- 0
  expect_equivalent(zero_imp(mock_data_w_missingness[,1:2]), expected_output)
})
test_that("max_imp produces correct output", {
  expected_output <- mock_data_w_missingness[,1:2]
  expected_output$b[expected_output$b %>% is.na] <- max(na.omit(expected_output$b))
  expect_equivalent(max_imp(mock_data_w_missingness[,1:2]), expected_output)
})
test_that("min_imp produces correct output", {
  expected_output <- mock_data_w_missingness[,1:2]
  expected_output$b[expected_output$b %>% is.na] <- min(na.omit(expected_output$b))
  expect_equivalent(min_imp(mock_data_w_missingness[,1:2]), expected_output)
})

