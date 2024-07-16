mock_data <- data.frame(
  a = 1:10/10,
  b = 10:1/10,
  c = c(0.1, 0.6, 0.22, 0.1, 0.2, 0.1, 0.8, 0.8, 0.0, 1),
  d = c(rep("a", 5), rep("b", 5)),
  o = factor(c("a", rep("b", 3), "a", rep("a", 3), "b", "b"), c("a", "b"))
)
mock_data_w_missingness <- mock_data
mock_data_w_missingness[c(2,3,6,8), c(2)] <- NA

mock_imp_tree <- list(
  knnImputation = list(`1` = mock_data[,-5],
                       `2` = mock_data[,-5]),
  zero_imp = list(`1` = mock_data[,-5])
)
mock_model_tree <- list(
  knnImputation = list(caret::train(x = mock_data[,-5], y = mock_data[,5], "rf")),
  zero_imp = list(caret::train(x = mock_data[,-5], y = mock_data[,5], "rf"),
                  caret::train(x = mock_data[,-5], y = mock_data[,5], "rf"))
)
mock_pred_tree <- list(
  knnImputation = list(model_1 = list(imp_1 = c(0.65, 0.174, 0.086, 0.24),
                                      imp_2 = c(0.56, 0.714, 0.860, 0.42))),
  zero_imp = list(model_1 = list(imp_1 = c(0.66, 0.376, 0.344, 0.224)),
                  model_2 = list(imp_1 = c(0.618, 0.328, 0.310, 0.272)))
)

test_that("performance_stats produces correct output", {
  observed_output <- performance_stats(predictions = mock_pred_tree, outcome = factor(c("a", "b", "a", "b"), c("a", "b")))
  observed_output_metrics <- observed_output[!names(observed_output) %in% c("TP", "FP", "FN", "TN")]
  expect_true(all(unlist(observed_output_metrics, recursive = TRUE) %>% is.na %>% `!`))
  expect_true(all(unlist(observed_output_metrics, recursive = TRUE) >= -1))
  expect_true(all(unlist(observed_output_metrics, recursive = TRUE) <= 1))
})
mock_perf_tree <- list(TP = list(knnImputation = list(model_1 = list(imp_1 = 1))), FP = list(knnImputation = list(model_1 = list(imp_1 = 2))))

turn_table_expected_output_tp <- data.frame(method = "knnImputation", model_ix = "model_1", test_completion_ix = "imp_1", value = 1) %>% magrittr::set_rownames("knnImputation:model_1:imp_1")
turn_table_expected_output_fp <- data.frame(method = "knnImputation", model_ix = "model_1", test_completion_ix = "imp_1", value = 2) %>% magrittr::set_rownames("knnImputation:model_1:imp_1")
test_that("transform_perf_tree_to_table produces correct output", {
  expect_equal(transform_perf_tree_to_table(perf_tree = mock_perf_tree$TP), turn_table_expected_output_tp)
  expect_equal(transform_perf_tree_to_table(perf_tree = mock_perf_tree$FP), turn_table_expected_output_fp)
})

test_that("merge_tables produces correct output", {
  expected_output <- data.frame(method = "knnImputation", model_ix = "model_1", test_completion_ix = "imp_1", FP = 2, TP = 1)
  expect_equal(merge_tables(tables = list(FP = turn_table_expected_output_fp, TP = turn_table_expected_output_tp)),
               expected_output)
})
