library(testthat)
source("R/preprocessing.R")

# split_train_test
set.seed(1)
mock_data <- data.frame(
  a = 1:10,
  b = 10:1,
  c = c(rep("a", 5), rep("b", 5)),
  d = c(rep("b", 5), rep("a", 4), NA),
  stringsAsFactors = FALSE
)
mock_split <- split_train_test(mock_data, 0.7)

test_that("split_train_test produces correct length outputs", {
  expect_equal(length(mock_split), 2)
  expect_equal(NROW(mock_split$training_set) + NROW(mock_split$test_set), 10)
})

test_that("split_train_test partitions the data", {
  expect_equal(sort(mock_data$a), sort(c(mock_split$training_set$a, mock_split$test_set$a)))
})

test_that("split_train_test fails correctly", {
  expect_error(split_train_test(mock_data, 1.0))
  expect_error(split_train_test(mock_data, 0.0))
  expect_error(split_train_test(mock_data, -1.0))
  expect_error(split_train_test(mock_data, 1.1))
  expect_error(split_train_test(mock_data, c(0.5, 0.5)))
  expect_error(split_train_test(mock_data, TRUE))
})

# code_labels
test_that("code_labels interprets known labels correctly", {
  pos <- c("yes", "affirmative")
  neg <- c("no", "nay")
  mock_labels <- c("affirmative", "no", "nay", "yes")
  expect_equal(code_labels(mock_labels, pos, neg), c("positive", "negative", "negative", "positive"))
})

test_that("code_labels fails correctly", {
  expect_error(code_labels("yes", "y", "n"))
  expect_error(code_labels(1, "y", "n"))
  expect_error(code_labels(1, 1, 0))
  expect_error(code_labels(data.frame(a = "y"), 1, 2))
})

# dummify_categoricals
test_that("dummify_categoricals produces correct output", {
  dummy_mock_data <- data.frame(
    c.a = c(rep(1,5), rep(0,5)),
    c.b = c(rep(0,5), rep(1,5)),
    d.b = c(rep(1,5), rep(0,4), 0),
    d.a = c(rep(0,5), rep(1,4), 0),
    d.NA = c(rep(0, 9), 1)
  )

  expect_equal(dummify_categoricals(mock_data[c("c", "d")]), dummy_mock_data)
})

test_that("dummify_categoricals fails correctly", {
  expect_error(dummify_categoricals(mock_data)) # Can't pass also numeric columns
  expect_error(dummify_categoricals(mock_data %>% sapply(factor) %>% data.frame)) # Can't pass factors
})

# form_variant_ids
mock_variant_data <- data.frame(
  X.Chrom = c("1", "X"),
  Pos = c(0, 1),
  Ref = c("A", "G"),
  Alt = c("C", "T"),
  FeatureID = c("ENS1", "ENS2")
)
test_that("form_variant_ids produces correct output", {
  expect_equal(form_variant_ids(mock_variant_data), c("1:0:A:C:ENS1", "X:1:G:T:ENS2"))
})
test_that("form_variant_ids fails correctly", {
  expect_error(dummify_categoricals(123))
  expect_error(dummify_categoricals(mock_variant_data[c(),, drop = FALSE]))
})

# a_priori_impute
mock_miss_data <- data.frame(
  a = 1:10,
  b = 10:1,
  c = c(rep("a", 5), rep("b", 5)),
  d = c(rep("b", 5), rep("a", 4), NA),
  stringsAsFactors = FALSE
)
mock_miss_data[c(1, 3, 5, 7), c("a")] <- NA
mock_default_imps <- list(a = 0, d = "b")
mock_miss_data_imputed <- data.frame(
  a = c(0, 2, 0, 4, 0, 6, 0, 8, 9, 10),
  b = 10:1,
  c = c(rep("a", 5), rep("b", 5)),
  d = c(rep("b", 5), rep("a", 4), "b"),
  stringsAsFactors = FALSE
)

test_that("a_priori_impute produces correct output", {
  expect_equal(a_priori_impute(mock_miss_data, mock_default_imps), mock_miss_data_imputed)
})

test_that("a_priori_impute fails correctly", {
  expect_error(a_priori_impute(mock_miss_data, list(a = "b"))) # Wrong type for imputed value
  expect_error(a_priori_impute(mock_miss_data, list(d = factor("b")))) # No factors
  expect_error(a_priori_impute(data.frame(mock_miss_data$d), list(d = "b"))) # No factors
})

# table_with_margin
# test_that("table_with_margin produces correct output", {
#   expected_output <- c(
#     "     a b ALL_",
#     "a    4 0    4",
#     "b    0 5    5",
#     "ALL_ 4 5    9")
#
#   expect_equal(table_with_margin(mock_data$d, mock_data$d) %>% capture.output, expected_output)
# })

# detect_imbalanced_consequence_classes
test_that("detect_imbalanced_consequence_classes produces correct output", {
  mock_conseqs <- c(rep("y", 50), rep("x", 50))
  mock_outcomes <- c("positive", rep("negative", 49), rep("positive", 25), rep("negative", 25))
  expect_equal(rownames(detect_imbalanced_consequence_classes(mock_conseqs, outcome = mock_outcomes, 0.05)), "y")
  expect_equal(rownames(detect_imbalanced_consequence_classes(mock_conseqs, outcome = mock_outcomes, 0.005)), character(0)) # If none were that rare
})
