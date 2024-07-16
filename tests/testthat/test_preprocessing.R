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
  expect_equal(length(mock_split), 3) # Training set, test set, index
  expect_equal(NROW(mock_split$training_set) + NROW(mock_split$test_set), 10)
})

test_that("split_train_test partitions the data", {
  expect_equal(sort(mock_data$a), sort(c(mock_split$training_set$a, mock_split$test_set$a)))
})

test_that("split_train_test fails correctly", {
  expect_error(split_train_test(mock_data, 1.0), "proportion < 1 is not TRUE", fixed = TRUE)
  expect_error(split_train_test(mock_data, 0.0), "proportion > 0 is not TRUE", fixed = TRUE)
  expect_error(split_train_test(mock_data, -1.0), "proportion > 0 is not TRUE", fixed = TRUE)
  expect_error(split_train_test(mock_data, 1.1), "proportion < 1 is not TRUE", fixed = TRUE)
  expect_error(split_train_test(mock_data, c(0.5, 0.5)), "length(proportion) == 1 is not TRUE", fixed = TRUE)
  expect_error(split_train_test(mock_data, TRUE), "is.numeric(proportion) is not TRUE", fixed = TRUE)
})

# code_labels
test_that("code_labels interprets known labels correctly", {
  pos <- c("yes", "affirmative")
  neg <- c("no", "nay")
  mock_labels <- c("affirmative", "no", "nay", "yes")
  expect_equal(code_labels(mock_labels, pos, neg), c("positive", "negative", "negative", "positive"))
})

test_that("code_labels fails correctly", {
  expect_error(code_labels("yes", "y", "n"), "Undefined class(es) in computing numeric labels: yes", fixed = TRUE)
  expect_error(code_labels(1, "y", "n"), "class(class_vector) == \"character\" is not TRUE", fixed = TRUE)
  expect_error(code_labels(1, 1, 0), "class(class_vector) == \"character\" is not TRUE", fixed = TRUE)
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
  expect_error(dummify_categoricals(mock_data$d), "`data` must be a data.frame") # Can't pass also numeric columns
  expect_error(dummify_categoricals(mock_data), "All columns in `data` must be character vectors") # Can't pass also numeric columns
  expect_error(dummify_categoricals(mock_data %>% sapply(factor) %>% data.frame), "All columns in `data` must be character vectors") # Can't pass factors
})

# form_variant_ids
mock_variant_data <- data.frame(
  X.Chrom = c("1", "X"),
  Pos = c(0, 1),
  Ref = c("A", "G"),
  Alt = c("C", "T"),
  FeatureID = c("ENS1", "ENS2"),
  Consequence.x = c("missense_variant", "intron_variant")
)
test_that("form_variant_ids produces correct output", {
  expect_equal(form_variant_ids(mock_variant_data), c("1:0:A:C:ENS1:missense_variant", "X:1:G:T:ENS2:intron_variant"))
})
test_that("form_variant_ids fails correctly", {
  expect_error(form_variant_ids(123), "`data` must be a data.frame")
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

# Removed these checks in a_priori_impute
# test_that("a_priori_impute fails correctly", {
#   expect_error(a_priori_impute(mock_miss_data, list(a = "b")), "Imputed values must match data by class") # Wrong type for imputed value
#   expect_error(a_priori_impute(mock_miss_data, list(d = factor("b"))), "Imputed values must match data by class") # No factors
#   expect_error(a_priori_impute(data.frame(mock_miss_data$d), list(d = "b")), "Imputed values must match data by class") # No factors
# })

# table_with_margin
test_that("table_with_margin produces correct output", {
  expected_output <- c(
    "     a b ALL_",
    "a    4 0    4",
    "b    0 5    5",
    "ALL_ 4 5    9")

  expect_equal(table_with_margin(mock_data$d, mock_data$d) %>% capture.output, expected_output)
})

mock_num_vars <- c("a", "b")
mock_cat_vars <- c("c", "d")
mock_data_w_dummies <- cbind(mock_data, dummify_categoricals(mock_data[mock_cat_vars]))

#drop_original_categorical_features
test_that("select_features drops non-dummy variables correctly", {
  expect_equal(
    colnames(select_features_after_dummy_coding(mock_data_w_dummies, mock_num_vars, mock_cat_vars)),
    c("a", "b", "c.a", "c.b", "d.b", "d.a", "d.NA")
  )
})

test_that("select_features fails correctly", {
  expect_error(select_features_after_dummy_coding(mock_data_w_dummies, c("a","c"), c("d")), "Non-numeric columns included in `numeric_features`") # Wrong types of column
  expect_error(select_features_after_dummy_coding(mock_data_w_dummies, c("a"), c("b", "d")), "Non-character columns included in `categorical_features`") # Wrong types of column
  expect_error(select_features_after_dummy_coding(mock_data_w_dummies, c(), c( "d")), "`numeric_features` must be a vector") # Empty numeric_features
  expect_error(select_features_after_dummy_coding(mock_data_w_dummies, c("a"), c()), "`categorical_features` must be a vector") # Empty categorical_features
  expect_error(select_features_after_dummy_coding(mock_data_w_dummies, list("a","c"), c("b", "d")), "`numeric_features` must be a character vector") # List instead of vector
  expect_error(select_features_after_dummy_coding(mock_data_w_dummies, c("a","c", "f"), c("b", "d")), "`numeric_features` must be a subset of `data` column names") # Inexistent column
  expect_error(select_features_after_dummy_coding(mock_data_w_dummies, c("a","c"), c("b", "d", "f")), "`categorical_features` must be a subset of `data` column names") # Inexistent column
})
