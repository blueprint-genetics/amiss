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
  zero_imp = list(`1` = zero_imp(mock_data_w_missingness[,-5]))
)
mock_model_tree <- list(
  knnImputation = list(train(x = mock_data[,-5], y = mock_data[,5], "rf")),
  zero_imp = list(train(x = mock_data[,-5], y = mock_data[,5], "rf"), 
                  train(x = mock_data[,-5], y = mock_data[,5], "rf"))
)

test_that("prediction produces reasonable output", {
  observed_output <- prediction(models = mock_model_tree, mock_imp_tree, "a", seed = 1)
  
  expect_true(all(observed_output$knnImputation$model_1$imp_1 %>% is.na %>% `!`))
  expect_true(all(observed_output$knnImputation$model_1$imp_2 %>% is.na %>% `!`))
  expect_true(all(observed_output$zero_imp$model_1$imp_1 %>% is.na %>% `!`))
  expect_true(all(observed_output$zero_imp$model_2$imp_1 %>% is.na %>% `!`))
  
  expect_true(all(observed_output$knnImputation$model_1$imp_1 >= 0))
  expect_true(all(observed_output$knnImputation$model_1$imp_2 >= 0))
  expect_true(all(observed_output$zero_imp$model_1$imp_1 >= 0))
  expect_true(all(observed_output$zero_imp$model_2$imp_1 >= 0))
  
  expect_true(all(observed_output$knnImputation$model_1$imp_1 <= 1))
  expect_true(all(observed_output$knnImputation$model_1$imp_2 <= 1))
  expect_true(all(observed_output$zero_imp$model_1$imp_1 <= 1))
  expect_true(all(observed_output$zero_imp$model_2$imp_1 <= 1))
})
