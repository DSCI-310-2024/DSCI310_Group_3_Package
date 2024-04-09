library(testthat)
library(dplyr)
library(recipes)
library(workflows)
library(parsnip)
library(yardstick)
library(tibble) # For creating tibbles

# Assuming the run_lm_workflow function is defined elsewhere and is being sourced or is in the environment

# Test data preparation
correct_train_data <- tibble(
  CWB_2021 = rnorm(100), 
  Income_2021 = rnorm(100),
  Education_2021 = rnorm(100),
  Housing_2021 = rnorm(100),
  Labour_Force_Activity_2021 = rnorm(100)
)

correct_test_data <- tibble(
  CWB_2021 = rnorm(50),  
  Income_2021 = rnorm(50),
  Education_2021 = rnorm(50),
  Housing_2021 = rnorm(50),
  Labour_Force_Activity_2021 = rnorm(50)
)

# Incorrect input data for testing error handling
incorrect_train_data <- "Not a data frame"
incorrect_test_data <- TRUE

# Tests
test_that("`run_lm_workflow` returns a list with `test_summary` and `lm_fit`", {
  result <- run_lm_workflow(correct_train_data, correct_test_data)
  expect_true("test_summary" %in% names(result))
  expect_true("lm_fit" %in% names(result))
  expect_s3_class(result$test_summary, "data.frame")
})

test_that("`run_lm_workflow` throws an error with incorrect input types", {
  expect_error(run_lm_workflow(incorrect_train_data, correct_test_data))
  expect_error(run_lm_workflow(correct_train_data, incorrect_test_data))
})

test_that("`test_summary` contains the correct structure", {
  result <- run_lm_workflow(correct_train_data, correct_test_data)
  test_summary <- result$test_summary
  
  # Update the expected columns to match the actual output of metrics()
  expected_columns <- c(".metric", ".estimator", ".estimate")
  
  # The test now checks for the presence of expected structural column names
  expect_true(all(expected_columns %in% names(test_summary)),
              info = paste("Expected columns:", paste(expected_columns, collapse = ", "),
                           "but found:", paste(names(test_summary), collapse = ", ")))
})



