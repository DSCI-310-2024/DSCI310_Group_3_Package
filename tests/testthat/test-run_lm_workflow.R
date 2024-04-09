library(testthat) #most widely used package in R for test cases



# test data ---------------------------------------------------------------


# function input for tests (making the input data)
correct_train_data <- tibble::tibble(
  CWB_2021 = rnorm(100), 
  Income_2021 = rnorm(100),
  Education_2021 = rnorm(100),
  Housing_2021 = rnorm(100),
  Labour_Force_Activity_2021 = rnorm(100)
)

correct_test_data <- tibble::tibble(
  CWB_2021 = rnorm(50),  
  Income_2021 = rnorm(50),
  Education_2021 = rnorm(50),
  Housing_2021 = rnorm(50),
  Labour_Force_Activity_2021 = rnorm(50)
)

incorrect_train_data <- 123456789

incorrect_test_data <- 987654321

empty_train_data <- data.frame()

empty_test_data <- data.frame()


# expected function output (expected outcomes)
empty_summary_output <- data.frame()

example_correct_output <- run_lm_workflow(correct_train_data, correct_test_data)

correct_summary_columns <- c("term", "estimate", "std.error", "statistic", 
                             "p.value")

# test suite --------------------------------------------------------------

#Make the tests

test_that("`run_lm_workflow` should return a data frame", {
  expect_s3_class(run_lm_workflow(correct_train_data, correct_test_data), "data.frame")
})

test_that("`run_lm_workflow` should return an empty dataframe 
          if the inputs are empty`", {
            expect_equivalent(run_lm_workflow(empty_train_data, empty_test_data), empty_summary_output)
          })

test_that("`run_lm_workflow should return a data frame that contains the 
following columns: `term`, `estimate`, `std.error``, `statistic`, `p.value`", {
  expect_true(all(correct_summary_columns %in% colnames(example_correct_output)))
})

test_that("`run_lm_workflow` should throw an error 
         when incorrect types are passed to `train_data` and `test_data` 
          arguments", {
            expect_error(run_lm_workflow(incorrect_train_data, correct_test_data))
            expect_error(run_lm_workflow(correct_train_data, incorrect_test_data))
          })
