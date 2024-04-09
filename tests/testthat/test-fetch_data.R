library(testthat) # most widely used package in R for test cases




# Test data ---------------------------------------------------------------


# Function input for tests (making the input data)
correct_data_url <- "https://data.sac-isc.gc.ca/geomatics/directories/output/DonneesOuvertes_OpenData/CWB/CWB_2021.csv"
correct_tfHeader <- T
correct_sepType <- ","

incorrect_data_url <- 12345
incorrect_tfHeader <- "True"
incorrect_sepType <- 1


# Test suite --------------------------------------------------------------

# Make the tests
test_that("`run_lm_workflow` should return a data frame", {
  expect_s3_class(fetch_data(correct_data_url, correct_tfHeader, correct_sepType), 
                  "data.frame")
})

test_that("Function stops with an error if 'data_url' is not a string", {
  expect_error(fetch_data(incorrect_data_url, correct_tfHeader,correct_sepType),
               "Data URL must be a string")
})

test_that("Function stops with an error if 'tfHeader' is not a boolean", {
  expect_error(fetch_data(correct_data_url, incorrect_tfHeader,correct_sepType),
               "tfHeader must be a Boolean")
})

test_that("Function stops with an error if 'sepType' is not a string", {
  expect_error(fetch_data(correct_data_url, correct_tfHeader,incorrect_sepType),
               "sepType must be a string")
})