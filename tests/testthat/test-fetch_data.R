library(testthat)

# Assume correct_data_url points to a valid, accessible local or remote CSV file for testing

correct_data_url <- "https://data.sac-isc.gc.ca/geomatics/directories/output/DonneesOuvertes_OpenData/CWB/CWB_2021.csv"
correct_tfHeader <- TRUE
correct_sepType <- ","

# Incorrect inputs for testing
incorrect_data_url <- 12345  # Not a character
incorrect_tfHeader <- "True"  # Not a logical TRUE or FALSE
incorrect_sepType <- 1       # Not a character

test_that("`fetch_data` should return a data frame", {
  expect_s3_class(fetch_data(correct_data_url, correct_tfHeader, correct_sepType), "data.frame")
})

test_that("Function stops with an error if 'data_url' is not a string", {
  expect_error(fetch_data(incorrect_data_url, correct_tfHeader, correct_sepType))
})

test_that("Function stops with an error if 'tfHeader' is not a boolean", {
  expect_error(fetch_data(correct_data_url, incorrect_tfHeader, correct_sepType))
})

test_that("Function stops with an error if 'sepType' is not a string", {
  expect_error(fetch_data(correct_data_url, correct_tfHeader, incorrect_sepType))
})

