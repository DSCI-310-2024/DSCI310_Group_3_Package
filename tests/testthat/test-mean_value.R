library(testthat)
library(tibble)
library(readr)

# Set up test environment ---------------------------------------------------
test_dir <- tempdir() # Temporary directory for test outputs

# Prepare mock data for testing
mock_data <- tibble(
  Income_2021 = c(50000, 60000, 70000),
  Education_2021 = c(12, 14, 16),
  Housing_2021 = c(200, 250, 300),
  Labour_Force_Activity_2021 = c(1, 2, 1),
  CWB_2021 = c(70, 75, 80)
)

# Test data ---------------------------------------------------------------

# Test: calculate_and_export_means correctly calculates mean
test_that("calculate_and_export_means correctly calculates means", {
  temp_file <- tempfile(tmpdir = test_dir, fileext = ".csv")
  calculate_and_export_means(mock_data, test_dir, basename(temp_file))
  output_data <- read_csv(temp_file)
  expected_means <- tibble(
    Income_2021 = mean(c(50000, 60000, 70000)),
    Education_2021 = mean(c(12, 14, 16)),
    Housing_2021 = mean(c(200, 250, 300)),
    Labour_Force_Activity_2021 = mean(c(1, 2, 1)),
    CWB_2021 = mean(c(70, 75, 80))
  )
  # Since 'output_data' and 'expected_means' are data frames, we compare their contents directly
  expect_equal(as.data.frame(output_data), as.data.frame(expected_means), tolerance = 0.001)
})


# Test: calculate_and_export_means handles an empty dataframe correctly
test_that("calculate_and_export_means handles an empty dataframe correctly", {
  temp_file <- tempfile(tmpdir = test_dir, fileext = ".csv")
  calculate_and_export_means(tibble(), test_dir, basename(temp_file))
  
  expect_true(file.exists(temp_file), "Output file should be created for an empty dataframe.")
  
  output_data <- read_csv(temp_file)
  expect_equal(nrow(output_data), 0, info = "Output file for an empty dataframe should have no rows.")
})