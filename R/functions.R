utils::globalVariables(c("Index", "count", "variable", "Income_2021",
                         "CWB_2021", ".pred"))

#' Generate a Line Plot
#'
#' This function takes a dataframe and a list of column names as input and
#' generates a line plot. The plot displays the counts of unique values in
#' the specified columns. Each line in the plot represents one of the
#' specified columns, differentiated by color.
#'
#' @param data A dataframe with rows of data and columns of variables. The
#' The dataframe should contain the columns specified in `cols`.
#' @param cols A vector of column names in `data` to be plotted.
#' These columns should contain categorical or discrete numerical values
#' for which counts will be plotted.
#'
#' @return A `ggplot` object representing a line plot of counts of unique
#' values for the specified columns in `data`. Each line is colored
#' differently to distinguish between the variables represented by the
#' columns in `cols`.
#' @export
#'
#' @examples
#' data <- data.frame(
#'   Names = c("Alice", "Bob", "Charlie"),
#'   Ages = c(25, 30, 22),
#'   Height = c(21, 33, 21))
#'
#' cols <- c("Ages", "Height")
#' lineplot(data, cols)
lineplot <- function(data, cols) {
  
  # Ensure 'data' is a dataframe
  if (!is.data.frame(data)) {
    stop("Data must be a dataframe")
  }
  
  combined_data <- lapply(cols, function(col) {
    # Ensure 'col' is a valid column in 'data'
    if (!col %in% names(data)) {
      stop(paste("Column", col, "not found in data"))
    }
    
    # Process each specified column
    temp_data <- data |>
      dplyr::group_by(!!rlang::sym(col)) |>
      dplyr::summarize(count = dplyr::n(), .groups = "drop") |>
      dplyr::mutate(variable = col) |>
      dplyr::rename(Index = !!rlang::sym(col))
    
    return(temp_data)
  }) |>
    dplyr::bind_rows()
  
  # Create the plot
  plot <- ggplot2::ggplot(combined_data,
                          ggplot2::aes(x = Index,
                                       y = count,
                                       color = variable)) +
    ggplot2::geom_line() +
    ggplot2::xlab("Index") +
    ggplot2::ylab("Counts") +
    ggplot2::ggtitle("Index of Different Variables") +
    ggplot2::scale_color_manual(values = c("Income_2021" = "blue",
                                           "Education_2021" = "red",
                                           "Labour_Force_Activity_2021" = "yellow",
                                           "Housing_2021" = "green",
                                           "CWB_2021" = "black")) +
    ggplot2::theme_minimal()
  
  return(plot)
}





#' Reads data from a given URl string literal and outputs that data
#'
#' This function takes a URl to a csv file online, whether the csv file has an header
#' and the separation type as an input  
#' and returns that data.
#' 
#' @param data_url A URl String to the .csv file online
#' @param tfHeader Boolean TRUE or FALSE for if the csv file has a header row
#' @param sepType String for how the values in the csv file has been separated
#'
#' @return A data frame or data frame extension (e.g. a tibble).
#'
#' @export
#' 
#' @examples
#' data_url <- paste0("https://data.sac-isc.gc.ca/",
#'             "geomatics/directories/output/",
#'             "DonneesOuvertes_OpenData/CWB/",
#'             "CWB_2021.csv")
#' fetch_data(data_url, TRUE, ",")
#' # Specify the URL for the data
fetch_data <- function(data_url, tfHeader, sepType) {
  if (!is.character(data_url)) {
    stop("Data URL must be a string", call. = FALSE)
  }
  if (!is.logical(tfHeader) || length(tfHeader) != 1) {
    stop("tfHeader must be a Boolean", call. = FALSE)
  }
  if (!is.character(sepType)) {
    stop("SepType must be a string", call. = FALSE)
  }
  
  raw_data <- tryCatch({
    utils::read.csv(data_url, header = tfHeader, sep = sepType)
  }, error = function(e) {
    stop("Failed to load data. Please check the URL and format.", call. = FALSE)
  })
  
  return(raw_data)
}







#' Calculate Mean Values of Specified Columns and Export to CSV
#'
#' Calculate mean values for specified columns in a dataframe and exports
#' these mean values to a CSV file. It is specifically designed to process
#' columns related to socioeconomic indicators, from 'Income_2021' to 'CWB_2021'
#' The function is designed to assist in the preprocessing steps of a data 
#' analysis workflow.
#'
#' @param data A dataframe or a tibble containing the relevant columns.
#' @param out_dir A string specifying the directory path where the output CSV 
#' file will be saved.
#' @param file_name A string specifying the name of the output CSV file.
#'        Default is "table_mean.csv".
#'
#' @details The function operates on a range of columns, calculating the mean 
#'          for each column and exporting the results to a CSV file named 
#'          "table_mean.csv" by default. It's part of a larger analysis workflow
#'          process that aims to learn about the health 
#'          and happiness of communities.
#'
#' @return Writes a CSV file to the specified directory containing the mean 
#'         values for the specified range of columns. The function itself 
#'         invisibly returns NULL.
#'
#' @examples
#' df <- tibble::tibble(
#'   Income_2021 = c(50000, 60000, 70000),
#'   Education_2021 = c(12, 14, 16),
#'   Housing_2021 = c(200, 250, 300),
#'   Labour_Force_Activity_2021 = c(1, 2, 1),
#'   CWB_2021 = c(70, 75, 80)
#' )
#' 
#' out_dir <- "results"
#' if (!dir.exists(out_dir)) {
#' dir.create(out_dir)}
#' calculate_and_export_means(df, out_dir, "table_mean.csv")
#' if (dir.exists(out_dir)) {
#' unlink(out_dir, recursive = TRUE)
#' }
#' 
#' @export
calculate_and_export_means <- function(data, out_dir, file_name = "table_mean.csv") {
  # Load necessary libraries
  if (!requireNamespace("dplyr", quietly = TRUE)) stop("dplyr is required but not installed.")
  if (!requireNamespace("readr", quietly = TRUE)) stop("readr is required but not installed.")
  
  if (ncol(data) == 0 || nrow(data) == 0) {
    empty_table <- tibble::tibble(
      Income_2021 = numeric(),
      Education_2021 = numeric(),
      Housing_2021 = numeric(),
      Labour_Force_Activity_2021 = numeric(),
      CWB_2021 = numeric()
    )
    output_path <- file.path(out_dir, file_name)
    readr::write_csv(empty_table, output_path)
    message("Empty dataframe or dataframe with no columns. Output CSV file created with column headers only.")
    return(invisible(NULL))
  }
  
  # Calculate the mean values for the specified range of columns
  table_mean <- data |>
    dplyr::summarize(dplyr::across(Income_2021:CWB_2021,
                                   ~mean(.x, na.rm = TRUE)))
  
  # Define the output file path
  output_path <- file.path(out_dir, file_name)
  
  # Write to CSV
  readr::write_csv(table_mean, output_path)
  
  message("Mean values have been successfully exported to: ", output_path)
  
  # The function primarily has a side effect (writing a file) and returns NULL
  invisible(NULL)
}









#' Run a Linear Regression Model & Output the Model Summary
#'
#' Creates a linear regression model workflow that predicts the  
#' community well-being scores for various Canadian communities,
#' and outputs a table of the model's summary.
#'
#' @param train_data A data frame or data frame extension (e.g. a tibble)
#' @param test_data A data frame or data frame extension (e.g. a tibble)
#'
#' @return A data frame or data frame extension (e.g. a tibble). 
#'   The table contains the summary outputs of the model.
#'
#' @export
#' 
#' @examples
#' run_lm_workflow(train_data, test_data)
#' Run a Linear Regression Model & Output the Model Summary
#'
#' Creates a linear regression model workflow that predicts the  
#' community well-being scores for various Canadian communities,
#' and outputs a table of the model's summary.
#'
#' @param train_data A data frame or data frame extension (e.g. a tibble)
#' @param test_data A data frame or data frame extension (e.g. a tibble)
#'
#' @return A data frame or data frame extension (e.g. a tibble). 
#'   The table contains the summary outputs of the model.
#'
#' @export
#' 
#' @examples
#' run_lm_workflow(train_data, test_data)
#' Run a Linear Regression Model & Output the Model Summary
#'
#' Creates a linear regression model workflow that predicts the  
#' community well-being scores for various Canadian communities,
#' and outputs a table of the model's summary.
#'
#' @param train_data A data frame or data frame extension (e.g. a tibble)
#' @param test_data A data frame or data frame extension (e.g. a tibble)
#'
#' @return two data frame or data frame extension (e.g. a tibble). 
#'   The table contains the summary outputs of the model, as well as the
#'   coefficients of the model.
#'
#' @export
#' 
#' @examples
#' train_data <- tibble::tibble(
#'   CWB_2021 = rnorm(100), 
#'   Income_2021 = rnorm(100),
#'   Education_2021 = rnorm(100),
#'   Housing_2021 = rnorm(100),
#'   Labour_Force_Activity_2021 = rnorm(100)
#'  )

#' test_data <- tibble::tibble(
#'   CWB_2021 = rnorm(50),  
#'   Income_2021 = rnorm(50),
#'   Education_2021 = rnorm(50),
#'   Housing_2021 = rnorm(50),
#'   Labour_Force_Activity_2021 = rnorm(50)
#'  )
#'
#'  results <- run_lm_workflow(train_data, test_data)
run_lm_workflow <- function(train_data, test_data) {
  
  # Define linear regression specification
  lm_spec <- parsnip::linear_reg() |>
    parsnip::set_engine("lm") |>
    workflows::set_mode("regression")
  
  # Define recipe
  lm_recipe <- recipes::recipe(CWB_2021 ~ Income_2021 + Education_2021 +
                                 Housing_2021 + Labour_Force_Activity_2021,
                               data = train_data)
  
  # Create workflow
  lm_fit <- workflows::workflow() |>
    workflows::add_recipe(lm_recipe) |>
    workflows::add_model(lm_spec) |>
    parsnip::fit(data = train_data)
  
  # Generate predictions and compute metrics for test data
  test_summary <- lm_fit |>
    stats::predict(new_data = test_data) |>
    dplyr::bind_cols(test_data) |>
    yardstick::metrics(truth = CWB_2021, estimate = .pred)
  
  # Return the test summary
  return(list(test_summary = test_summary, lm_fit = lm_fit))
}



