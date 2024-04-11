# G3package
The G3package is a specialized R package designed to streamline
data analysis workflows for DSCI 310 Group 3's project. It features key 
functions like `lineplot` for visualizing data counts, `fetch_data` for 
importing CSV files, `calculate_and_export_means` for statistical analysis,
and `run_lm_workflow` for predictive modeling using linear regression. Authored 
by Shawn Li, Selena Shew, Sri Chaitanya, and Lesley Mai, this package is 
versioned at 0.0.0.9000 and licensed under MIT. It relies on important R 
libraries like `dplyr`, `ggplot2`, and `tidymodels` for its operations. The 
package facilitates easy bug reporting and contributions through its GitHub
page.

## Installation

You can install the development version of our visualization package from
[GitHub](https://github.com/) with:
      
``` {r}
devtools::install_github("DSCI-310-2024/DSCI310_Group_3_Package")
```

## Usage

Here are examples of how our functions can be used:


``` {r}
# Example 1: lineplot function
# Assume we have a dataframe 'df' with columns
# 'Category1', 'Category2', and 'Count'

(df <- data.frame(
  Category1 = c("A", "A", "B", "B"),
  Category2 = c("X", "Y", "X", "Y"),
  Count = c(10, 20, 15, 25)
))

# To plot 'Category1' and 'Category2' using our lineplot function
cols_to_plot <- c("Category1", "Category2")
lineplot(df, cols_to_plot)

# Example 2: run_lm_workflow
train_data <- tibble(
  CWB_2021 = rnorm(100),
  Income_2021 = rnorm(100),
  Education_2021 = rnorm(100),
  Housing_2021 = rnorm(100),
  Labour_Force_Activity_2021 = rnorm(100)
)

test_data <- tibble(
  CWB_2021 = rnorm(50),
  Income_2021 = rnorm(50),
  Education_2021 = rnorm(50),
  Housing_2021 = rnorm(50),
  Labour_Force_Activity_2021 = rnorm(50)
)

results <- run_lm_workflow(train_data, test_data)



# Example 3: fetch_data
  data_url <- "https://data.sac-isc.gc.ca/geomatics/directories/output/DonneesOuvertes_OpenData/CWB/CWB_2021.csv"
  fetch_data(data_url, TRUE, ",")

# Example 4: calculate_and_export_means
df <- tibble::tibble(
                     Income_2021 = c(50000, 60000, 70000),
                     Education_2021 = c(12, 14, 16),
                     Housing_2021 = c(200, 250, 300),
                     Labour_Force_Activity_2021 = c(1, 2, 1),
                     CWB_2021 = c(70, 75, 80))
calculate_and_export_means(df, getwd(), "table_mean.csv")
```

If you want to load the package to use all functions listed above, you can
quickly type:

``` {r}
library(G3package)
```

