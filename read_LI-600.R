# Function to read LI-600 data
# 23 Oct 2024 LB

library(tidyverse)

# Function to read LI-600 data
read_li600_data <- function(file_path) {
  # Read the CSV file, assuming comma-delimited
  raw_data <- read_csv(file_path, skip = 1)
  tidied = raw_data[-1,]
  return(tidied)
}

# Example usage
# li600_data <- read_li600_data("path/to/your/li600_data.csv")
# head(li600_data)