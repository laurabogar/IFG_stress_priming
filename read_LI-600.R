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

# Read an LI-600 / LI-600N export with correct column types
# 18 Nov 2026 LB
#
# The export has a group row, a header row, then a units row. Reading with
# skip = 1 and dropping the units row afterwards (as read_li600_data() does)
# leaves any column with a unit label (gsw, leak_pct, Tleaf, ...) stored as
# text. This version skips the units row before guessing types, and keeps the
# ID columns as text so "001" stays "001".
read_li600_clean <- function(file_path) {
  col_names <- names(read_csv(file_path, skip = 1, n_max = 0,
                              show_col_types = FALSE,
                              name_repair = "unique_quiet"))
  read_csv(file_path, skip = 3, col_names = col_names,
           col_types = cols(`Plant number` = col_character(),
                            `Leaf number`  = col_character(),
                            remark         = col_character(),
                            Date           = col_character(),
                            Time           = col_character(),
                            .default       = col_guess()),
           show_col_types = FALSE, name_repair = "unique_quiet")
}
