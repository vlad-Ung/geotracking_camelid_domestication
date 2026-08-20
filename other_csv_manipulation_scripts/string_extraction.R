library(openxlsx)
library(dplyr)

data <- read.xlsx("database/1 Key.xlsx")

# Merge all meas columns into one column (called union) and add separator.
data <- data |> 
  select(starts_with("Meas")) |> 
  tidyr::unite(col = "union", sep = " / ")

# split concatenated rows at the added separator (it results in a list of chr vectors).
data <- stringr::str_split(data$union, " / ")

# concatenates all list items into one chr vector.
data <- purrr::list_c(data)

data <- data[data != "NA"]

# Source - https://stackoverflow.com/a/8613719
# Posted by Dason, modified by community. See post 'Timeline' for change history
# Retrieved 2026-08-20, License - CC BY-SA 3.0

# Get the parenthesis and what is inside
data <- stringr::str_extract_all(data, "\\([^()]+\\)")
# Remove parenthesis
data <- substring(data, 2, nchar(data) - 1)

data <- unique(data)

print(data)
