library(dplyr)
library(openxlsx)

# Change file paths according to which dataset the numbers must be fixed for.
file_name <- "1_archaeological_reference_meas"
input_file_path <- paste0("database/", file_name, "_cleaned.xlsx")
output_file_path <- paste0("database/", file_name, "_numbers_fixed.xlsx")

data <- read.xlsx(input_file_path)

# Basically just make sure every "Meas" column is numeric after
# replacing "," with "." in the numeric values.
data <- data |>
  mutate(
    across(
      starts_with("Meas"),
      ~ as.numeric(gsub(",", ".", .))
    )
  )

# And save them under the same file name.
write.xlsx(data, output_file_path)
