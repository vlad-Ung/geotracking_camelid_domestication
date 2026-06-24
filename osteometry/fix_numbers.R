library(dplyr)
library(openxlsx)

file_name <- "arch_meas"
file_path <- paste0("database/", file_name, "_for_imputation.xlsx")

data <- read.xlsx(file_path)

data <- data |>
  mutate(
    across(
      starts_with("Meas"),
      ~ as.numeric(gsub(",", ".", .))
    )
  )

write.xlsx(data, file_path)
