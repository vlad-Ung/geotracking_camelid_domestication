library(dplyr)
library(openxlsx)

data <- read.xlsx("database/arch_ref_meas_for_preparation.xlsx")

data <- data |>
  mutate(
    across(
      starts_with("Meas"),
      ~ as.numeric(gsub(",", ".", .))
    )
  )

write.xlsx(data, "database/arch_ref_meas_for_preparation.xlsx")
