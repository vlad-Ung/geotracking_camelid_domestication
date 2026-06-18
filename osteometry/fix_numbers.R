library(dplyr)
library(openxlsx)

data <- read.xlsx("database/1 Modern specimens Meas.xlsx")

data <- data |>
  mutate(
    across(
      starts_with("Meas"),
      ~ as.numeric(gsub(",", ".", .))
    )
  )

write.xlsx(data, "database/modern_meas_cleaned.xlsx")
