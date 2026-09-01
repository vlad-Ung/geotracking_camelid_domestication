library(dplyr)

dir <- "map_project/statistics"

files <- list.files(dir, ".csv", full.names = TRUE)

for (file in files) {
  data <- read.csv(file)
  data <- data |>
    group_by(shapeName, evidence_t) |>
    summarise(
      n = n(),
      .groups = "drop"
    ) |>
    group_by(shapeName) |>
    mutate(
      min_evidence = min(n),
      max_evidence = max(n),
      total = sum(n)
    ) |>
    ungroup()

  file <- gsub(".csv$", "", file)
  file <- paste0(file, "_stats.csv")

  write.csv(data, file)
}
