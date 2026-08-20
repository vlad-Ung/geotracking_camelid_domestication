# Load data
species <- "bactrianus" # change as needed
file_path <- paste0("map_project/camel_data/", species, ".csv")
data <- read.csv(file_path)

# Pick which column becomes the filter.
filter_column  <- "date_old"
# Then make and sort the filter.
filter_vector <- unique(data[, filter_column])
filter_vector <- sort(filter_vector)

file_path <- gsub(".csv", "", file_path)

output <- data[0, ]

counter <- 1

# Now just go through each filter item and incrementally append
# selection to output and export it after each iteration.
for(item in filter_vector) {
  tmp <- data |>
    dplyr::filter(date_old == item)

  output <- dplyr::bind_rows(output, tmp)

  write.csv(output, paste0(file_path, "_", counter, ".csv"), sep = "\t")

  counter <- counter + 1
}
