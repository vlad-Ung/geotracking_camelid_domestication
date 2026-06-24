library(openxlsx)
library(dplyr)
library(tidyr)
library(missMDA)
library(zoolog)

# Load IMPUTED training data.
training_file_name <- "database/training_data"
training_file_path <- paste0(training_file_name, "_imputed.csv")

# Read data.
training_data <- read.csv(training_file_path, sep = "\t")

# Initialise empty new dataframe.
new_data <- data.frame()

# Read new data tables and merge them into one dataframe
new_file_paths <- list.files("database/", "^arch.*\\.xlsx$", full.names = TRUE)

for (path in new_file_paths){
  temp <- read.xlsx(path)
  new_data <- dplyr::bind_rows(new_data, temp)
}
rm(temp)

# Change colnames into something zoolog would recognize before standardization.
colnames(new_data)[grep("Specimen", colnames(new_data))] <- "Specimen.ID"
colnames(new_data)[grep("element", colnames(new_data))] <- "Element"

# Standardize nomenclature.
new_data <- StandardizeDataSet(new_data)

selection <- c("Specimen.ID", "Element")

# Drop useless columns. Species and sex are only relevant for model fitting.
# When imputing data for predictions based on a fitted model only predictors are necessary.
new_data <- new_data |>
  dplyr::select(
    all_of(selection),
    starts_with("Meas")
  )

# Merge training and new data into one dataframe to make imputations
# based on already imputed training data

# Extract specimens ids to separate data after.
arch_ids <- new_data$Specimen.ID

combined_data <- dplyr::bind_rows(training_data, new_data)

# Get skeletal element names to iterate over.
bones <- unique(combined_data$Element)

# Store results in a list of lists for memmory efficiency
# I could do a growing dataframe but it's more intensive.
results <- list()

for (bone in bones) {
  bone_element <- combined_data |> 
    filter(Element == bone) |> 
    dplyr::select(
      all_of(selection),
      starts_with("Meas")
    )

  # Keep all the corresponding columns in training data.
  required_cols <- training_data |> 
    filter(Element == bone) |> 
    dplyr::select(starts_with("Meas")) |> 
    dplyr::select(where(~ !all(is.na(.x)))) |> 
    colnames()

  # Extract measurements.
  X <- bone_element |>
    dplyr::select(all_of(required_cols))

  # Have to skip if X has no values, bone element is useless.
  if (ncol(X) == 0 || ncol(X) == 1) {
    message("Skipped ", bone, ": no usable measurements.")
    next
  }

  # Impute missing values.
  if (any(is.na(X))) { # have to check if X actually has missing values.

    ncpca <- estim_ncpPCA(X)

    imputed <- imputePCA(
      X,
      ncp = ncpca$ncp
    )

    X <- as.data.frame(imputed$completeObs)
  }

  # Recombine metadata with measurements.
  bone_element <- bone_element |>
    dplyr::select(
      all_of(selection)
    ) |>
    bind_cols(X)

  # Store result in results list.
  results[[bone]] <- bone_element

  message("Done for ", bone, "!")
}

results <- results[!sapply(results, is.null)]

# Combine all results into one dataframe.
new_data <- dplyr::bind_rows(results)

new_data <- new_data |> 
  dplyr::select(all_of(selection), starts_with("Meas")) |> 
  filter(Specimen.ID %in% arch_ids)

# Export
readr::write_tsv(new_data, "database/arch_data_imputed.csv")
