library(openxlsx)
library(dplyr)
library(tidyr)
library(missMDA)
library(zoolog)

# Read data.
data <- read.xlsx("database/training_data_for_imputation.xlsx")

# Drop useless columns.
data <- data |> 
  dplyr::select(Species, Sex, Element, starts_with("Meas"))

# Standardize nomenclature.
data <- StandardizeDataSet(data)

# Add unique row id because pivot_wider will complain that values
# are are not separated by unique identifiers.
data <- data |>
  filter(!is.na(Element)) |>
  mutate(row_id = row_number())

# Convert measurements to long format so all invormation is in one
# column to call it from.
data <- data |>
  pivot_longer(
    cols = starts_with("Meas"),
    names_to = "Meas",
    values_to = "Value"
  ) |>
  filter(!is.na(Sex)) # need to know sex to separate sexual dimorphism

# Get skeletal element names to iterate over.
bones <- unique(data$Element)

# Store results in a list of lists for memmory efficiency
# I could do a growing dataframe but it's more intensive.
results <- vector("list", length(bones))

for (i in seq_along(bones)) {
  bone <- bones[i] # onto the next bone element.

  # Make a subset of data for the current bone element.
  bone_element <- data |>
    filter(Element == bone) |>
    dplyr::select(row_id, Taxon, Sex, Element, Meas, Value) |>
    pivot_wider(
      names_from = Meas,
      values_from = Value
    )

  # Extract measurements.
  X <- bone_element |>
    dplyr::select(where(is.numeric)) |>
    dplyr::select(-row_id)

  # Drop measurements with more than 50% missing values.
  X <- X |>
    dplyr::select(where(~ mean(is.na(.x)) < 0.5))

  # Remove measurements with no variance.
  X <- X |>
    dplyr::select(where(~ {
      v <- var(.x, na.rm = TRUE)
      !is.na(v) && v > 0
    }))

  # Have to skip if X has no values, bone element is useless.
  if (ncol(X) == 0) {
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
    dplyr::select(row_id, Taxon, Sex, Element) |>
    bind_cols(X)

  # Store result in results list.
  results[[i]] <- bone_element

  message("Done for ", bone, "!")
}

results <- results[!sapply(results, is.null)]

# Combine all results into one dataframe.
data_new <- bind_rows(results)

data_new$Taxon[grep("F2", data_new$Taxon)] <- "F2_hybrid"

data_new$Sex[data_new$Sex == "."] <- NA

data_new <- data_new  |>
  filter(!is.na(Sex))

data_new$Taxon <- gsub(". ", "_", data_new$Taxon)

data_new <- data_new |> 
  mutate(Taxon = paste0(Taxon, "_", Sex))

unique(data_new$Taxon)

# Export
readr::write_tsv(data_new, "modern_meas_imputed.csv")
