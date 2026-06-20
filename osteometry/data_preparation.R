library(openxlsx)
library(dplyr)
library(tidyr)
library(missMDA)
library(zoolog)

file_name <- "database/arch_ref_meas"
file_path <- paste0(file_name, "_for_imputation.xlsx")

# Read data.
data <- read.xlsx(file_path)

colnames(data)[grep("Specimen", colnames(data))] <- "Specimen.ID"
colnames(data)[grep("element", colnames(data))] <- "Element"

# Standardize nomenclature.
data <- StandardizeDataSet(data)

# Determine if there is species or not in the dataset.
# The species and sex separation is only relevant for fitting the LDA anyway.
if ("Taxon" %in% colnames(data) && "Sex" %in% colnames(data)) {
  selection <- c("Specimen.ID", "Taxon", "Sex", "Element")
} else if ("Taxon" %in% colnames(data)) {
  selection <- c("Specimen.ID", "Taxon", "Element")
} else if ("Sex" %in% colnames(data)) {
  selection <- c("Specimen.ID", "Sex", "Element")
} else {
  selection <- c("Specimen.ID", "Element")
}

# Drop useless columns. Species and sex are only relevant for model fitting.
# When imputing data for predictions based on a fitted model only predictors are necessary.
data <- data |>
  dplyr::select(
    all_of(selection),
    starts_with("Meas")
  )

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
  )

# Need to filter out sex only for fitting.
# Need to know sex to separate sexual dimorphism.
if ("Sex" %in% colnames(data)) {
  data <- data |>
    filter(!is.na(Sex))
}

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
    dplyr::select(
      row_id,
      all_of(selection),
      Meas,
      Value
    ) |>
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
      row_id,
      all_of(selection)
    ) |>
    bind_cols(X)

  # Store result in results list.
  results[[i]] <- bone_element

  message("Done for ", bone, "!")
}

results <- results[!sapply(results, is.null)]

# Combine all results into one dataframe.
data_new <- bind_rows(results)

if ("Sex" %in% colnames(data_new)) {
  data_new$Sex[data_new$Sex == "."] <- NA

  data_new <- data_new |>
    filter(!is.na(Sex))
}

if ("Taxon" %in% colnames(data_new)) {
  data_new$Taxon[grep("F2", data_new$Taxon)] <- "F2_hybrid"
  data_new$Taxon[grep("F1", data_new$Taxon)] <- "F1_hybrid"
  data_new$Taxon[grep("Dromedary", data_new$Taxon, fixed = TRUE)] <- "C. dromedarius"
  data_new$Taxon[grep("Dromedary backcross", data_new$Taxon, fixed = TRUE)] <- "Dromedary_backcross"
  data_new$Taxon <- gsub(". ", "_", data_new$Taxon)

  if ("Sex" %in% colnames(data_new)){
    data_new <- data_new |>
      mutate(Taxon = paste0(Taxon, "_", Sex))
  }
}

# Export
readr::write_tsv(data_new, paste0(file_name, "_imputed.csv"))
