library(openxlsx)
library(dplyr)
library(tidyr)
library(missMDA)
library(zoolog)

file_name <- "database/training_data"
file_path <- paste0(file_name, "_for_imputation.xlsx")

# Read data.
data <- read.xlsx(file_path)

# Change relevant column names to something zoolog might better recognise
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
  filter(!is.na(Element))

# Need to filter out sex only for fitting.
# Need to know sex to separate sexual dimorphism.
if ("Sex" %in% colnames(data)) {
  data$Sex[grep("^\\W+$", data$Sex)] <- NA

  data <- data |>
    filter(!is.na(Sex))
}

# Get skeletal element names to iterate over.
bones <- unique(data$Element)

# Store results in a list of lists for memmory efficiency
# I could do a growing dataframe but it's more intensive.
results <- list()

for (bone in bones) {
  bone_element <- data |> 
    filter(Element == bone) |> 
    dplyr::select(
      all_of(selection),
      starts_with("Meas")
    )

  # Extract measurements.
  X <- bone_element |>
    dplyr::select(where(is.numeric))

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
      all_of(selection)
    ) |>
    bind_cols(X)

  # Store result in results list.
  results[[bone]] <- bone_element

  message("Done for ", bone, "!")
}

results <- results[!sapply(results, is.null)]

# Combine all results into one dataframe.
new_data <- bind_rows(results)

# This is only if the imputed data contains a "Taxon column" which
# for training data it should.
if ("Taxon" %in% colnames(new_data)) {
  new_data$Taxon[grep("^\\W+$", new_data$Taxon)] <- NA
  new_data <- new_data |>
    filter(!is.na(Taxon))
  new_data$Taxon[grep("hybrid", new_data$Taxon, ignore.case = TRUE)] <- "Hybrid"
  new_data$Taxon[grep("backcross", new_data$Taxon, ignore.case = TRUE)] <- "Hybrid"
  new_data$Taxon[grep("Dromedary", new_data$Taxon, ignore.case = TRUE)] <- "C. dromedarius"

  new_data$Taxon <- gsub("\\. ", "_", new_data$Taxon)

  # Again, an unnecesary conditioning because training data SHOULD have Sex determinations.
  if ("Sex" %in% colnames(new_data)){
    new_data <- new_data |>
      mutate(Taxon = paste0(Taxon, "_", Sex))
  }
}

# Export
readr::write_tsv(new_data, paste0(file_name, "_imputed.csv"))
