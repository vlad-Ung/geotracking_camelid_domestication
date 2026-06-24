library(caret)
library(dplyr)
library(MASS)
library(zoolog)
library(ggplot2)

# Set seed for reproductibility
set.seed(2026)
training_data <- read.csv("database/training_data_imputed.csv", sep = "\t")

# Select the skeletal element you want to compute the LDA for and
# make a subset for it.
parts <- unique(training_data$Element)

# Initialise empty results vector to store the results of individual LDAs.
models <- list()

# Initialise empty list to store confusion matrices.
matrices <- models

# Initialise empty matrix dataframe to extract accuracy metrics from confusion matrices.
matrix <- data.frame()

# Initialise empty graph vector to store the graphs.
graphs <- models

for (part in parts) {
  set <- training_data |>
    dplyr::filter(Element == part) |>
    dplyr::select(where(~ !all(is.na(.x))))

  # Check if the training data has at least 2 classes.
  # If <2 classes, Cohen's Kappa can't be calculated.
  check <- length(unique(set$Taxon))

  if (check < 2) {
    message(part, " has only one class present in the training data. Skipping...")
  }

  # Check if there are classes appear a single time in the set and skip if yes.
  # If a class appears only once, there is no group dispersion and it will
  # arcificially inflate accuracy.
  check <- set |> count(Taxon)

  if (any(check$n == 1)) {
    message(part, " has at least a taxonomic class appearing only once. Skipping...")
    next
  }

  # Need to convert taxon to factor otherwise train() function throws and error.
  set$Taxon <- as.factor(set$Taxon)

  # Group measurements by individulas for cross validation.
  # Leave one whole individual out for cross validation instead of one row out.
  # Because symmetrical elements from the same individual are basically clones.
  # And not accounting for symmetri inflates the accuracy.
  folds <- groupKFold(set$Specimen.ID, k = length(unique(set$Specimen.ID)))

  # This defines the cross-validation strategy which the model uses.
  # Instead of arbitrary 80/20 separation, this uses LOO
  ctrl <- trainControl(
    method = "loocv",
    savePredictions = "final", # very important to save the predictions for the confusion matrix.
    index = folds
  )

  # Drop unnecessary columns here (not above becuase we need Specimen.number for group folds).
  set <- set |>
    dplyr::select(-Sex, -Element, -Specimen.ID)

  # Fit the model with train() function from caret.
  model <- train(
    Taxon ~ .,
    data = set,
    method = "lda",
    trControl = ctrl,
    preProcess = c("scale", "center", "pca"),
    metric = "Accuracy"
  )

  # Caret doesn't give out the LDA scores so to plot them
  # this first applies the same preprocessing pipeline to the data and stores it in a separate object.
  preproc <- model$preProcess
  pred <- predict(preproc, set[, -which(names(set) == "Taxon")])

  # Extract the LDA scores by reapplying the same model conditions to the
  # data preprocessed by scaling, centering, and PCA.
  lda_scores <- predict(model$finalModel, pred)$x |>
    as.data.frame()

  # Append the groups from set.
  lda_scores$Taxon <- set$Taxon

  # Plot.
  p <- ggplot(lda_scores, aes(x = LD1, y = LD2, color = Taxon)) +
    geom_point(size = 2.5) +
    stat_ellipse(type = "norm", linetype = 2) +
    stat_ellipse(type = "t") +
    labs(title = "Training species in LDA space", color = "Taxon") +
    xlab("LD1") +
    ylab("LD2")

  # Store models, matrices, and graphs in their distinct lists.
  models[[part]] <- model

  matrices[[part]] <- confusionMatrix(factor(model$pred$pred), factor(model$pred$obs))

  # matrices[[part]] <- confusionMatrix(model$pred$pred, model$pred$obs)
  graphs[[part]] <- p

  # Extract accuracy metrics from matrices list into a dataframe.
  matrix <- rbind(matrix, t(as.data.frame(matrices[[part]]$overall)))

  message("Fitting done for ", part, "!")
}

# Extract overall accuracies from matrices to one dataframe.
matrix <- cbind(matrix, names(matrices))
colnames(matrix)[colnames(matrix) == "names(matrices)"] <- "part"
rownames(matrix) <- seq(1, length(names(matrices)))

# Need part variable as factor for reordering.
matrix$part <- as.factor(matrix$part)

# Need in long format for grouping in the plot.
matrix_long <- matrix |>
  dplyr::select(part, Accuracy, Kappa) |>
  tidyr::pivot_longer(-part, names_to = "metric", values_to = "value")

# Plot accuracy distribution per skeletal part.
p <- ggplot(
  matrix_long,
  aes(
    forcats::fct_reorder(part, value, .desc = TRUE),
    value,
    color = metric,
    group = metric
  )
) +
  geom_point(size = 2, shape = 21, stroke = 1.5) +
  geom_line() +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  xlab("") +
  ylab("") +
  labs(
    title = "Accuracy metrics per skeletal element",
    colour = "Accuracy metric"
  ) +
  scale_colour_manual(values = c("orange", "#4d4d4d"))
ggsave("accuracy_element_distribution.png", plot = p, width = 10, height = 8, units = "in")

# Keep only elements which can indeed distinguish between classes.
relevant_elements <- matrix |>
  filter(Kappa > 0.6) # Accuracy is always bigger than Kappa based on graph observations.

# Extract the skeletal parts that have models.
fitted_parts <- names(models)


# For predictions.
message("\nMoving to predictions...\n")
new_data <- read.csv("database/arch_data_imputed.csv", sep = "\t")

# Initialise empty results list.
pred_results <- list()

# What skeletal elements are in the prediction data?
new_data_parts <- unique(new_data$Element)

# What elements are in the prediction data and not in the training data?
difference <- new_data_parts[!(new_data_parts %in% fitted_parts)]
message(length(difference), " elements are missing from the available models.\n")

if (length(difference) != 0) {
  message("Excluding them from the predictions dataset...\n")

  # Predictions can only be made on parts wich had an LDA fitted.
  new_data <- new_data |>
    filter(Element %in% fitted_parts)

  # Also update new_data_parts
  new_data_parts <- unique(new_data$Element)
} else {
  message("No exclusion needed. Making predictions...\n")
}

# Check if all elements in the prediction data are in the trained set.
if (all(new_data$Element %in% fitted_parts)) {
  for (part in new_data_parts) {
    set <- new_data |>
      dplyr::filter(Element == part) |>
      dplyr::select(where(~ !all(is.na(.x))))

    prediction <- predict(models[[part]], newdata = set[, -c(1, 2)])

    set <- set |>
      mutate(Taxon = prediction)

    pred_results[[part]] <- set
    message("Predictions done for ", part, "!")
  }


  predictions <- dplyr::bind_rows(pred_results)

  # Collapse M/F separation from the predictions.
  # They're still accounted for in the model's accuracy metrics.
  predictions$Taxon <- gsub("_(M|F)$", "", predictions$Taxon)

  # Count how many times a taxon has been predicted for a specimen.
  taxon_counts <- predictions |>
    count(Specimen.ID, Taxon)

  taxon_counts$Specimen.ID <- as.factor(taxon_counts$Specimen.ID)

  g <- ggplot(
    taxon_counts,
    aes(
      forcats::fct_reorder(Specimen.ID, n, .desc = TRUE),
      n,
      fill = Taxon
    )
  ) +
    geom_col(position = "stack") +
    labs(
      x = "",
      y = "Frequency of predicted taxon",
      fill = "Taxon",
      title = "Taxon predictions per specimen across skeletal elements"
    ) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 1, size = 4))
  ggsave("number_of_predictions_per_specimen.png", plot = g, width = 13.5, height = 8, units = "in")

  final_pred <- taxon_counts |> 
    group_by(Specimen.ID) |> 
    filter(n == max(n))

  length(unique(final_pred$Specimen.ID))

  readr::write_tsv(final_pred, "database/predictions.csv")

} else {
  message("\nThere are elements in the predictions dataset for which no LDA has been trained.\n")
}

# ggsave("femur_training_lda.png", plot = graphs$femur, width = 10, height = 8, units = "in")
# ggsave("atlas_training_lda.png", plot = graphs$atlas, width = 10, height = 8, units = "in")
