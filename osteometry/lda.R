library(caret)
library(dplyr)
library(MASS)
library(zoolog)
library(ggplot2)

# Set seed for reproductibility
set.seed(2026)
data <- read.csv("modern_meas_imputed.csv", sep = "\t")
data <- StandardizeDataSet(data)

# Select the skeletal element you want to compute the LDA for and
# make a subset for it.
parts <- unique(data$Element)

# Initialise empty results vector to store the results of individual LDAs.
models <- list()

# Initialise empty list to store confusion matrices.
matrices <- models

# Initialise empty matrix dataframe to extract accuracy metrics from confusion matrices.
matrix <- data.frame()

# Initialise empty graph vector to store the graphs.
graphs <- models

# This defines the cross-validation strategy which the model uses.
# Instead of arbitrary 80/20 separation, this uses LOO
ctrl <- trainControl(
  method = "loocv",
  savePredictions = "final" # very important to save the predictions for the confusion matrix.
)

for (part in parts){
  set <- data |>
    dplyr::filter(Element == part) |>
    dplyr::select(where(~ !all(is.na(.x)))) |>
    dplyr::select(-Sex, -row_id, -Element)

  # Need to convert taxon to factor otherwise train() function throws and error.
  set$Taxon <- as.factor(set$Taxon)

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
  matrices[[part]] <- confusionMatrix(model$pred$pred, model$pred$obs)
  graphs[[part]] <- p

  # Extract accuracy metrics from matrices list into a dataframe.
  matrix <- rbind(matrix, t(as.data.frame(matrices[[part]]$overall)))

  message("Done for ", part, "!")
}

matrix <- cbind(matrix, unique(data$Element))
colnames(matrix)[colnames(matrix) == "unique(data$Element)"] <- "part"
rownames(matrix) <- seq(1, length(unique(data$Element)))

matrix$part <- as.factor(matrix$part)

matrix_long <- matrix |>
  dplyr::select(part, Accuracy, Kappa) |> 
  tidyr::pivot_longer(-part, names_to = "metric", values_to = "value")

p <- ggplot(matrix_long,
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
  labs(title = "Accuracy metrics per skeletal element",
    colour = "Accuracy metric"
  ) +
  scale_colour_manual(values = c("orange", "#4d4d4d"))
plot(p)
