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
part <- "metacarpus"
set <- data |>
  dplyr::filter(Element == part) |>
  dplyr::select(where(~ !all(is.na(.x)))) |>
  dplyr::select(-Sex, -row_id, -Element)

# Need to convert taxon to factor otherwise train() function throws and error.
set$Taxon <- as.factor(set$Taxon)

# This defines the cross-validation strategy which the model uses.
# Instead of arbitrary 80/20 separation, this uses LOO
ctrl <- trainControl(
  method = "loocv",
  savePredictions = "final" # very important to save the predictions for the confusion matrix.
)

# Fit the model with train() function from caret.
model <- train(
  Taxon ~ .,
  data = set,
  method = "lda",
  trControl = ctrl,
  preProcess = c("scale", "center", "pca"),
  metric = "Accuracy"
)

print(model)
print(model$results)

# Extract confusion matrix.
conf <- confusionMatrix(model$pred$pred, model$pred$obs)
print(conf)

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
print(p)
