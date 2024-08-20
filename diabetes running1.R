# Load necessary libraries
library(caret)

# Load data
data <- read.csv('/Users/DataScienceClass/Desktop/R/diabetes.csv')

# Define columns names
colnames(data) <- c("Pregnancies", "Glucose", "BloodPressure", "SkinThickness", "Insulin", "BMI", "DiabetesPedigreeFunction", "Age", "Outcome")

# Convert 'Outcome' to factor
data$Outcome <- factor(data$Outcome, levels = c(0, 1), labels = c("No", "Yes"))

# Perform stratified sampling
set.seed(123)
train_index <- createDataPartition(data$Outcome, p = 0.75, list = FALSE)
train <- data[train_index, ]
test <- data[-train_index, ]

# Define the control parameters
ctrl <- trainControl(method = "cv", number = 10, classProbs = TRUE, summaryFunction = twoClassSummary)

# Define models
models <- list(
  C50 = list(
    model = "C5.0",
    train_function = train
  ),
  C50_PCA = list(
    model = "C5.0",
    preProcess = "pca",
    train_function = train
  ),
  NaiveBayes = list(
    model = "nb",
    train_function = train
  ),
  NeuralNetwork = list(
    model = "nnet",
    train_function = train
  ),
  SVM_SMO = list(
    model = "svmLinear",
    train_function = train
  ),
  RandomForest = list(
    model = "rf",
    train_function = train
  )
)

# Train models
results <- lapply(models, function(model) {
  train_function <- model$train_function
  if (length(train_function) == 1) {
    train_function <- as.character(train_function)
    if (exists(train_function)) {
      train_function <- get(train_function)
      if (exists("model$preProcess", model)) {
        train_function(Outcome ~ ., data = train, method = model$model, trControl = ctrl, preProcess = model$preProcess)
      } else {
        train_function(Outcome ~ ., data = train, method = model$model, trControl = ctrl)
      }
    } else {
      stop(paste("Train function", train_function, "not found"))
    }
  } else {
    stop("train_function should be a single function name")
  }
})

# Compare models
model_results <- resamples(results)

# Summarize results
summary(model_results)

# Plot results
bwplot(model_results, metric = "Accuracy")

# Check the best model
best_model <- getModelInfo()$C50$label

# Print the best model
print(paste("The best model is:", best_model))

# Now you can add further code for making predictions on the test set and evaluating the best model's performance on the test data.
