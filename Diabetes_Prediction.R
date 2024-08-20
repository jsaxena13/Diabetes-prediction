# Load required libraries
library(e1071) # for Naive Bayes classifier
library(randomForest) # for Random Forest classifier
library(ggplot2) # for plotting

# Read the diabetes dataset
data <- read.csv('/Users/DataScienceClass/Desktop/R/diabetes_prediction_dataset.csv')

# Check column names
colnames(data)

# Explore the structure of the dataset
str(data)

# Split the data into training and testing sets
set.seed(123) # for reproducibility
train_index <- sample(1:nrow(data), 0.8*nrow(data)) # 80% for training
train_data <- data[train_index, ]
test_data <- data[-train_index, ]

# Define features and target variable
features <- names(data)[-9] # excluding the target variable 'diabetes'
target <- "diabetes"

# Train Naive Bayes classifier
nb_model <- naiveBayes(as.factor(diabetes) ~ ., data = train_data)

# Train another model (Random Forest for example)
rf_model <- randomForest(as.factor(diabetes) ~ ., data = train_data)

# Predictions on test data
nb_pred <- predict(nb_model, newdata = test_data, type = "class")
rf_pred <- predict(rf_model, newdata = test_data)

# Confusion matrix for Naive Bayes
nb_conf_matrix <- table(Actual = test_data$diabetes, Predicted = nb_pred)

# Confusion matrix for Random Forest
rf_conf_matrix <- table(Actual = test_data$diabetes, Predicted = rf_pred)

# Accuracy calculation
nb_accuracy <- sum(diag(nb_conf_matrix)) / sum(nb_conf_matrix) * 100
rf_accuracy <- sum(diag(rf_conf_matrix)) / sum(rf_conf_matrix) * 100

# Combine predictions and actual diabetess for plotting
predictions <- data.frame(Actual = test_data$diabetes,
                          Naive_Bayes = nb_pred,
                          Random_Forest = rf_pred)

# Plotting
ggplot(predictions, aes(x = Actual, fill = Naive_Bayes)) +
  geom_bar(position = "dodge") +
  labs(title = "Comparison of Actual vs Predicted (Naive Bayes)",
       x = "diabetes",
       y = "Count") +
  theme_minimal()

ggplot(predictions, aes(x = Actual, fill = Random_Forest)) +
  geom_bar(position = "dodge") +
  labs(title = "Comparison of Actual vs Predicted (Random Forest)",
       x = "diabetes",
       y = "Count") +
  theme_minimal()

# Final dataset with predictions
final_dataset <- cbind(test_data, Naive_Bayes_Prediction = nb_pred, Random_Forest_Prediction = rf_pred)
