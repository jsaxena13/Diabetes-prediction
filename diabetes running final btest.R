## Loading the required libraries
library(ggplot2)
library(ggvis)
library(corrplot)
library(caTools)
library(ROCR)
library(e1071)  # Required for Naive Bayes

## Data Loading

data <- read.csv('/Users/DataScienceClass/Desktop/R/diabetes.csv')
head(data)
summary(data)
str(data)

## Data Cleaning
# Assuming you want to handle missing values by removing rows with missing values
data <- na.omit(data)

## Feature Scaling
# You can scale the numeric features if needed
numeric_cols <- sapply(data, is.numeric)
data[numeric_cols] <- lapply(data[numeric_cols], scale)

## Correlations

correlations <- cor(data)
correlations
corrplot(correlations, method="color")

## Visualization
pairs(data, col=data$Outcome)

### Glucose and Insulin

ggvis(data, ~Glucose, ~Insulin, fill =~Outcome) %>% 
  layer_points()

### BMI and DiabetesPedigreeFunction

ggvis(data, ~BMI, ~DiabetesPedigreeFunction, fill =~Outcome) %>% 
  layer_points()

### Age and Pregnancies

ggvis(data, ~Age, ~Pregnancies, fill =~Outcome) %>% 
  layer_points()

## Preparing the data

set.seed(88)
split <- sample.split(data$Outcome, SplitRatio = 0.75)
data_train <- subset(data, split == TRUE)
data_test <- subset(data, split == FALSE)

## Naive Bayes Classifier
nb_model <- naiveBayes(Outcome ~ ., data = data_train)

## Prediction
predict_train_nb <- predict(nb_model, data_train, type = "raw")
predict_test_nb <- predict(nb_model, data_test, type = "raw")

## Extracting Class Probabilities
prob_train <- predict(nb_model, data_train, type = "raw")[, 2]
prob_test <- predict(nb_model, data_test, type = "raw")[, 2]

## ROC Curve for Naive Bayes
ROCRpred_nb <- prediction(prob_train, data_train$Outcome)
ROCRperf_nb <- performance(ROCRpred_nb, 'tpr', 'fpr')

## Plot ROC Curve for Naive Bayes
plot(ROCRperf_nb, colorize = TRUE, text.adj = c(-0.2,1.7))


## Comparison between Observed and Predicted values for Naive Bayes
compare_nb <- data.frame(data_test$Outcome, predict_test_nb)
colnames(compare_nb) <- c("Observed Values","Predicted values")
ggplot(data = compare_nb, aes(x = "Observed Values", y = "Predicted values")) + geom_abline() +
  xlab("Observed Values") + ylab("Predicted values") + theme_classic()
compare_nb
