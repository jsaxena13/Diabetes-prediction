## Loading the required libraries
library(ggplot2)
library(ggvis)
library(corrplot)
library(caTools)
library(ROCR)

## Data Loading

data <- read.csv('/Users/DataScienceClass/Desktop/R/diabetes.csv')
head(data)
summary(data)
str(data)

## Correlations

correlations <- cor(data)
correlations
corrplot(correlations, method="color")

## Visualization
pairs(data, col=data$Outcome)

### Glucose and Insulin

ggvis(data, ~Glucose, ~Insulin, fill =~Outcome) %>% 
  layer_points()

### BMI ad DiabetesPedigreeFunction

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

## Logistic regression

model <- glm (Outcome ~ .-Pregnancies + Glucose + BloodPressure + SkinThickness + Insulin + BMI + DiabetesPedigreeFunction + Age, data = data_train, family = binomial)
summary(model)

## Prediction

predict_train <- predict(model, type = 'response')
predict_test <- predict(model, newdata = data_test, type = 'response')

## ROC Curve

ROCRpred <- prediction(predict_train, data_train$Outcome)
ROCRperf <- performance(ROCRpred, 'tpr','fpr')
plot(ROCRperf, colorize = TRUE, text.adj = c(-0.2,1.7))

## Comparison

predict_test_c = predict_test
i = 1
while(i <= length(predict_test))
{
  if(predict_test[i] < 0.5)
    predict_test_c[i] = 0
  else
    predict_test_c[i] = 1
  i = i + 1;
}
compare <- data.frame(data_test$Outcome,predict_test_c)
colnames(compare) <- c("Observed Values","Predicted values")
ggplot(data = compare,aes(x = "Observed Values", y = "Predicted values")) + geom_abline() +
  xlab("Observed Values") + ylab("Predicted values") + theme_classic()
compare
