# XGBoost

# Importing the dataset
dataset = read.csv('Churn_Modelling.csv')
dataset = dataset[4:14]

# Encoding the categorical variables as factors
clean_dataset$sentiment_value = as.numeric(factor(clean_dataset$sentiment_value))
clean_dataset$content_rating = as.numeric(factor(clean_dataset$content_rating))

# Splitting the dataset into the Training set and Test set
# install.packages('caTools')
library(caTools)
set.seed(123)
split = sample.split(dataset$Exited, SplitRatio = 0.8)
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)

# Fitting XGBoost to the Training set
# install.packages('xgboost')
library(xgboost)
classifier = xgboost(data = as.matrix(training_set[-1]), label = training_set$profit, nrounds = 20)

# Predicting the Test set results
y_pred = predict(classifier, newdata = as.matrix(test_set[-1]))
y_pred = (y_pred >= 0.5)

# Making the Confusion Matrix
cm = table(test_set[, 1], y_pred)

# Applying k-Fold Cross Validation
# install.packages('caret')
library(caret)
folds = createFolds(training_set$profit, k = 10)
cv = lapply(folds, function(x) {
  training_fold = training_set[-x, ]
  test_fold = training_set[x, ]
  classifier = xgboost(data = as.matrix(training_set[-1]), label = training_set$profit, nrounds = 20)
  y_pred = predict(classifier, newdata = as.matrix(test_fold[-1]))
  y_pred = (y_pred >= 0.5)
  cm = table(test_fold[, 1], y_pred)
  accuracy = (cm[1,1] + cm[2,2]) / (cm[1,1] + cm[2,2] + cm[1,2] + cm[2,1])
  return(accuracy)
})
accuracy = mean(as.numeric(cv))
