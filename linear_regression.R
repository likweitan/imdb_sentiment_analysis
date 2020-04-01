# simple linear regression
linear_regression_dataset <- dataset %>%
  select(profit,sentiment_value) %>%
  mutate(sentiment = ifelse(sentiment_value == "positive", 1,
                            ifelse(sentiment_value == "negative", 0, NA)))
linear_regression_dataset$sentiment_value <- NULL
linear_regression_dataset <- linear_regression_dataset[!is.na(linear_regression_dataset$sentiment),]
# Splitting the dataset into the Training set and Test set

set.seed(123)
split = sample.split(linear_regression_dataset$gross, SplitRatio = 0.75)
linear_regression_training_set = subset(linear_regression_dataset, split == TRUE)
linear_regression_test_set = subset(dataset, split == FALSE)

# Fitting Simple Linear Regression to the Training set
regressor = lm(formula = profit ~ sentiment,
               data = linear_regression_training_set)
summary(regressor)
# Predicting the Test set results
y_pred = predict(regressor, newdata = linear_regression_test_set)

# Visualising the Training set results
ggplot() +
  geom_point(aes(x = linear_regression_training_set$sentiment, y = linear_regression_training_set$profit),
             colour = 'red') +
  geom_line(aes(x = linear_regression_training_set$sentiment, y = predict(regressor, newdata = linear_regression_training_set)),
            colour = 'blue') +
  ggtitle('Profit vs Budget (Training set)') +
  xlab('Budget') +
  ylab('Profit')

# Visualising the Test set results
ggplot() +
  geom_point(aes(x = linear_regression_test_set$budget, y = linear_regression_test_set$gross),
             colour = 'red') +
  geom_line(aes(x = linear_regression_training_set$budget, y = predict(regressor, newdata = linear_regression_training_set)),
            colour = 'blue') +
  ggtitle('Profit vs Budget (Test set)') +
  xlab('Budget') +
  ylab('Profit')
