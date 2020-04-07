# Fitting Logistic Regression to the Training set
classifier = glm(formula = profit ~ duration + director_facebook_likes + actor_1_facebook_likes + 
                   gross  + facenumber_in_poster + num_user_for_reviews + country + 
                   content_rating + budget + title_year + imdb_score + aspect_ratio + 
                   movie_facebook_likes + sentiment_value,
                 family = binomial(link = 'logit'),
                 data = training_set)

summary(classifier)


# Predicting the Test set results
prob_pred = predict(classifier, type = 'response', newdata = test_set[-20])
y_pred = ifelse(prob_pred > 0.5, 1, 0)

# Making the Confusion Matrix
cm = table(test_set[, 20], y_pred > 0.5)

# accuracy
accuracy <- (cm[[1,1]] + cm[[2,2]])/sum(cm)
accuracy

# Density of probabilities
ggplot(data.frame(prob_pred) , aes(prob_pred)) + 
  geom_density(fill = 'lightblue' , alpha = 0.4) +
  labs(x = 'Predicted Probabilities on test set')

k = 0
accuracy = c()
sensitivity = c()
specificity = c()
for(i in seq(from = 0.01 , to = 0.5 , by = 0.01)){
  k = k + 1
  preds_binomial = ifelse(prob_pred > i , 1 , 0)
  confmat = table(test_set$profit , preds_binomial)
  accuracy[k] = sum(diag(confmat)) / sum(confmat)
  sensitivity[k] = confmat[1 , 1] / sum(confmat[ , 1])
  specificity[k] = confmat[2 , 2] / sum(confmat[ , 2])
}

threshold = seq(from = 0.01 , to = 0.5 , by = 0.01)

data = data.frame(threshold , accuracy , sensitivity , specificity)
head(data)

# Gather accuracy , sensitivity and specificity in one column
ggplot(gather(data , key = 'Metric' , value = 'Value' , 2:4) , 
       aes(x = threshold , y = Value , color = Metric)) + 
  geom_line(size = 1.5)

