library(tibble)
library(caTools)
library(dplyr)
library(tidytext)
library(tidyr)
library(ggplot2)
library(wordcloud)
library(reshape2)

# DATA PREPROCESSING

# read csv
dataset <- read.csv("documents/R Programming/data/movie_metadata.csv", stringsAsFactors = FALSE)
str(dataset)
# quick look
glimpse(dataset)
summary(dataset)

# Dicard unnecessary columns
dataset$movie_imdb_link <- NULL

xtabs(~ color, data = dataset)
dataset$color <- NULL
dataset$director_name <- NULL
dataset$director_facebook_likes <- NULL
dataset$actor_1_name <- NULL
dataset$actor_2_name <- NULL
dataset$actor_3_name <- NULL
dataset$actor_1_facebook_likes <- NULL
dataset$actor_2_facebook_likes <- NULL
dataset$actor_3_facebook_likes <- NULL
dataset$facenumber_in_poster <- NULL

# Remove Duplicates
# calculate number of duplicate rows
sum(duplicated(dataset))
# delete duplicate rows
dataset <- distinct(dataset)

# Missing Values
# calculate total missing values
colSums(sapply(dataset, is.na))

# fb likes
#dataset <- dataset[dataset$movie_facebook_likes > 0,]
#summary(dataset$movie_facebook_likes)
#bench <- 16250 + 1.5 * IQR(dataset$movie_facebook_likes)
#bench
# the outliers from dataset
#dataset$movie_facebook_likes[dataset$movie_facebook_likes > bench]
# remove the outliers
#dataset <- dataset[dataset$movie_facebook_likes < bench,]
#boxplot(dataset$movie_facebook_likes)

#
dataset <- dataset[!is.na(dataset$gross),]
dataset <- dataset[!is.na(dataset$budget),]

#
ggplot(dataset,aes(title_year)) +
  geom_bar()
dataset <- dataset[dataset$title_year > 1980,]


# Outlier
#summary(dataset$gross)
#bench <- 63475749 + 1.5 * IQR(dataset$gross)
#bench
# winsoring
#dataset$gross[dataset$gross > bench] <- bench
#boxplot(dataset$gross)

#aggregate(dataset$gross,list(dataset$title_year),mean)

# DATA TRANSFORMATION
table(dataset$content_rating)

dataset$id <- seq.int(nrow(dataset))

dataset <- dataset[!(dataset$content_rating %in% ""),]

dataset$content_rating[dataset$content_rating == "M"] <- 'PG'
dataset$content_rating[dataset$content_rating == "GP"] <- 'PG'
dataset$content_rating[dataset$content_rating == "X"] <- 'NC-17'

dataset$content_rating[dataset$content_rating == "Approved"] <- 'R'
dataset$content_rating[dataset$content_rating == "Passed"] <- 'R'
dataset$content_rating[dataset$content_rating == "Unrated"] <- 'R'
dataset$content_rating[dataset$content_rating == "Not Rated"] <- 'R'

dataset$content_rating <- factor(dataset$content_rating)

table(dataset$content_rating)

dataset <- dataset %>%
  mutate(profit = gross - budget,
         percent_return_on_invest = (profit/budget)*100)

#bin
dataset$bin_imdb_score <- cut(dataset$imdb_score, breaks = c(0,4,6,8,10))

# plot_keyword_df
dataset$genres <- gsub("\\|", " ", dataset$genres)
corpus = VCorpus(VectorSource(dataset$genres))
corpus = tm_map(corpus, content_transformer(tolower))
as.character(corpus[[1]])
corpus = tm_map(corpus, removeNumbers)
corpus = tm_map(corpus, removePunctuation)
corpus = tm_map(corpus, removeWords, stopwords("en"))
corpus = tm_map(corpus, stripWhitespace)
# create the bag of words model
dtm <- DocumentTermMatrix(corpus)
dtm <- removeSparseTerms(dtm,0.999)
plot_keyword <- as.data.frame(as.matrix(dtm))


# Data Partition

# Splitting the dataset into the Training set and Test set
set.seed(123)
split = sample.split(dataset$profit, SplitRatio = 2/3)
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)

# Feature Scaling
# training_set = scale(training_set)
# test_set = scale(test_set)


# DESCRIPTIVE
tidy_plot_keyword <- tibble(line = 1:3697,text = as.character(dataset$plot_keywords))
tidy_plot_keyword <- unnest_tokens(tidy_plot_keyword,word,text)

# Comparing the three sentiment dictionaries
afinn <- tidy_plot_keyword %>%
  group_by(index = line) %>%
  inner_join(get_sentiments("afinn")) %>% 
  summarise(sentiment = sum(value)) %>% 
  mutate(method = "AFINN")

bing_and_nrc <- bind_rows(tidy_plot_keyword %>%
                            inner_join(get_sentiments("bing")) %>%
                            mutate(method = "Bing"),
                          tidy_plot_keyword %>%
                            inner_join(get_sentiments("nrc") %>% 
                                         filter(sentiment %in% c("positive", 
                                                                 "negative"))) %>%
                            mutate(method = "NRC")) %>%
  count(method, index = line, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative)

bind_rows(afinn, 
          bing_and_nrc) %>%
  ggplot(aes(index, sentiment, fill = method)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~method, ncol = 1, scales = "free_y")

# count how many positive and negative in nrc
  tidy_plot_keyword %>%
  inner_join(get_sentiments("nrc")) %>%
  filter(sentiment %in% c("positive", 
                          "negative")) %>% 
  count(sentiment)

# count how many positive and negative in bing
tidy_plot_keyword %>%
  inner_join(get_sentiments("bing")) %>%
  filter(sentiment %in% c("positive", 
                          "negative")) %>% 
  count(sentiment)

# tibble line-sentiment value
line_sentiment_category <- tidy_plot_keyword %>%
  inner_join(get_sentiments("nrc")) %>%
  filter(sentiment %in% c("positive", 
                          "negative")) %>%
  mutate(sentiment_value = ifelse(sentiment %in% c('positive'), 1,
                           -1)) %>%
  group_by(line) %>%
  summarise(sentiment_value = mean(sentiment_value)) %>%
  mutate(sentiment_value = ifelse(sentiment_value > 0, "positive",
                                  ifelse(sentiment_value < 0, "negative", "neutral"))) %>%
  mutate(id = line) %>%
  data.frame()

dataset <- full_join(dataset,line_sentiment_category, by = 'id')
dataset <- dataset[!is.na(dataset$profit),]

sentiment_profit <- dataset %>%
  select(profit,sentiment_value) %>%
  group_by(sentiment_value)

sent <- sentiment_profit %>%
  #filter(sentiment_value %in% "positive") %>%
  #mutate(profit = ifelse(profit > 0, 1,
                         #ifelse(profit < 0, -1, 0))) %>%
  group_by(sentiment_value) %>%
  summarise(profit_count = sum(profit > 0),
            loss_count = sum(profit < 0))

# profit count pie chart
pie(sent$profit_count, main = "Total profit count",
                       labels = c("negative", "neutral", "positive"),
                       col = c("red", "blue", "green"))
# loss count pie chart
pie(sent$loss_count, main = "Total loss count",
    labels = c("negative", "neutral", "positive"),
    col = c("red", "blue", "green"))



sentiment_category <- tidy_plot_keyword %>%
  inner_join(get_sentiments("nrc")) %>%
  filter(sentiment %in% c("positive", 
                          "negative")) %>%
  mutate(sentiment_value = ifelse(sentiment %in% c('positive'), 1,
                                  -1)) %>%
  group_by(line) %>%
  summarise(sentiment_value = as.double(format(round(mean(sentiment_value), 5), nsmall = 5))) %>%
  count(sentiment_value, sort = TRUE) %>%
  mutate(sentiment_category = ifelse(sentiment_value > 0, "positive",
                                    ifelse(sentiment_value < 0, "negative",
                                    ifelse(sentiment_value == 0, "neutral", "")))) %>%
  group_by(sentiment_category) %>%
  summarise(n = sum(n)) %>%
  ungroup() %>%
  select(sentiment_category) %>%
  as.vector() %>%
  unlist()

sentiment_color <- tidy_plot_keyword %>%
  inner_join(get_sentiments("nrc")) %>%
  filter(sentiment %in% c("positive", 
                          "negative")) %>%
  mutate(sentiment_value = ifelse(sentiment %in% c('positive'), 1,
                                  -1)) %>%
  group_by(line) %>%
  summarise(sentiment_value = as.double(format(round(mean(sentiment_value), 2), nsmall = 2))) %>%
  count(sentiment_value, sort = TRUE) %>%
  mutate(average_sentiment = ifelse(sentiment_value < 0.000, "positive",
                                    ifelse(sentiment_value > 0.000, "negative",
                                           "neutral"))) %>%
  group_by(average_sentiment) %>%
  count(average_sentiment, sort = TRUE) %>%
  ungroup() %>%
  select(average_sentiment) %>%
  mutate(sentiment_color = ifelse(average_sentiment == "positive", "green",
                                  ifelse(average_sentiment == "negative", "red",
                                  "white"))) %>%
  select(sentiment_color) %>%
  as.vector() %>%
  unlist()

sentiment_value <- tidy_plot_keyword %>%
  inner_join(get_sentiments("nrc")) %>%
  filter(sentiment %in% c("positive", 
                          "negative")) %>%
  mutate(sentiment_value = ifelse(sentiment %in% c('positive'), 1,
                                  -1)) %>%
  group_by(line) %>%
  summarise(sentiment_value = as.double(format(round(mean(sentiment_value), 5), nsmall = 5))) %>%
  count(sentiment_value, sort = TRUE) %>%
  mutate(average_sentiment = ifelse(sentiment_value > 0, "positive",
                                    ifelse(sentiment_value < 0, "negative",
                                           ifelse(sentiment_value == 0, "neutral", "")))) %>%
  group_by(average_sentiment) %>%
  summarise(n = sum(n)) %>%
  ungroup() %>%
  select(n) %>%
  as.vector() %>%
  unlist()

# create the bar chart
barplot(sentiment_value, main = "",
                         xlab = "average sentiment score",
                         ylab = "number of movies",
                         names.arg = average_sentiment,
                         col = c("red","blue","green"))

# list all the words which involves in the lexicons in bing
bing_word <- tidy_plot_keyword %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()

# bar chart
bing_word %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(y = "Contribution to sentiment",
       x = NULL) +
  coord_flip()

# list all the words which involves in the lexicons in nrc
nrc_word <- tidy_plot_keyword %>%
  inner_join(get_sentiments("nrc")) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()

# har chart
nrc_word %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(y = "Contribution to sentiment",
       x = NULL) +
  coord_flip()

# bing filter in nrc
bing_nrc_word <- nrc_word %>%
  filter(sentiment == "positive" | sentiment == "negative") %>%
  inner_join(get_sentiments("bing"))

# bar chart
bing_nrc_word %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(y = "Plot Keyword Count",
       x = NULL) +
  coord_flip()

# Wordclouds
tidy_plot_keyword %>%
  anti_join(stop_words) %>%
  count(word) %>%
  with(wordcloud(word, n, max.words = 100))

tidy_plot_keyword %>%
  inner_join(get_sentiments("bing")) %>%
  inner_join(get_sentiments("nrc")) %>%
  count(word, sentiment, sort = TRUE) %>%
  acast(word ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("gray20", "gray80"),
                   max.words = 100)

# pie chart (profit vs loss)
profit_loss <- c(abs(sum(dataset$profit[dataset$profit > 0])),abs(sum(dataset$profit[dataset$profit < 0])))
labels <- c("profit", "loss")
pie_percent <- round(100 * profit_loss / sum(profit_loss), 1)

png(file = "profit_loss_percentage.jpg")

pie(profit_loss,
    labels = pie_percent,
    col = rainbow(length(profit_loss))
    )
legend("topright", labels, cex = 0.8, fill = rainbow(length(profit_loss)))

# save the file
dev.off()

# pie chart positive
average_sentiment <- tidy_plot_keyword %>%
  inner_join(get_sentiments("nrc")) %>%
  filter(sentiment %in% c("positive", 
                          "negative")) %>%
  mutate(sentiment_value = ifelse(sentiment %in% c('positive'), 1,
                                  -1)) %>%
  group_by(line) %>%
  summarise(sentiment_value = as.double(format(round(mean(sentiment_value), 5), nsmall = 5))) %>%
  count(sentiment_value, sort = TRUE) %>%
  mutate(average_sentiment = ifelse(sentiment_value > 0, "positive",
                                     ifelse(sentiment_value < 0, "negative",
                                            ifelse(sentiment_value == 0, "neutral", "")))) %>%
  ungroup() %>%
  select(average_sentiment) %>%
  as.vector() %>%
  unlist()

# Predictive analytics

# simple linear regression

# Fitting Simple Linear Regression to the Training set
regressor = lm(formula = gross ~ budget,
               data = training_set)
summary(regressor)
# Predicting the Test set results
y_pred = predict(regressor, newdata = test_set)

# Visualising the Training set results
ggplot() +
  geom_point(aes(x = training_set$budget, y = training_set$gross),
             colour = 'red') +
  geom_line(aes(x = training_set$budget, y = predict(regressor, newdata = training_set)),
            colour = 'blue') +
  ggtitle('Profit vs Budget (Training set)') +
  xlab('Budget') +
  ylab('Profit')

# Visualising the Test set results
ggplot() +
  geom_point(aes(x = test_set$budget, y = test_set$gross),
             colour = 'red') +
  geom_line(aes(x = training_set$budget, y = predict(regressor, newdata = training_set)),
            colour = 'blue') +
  ggtitle('Profit vs Budget (Test set)') +
  xlab('Budget') +
  ylab('Profit')

# KMeans
budget_profit <- dataset %>% select(profit,budget)
set.seed(123)
wcss <- vector()
for(i in 1:10)
  wcss[i] <- sum(kmeans(budget_profit, i)$withinss)
plot(1:10, wcss, type = "b", main = paste("Cluster of Movies"))

# Fitting K-Means to the dataset
set.seed(29)
kmeans = kmeans(budget_profit, 3)
y_kmeans = kmeans$cluster

# Visualising the clusters
library(cluster)
clusplot(budget_profit,
         y_kmeans,
         lines = 0,
         shade = TRUE,
         color = TRUE,
         labels = 2,
         plotchar = FALSE,
         span = TRUE,
         main = paste('Clusters of customers'),
         xlab = 'Profit',
         ylab = 'Budget')

# Logistic Regression
logistic_regression_dataset <- dataset %>%
  select(gross,profit,sentiment_value) %>%
  mutate(sentiment = ifelse(sentiment_value == "positive", 1,
                            ifelse(sentiment_value == "negative", 0, NA)))
logistic_regression_dataset$sentiment_value <- NULL
logistic_regression_dataset <- logistic_regression_dataset[!is.na(logistic_regression_dataset$sentiment),]

set.seed(123)
split = sample.split(logistic_regression_dataset$sentiment, SplitRatio = 0.75)
training_set = subset(logistic_regression_dataset, split == TRUE)
test_set = subset(logistic_regression_dataset, split == FALSE)

# Feature Scaling
training_set[-3] = scale(training_set[-3])
test_set[-3] = scale(test_set[-3])

# Fitting Logistic Regression to the Training set
classifier = glm(formula = sentiment ~ .,
                 family = binomial,
                 data = training_set)

# Predicting the Test set results
prob_pred = predict(classifier, type = 'response', newdata = test_set[-3])
y_pred = ifelse(prob_pred > 0.5, 1, 0)

# Making the Confusion Matrix
cm = table(test_set[, 3], y_pred > 0.5)

# Visualising the Training set results
library(ElemStatLearn)
set = training_set
X1 = seq(min(set[, 1]) - 1, max(set[, 1]) + 1, by = 0.01)
X2 = seq(min(set[, 2]) - 1, max(set[, 2]) + 1, by = 0.01)
grid_set = expand.grid(X1, X2)
colnames(grid_set) = c('Age', 'EstimatedSalary')
prob_set = predict(classifier, type = 'response', newdata = grid_set)
y_grid = ifelse(prob_set > 0.5, 1, 0)
plot(set[, -3],
     main = 'Logistic Regression (Training set)',
     xlab = 'Age', ylab = 'Estimated Salary',
     xlim = range(X1), ylim = range(X2))
contour(X1, X2, matrix(as.numeric(y_grid), length(X1), length(X2)), add = TRUE)
points(grid_set, pch = '.', col = ifelse(y_grid == 1, 'springgreen3', 'tomato'))
points(set, pch = 21, bg = ifelse(set[, 3] == 1, 'green4', 'red3'))

# Visualising the Test set results
library(ElemStatLearn)
set = test_set
X1 = seq(min(set[, 1]) - 1, max(set[, 1]) + 1, by = 0.01)
X2 = seq(min(set[, 2]) - 1, max(set[, 2]) + 1, by = 0.01)
grid_set = expand.grid(X1, X2)
colnames(grid_set) = c('Age', 'EstimatedSalary')
prob_set = predict(classifier, type = 'response', newdata = grid_set)
y_grid = ifelse(prob_set > 0.5, 1, 0)
plot(set[, -3],
     main = 'Logistic Regression (Test set)',
     xlab = 'Age', ylab = 'Estimated Salary',
     xlim = range(X1), ylim = range(X2))
contour(X1, X2, matrix(as.numeric(y_grid), length(X1), length(X2)), add = TRUE)
points(grid_set, pch = '.', col = ifelse(y_grid == 1, 'springgreen3', 'tomato'))
points(set, pch = 21, bg = ifelse(set[, 3] == 1, 'green4', 'red3'))




# Data Validation

