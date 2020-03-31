# DATA PREPROCESSING

library(dplyr)
library(tm)
library(sentimentr)
library(wordcloud)
library(tidytext)
library(tokenizers)
library(SnowballC)

# read csv
dataset <- read.csv("data/movie_metadata.csv")
str(dataset)
summary(dataset)

# Dicard unnecessary columns
dataset$movie_imdb_link <- NULL

table(dataset$color)
# Discard color
dataset$color <- NULL


# Remove Duplicates
# calculate number of duplicate rows
sum(duplicated(dataset))
# delete duplicate rows
dataset <- distinct(dataset)

# Missing Values
# calculate total missing values
colSums(sapply(dataset, is.na))

#
dataset <- dataset[!is.na(dataset$gross),]
dataset <- dataset[!is.na(dataset$budget),]

# aspect ratio
table(dataset$aspect_ratio)
dataset$aspect_ratio[is.na(dataset$aspect_ratio)] <- 0
mean(dataset$budget[dataset$aspect_ratio == 1.85])
mean(dataset$budget[dataset$aspect_ratio == 2.35])
mean(dataset$budget[dataset$aspect_ratio != 1.85 & dataset$aspect_ratio != 2.35])

# check the mean of all duration values without the na
dataset$duration[is.na(dataset$duration)] = mean(dataset$duration[!is.na(dataset$duration)])
# Check the mean
dataset$gross[is.na(dataset$gross)] = mean(dataset$gross[!is.na(dataset$gross)])

dataset$num_critic_for_reviews[is.na(dataset$num_critic_for_reviews)] = mean(dataset$num_critic_for_reviews[!is.na(dataset$num_critic_for_reviews)])

dataset$actor_1_facebook_likes[is.na(dataset$actor_1_facebook_likes)] = mean(dataset$actor_1_facebook_likes[!is.na(dataset$actor_1_facebook_likes)])

dataset$actor_2_facebook_likes[is.na(dataset$actor_2_facebook_likes)] = mean(dataset$actor_2_facebook_likes[!is.na(dataset$actor_2_facebook_likes)])

dataset$actor_3_facebook_likes [is.na(dataset$actor_3_facebook_likes)] = mean(dataset$actor_3_facebook_likes [!is.na(dataset$actor_3_facebook_likes )])

dataset$facenumber_in_poster [is.na(dataset$facenumber_in_poster)] = mean(dataset$facenumber_in_poster [!is.na(dataset$facenumber_in_poster )])

# Discard outliers
summary(dataset$num_critic_for_reviews)
boxplot(dataset$num_critic_for_reviews)
bench = 120 + 1.5*IQR(dataset$num_critic_for_reviews)
dataset$num_critic_for_reviews
dataset$num_critic_for_reviews[dataset$num_critic_for_reviews>bench] <- bench

# Data Transformation

table(dataset$content_rating)

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

# country
table(dataset$country)

# Data Mining
dataset$plot_keyword <- gsub("\\|", " ", dataset$plot_keyword)

dataset$genres <- gsub("\\|", " ", dataset$genres)

plot_keyword <- VectorSource(dataset$plot_keyword)
corpus <- Corpus(plot_keyword)

inspect(corpus[1:3])

corpus <- tm_map(corpus,stripWhitespace)

tdm <- TermDocumentMatrix(corpus)
tdm
tdm <- as.Matrix(tdm)
tdm
# Bar plot
w <- rowSums(tdm)

dataset$plot_keyword %>%
  inner_join(get_sentiments("nrc")) %>%
  count(word, sort=TRUE)
  
  
  
get_sentiments("afinn") %>%
  filter(sentiment >0)

sentiment(dataset$plot_keyword)

wordcloud(dataset$genre, max.words=20,color=rainbow(3))

tokenize_words(dataset$genres)
count_words(dataset$genres)

# clean the text
corpus = VCorpus(VectorSource(dataset$genres))
corpus = tm_map(corpus, content_transformer(tolower))
as.character(corpus[[1]])
corpus = tm_map(corpus, removeNumbers)
corpus = tm_map(corpus, removePunctuation)
corpus = tm_map(corpus, removeWords, stopwords("en"))
# Find the root of the word
#corpus = tm_map(corpus, stemDocument)
corpus = tm_map(corpus, stripWhitespace)

# create the bag of words model
dtm <- DocumentTermMatrix(corpus)
dtm <- removeSparseTerms(dtm,0.999)

plot_keyword <- as.data.frame(as.matrix(dtm))

# Encoding the target feature as factor
plot_keyword$movie_facebook_likes <- dataset$movie_facebook_likes
#plot_keyword$movie_facebook_likes = factor(plot_keyword$movie_facebook_likes, levels = c(0, 1))

# Splitting the dataset into the Training set and Test set
# install.packages('caTools')
library(caTools)
set.seed(123)
split = sample.split(plot_keyword$movie_facebook_likes, SplitRatio = 0.8)
training_set = subset(plot_keyword, split == TRUE)
test_set = subset(plot_keyword, split == FALSE)

# Fitting Random Forest Classification to the Training set
# install.packages('randomForest')
library(randomForest)
classifier = randomForest(x = training_set[-3806],
                          y = training_set$movie_facebook_likes,
                          ntree = 15)

# Predicting the Test set results
y_pred = predict(classifier, newdata = test_set[-3806])

# Making the Confusion Matrix
cm = table(test_set[, 692], y_pred)

# Splitting the dataset into the Training set and Test set
# install.packages('caTools')
library(caTools)
set.seed(123)
split = sample.split(plot_keyword$movie_facebook_likes, SplitRatio = 0.75)
training_set = subset(plot_keyword, split == TRUE)
test_set = subset(plot_keyword, split == FALSE)

backwardElimination <- function(x, sl) {
  numVars = length(x)
  for (i in c(1:numVars)){
    regressor = lm(formula = movie_facebook_likes ~ ., data = x)
    maxVar = max(coef(summary(regressor))[c(2:numVars), "Pr(>|t|)"])
    if (maxVar > sl){
      j = which(coef(summary(regressor))[c(2:numVars), "Pr(>|t|)"] == maxVar)
      x = x[, -j]
    }
    numVars = numVars - 1
  }
  return(summary(regressor))
}

SL = 0.05
backwardElimination(training_set, SL)


# Fitting Multiple Linear Regression to the Training set
regressor = lm(formula = movie_facebook_likes ~ .,
               data = training_set)

# Predicting the Test set results
y_pred = predict(regressor, newdata = test_set)

model = lm(movie_facebook_likes~., data = plot_keyword)

for(i in 1:21)
{
mean(plot_keyword$movie_facebook_likes[plot_keyword[21] == 1])
}
