library(tibble)
library(caTools)
library(dplyr)
library(tidytext)
library(tidyr)
library(ggplot2)
library(wordcloud)
library(reshape2)
library(VIM)          # kNN Imputation

# DATA PREPROCESSING

# Read csv
dataset <- read.csv("documents/R Programming/data/movie_metadata.csv", stringsAsFactors = FALSE)
str(dataset)
# quick look
glimpse(dataset)
summary(dataset)

# change to factor
dataset$country <- as.factor(dataset$country)
dataset$language <- as.factor(dataset$language)
dataset$duration <- as.numeric(dataset$duration)
dataset$budget <- as.numeric(dataset$budget)

# Remove Duplicates
# calculate number of duplicate rows
sum(duplicated(dataset))
# delete duplicate rows
dataset <- distinct(dataset)

summary(dataset)
# Missing Values
# calculate total missing values
colSums(sapply(dataset, is.na))

# NA imputation for all columns
for(column in 1:length(dataset))
  dataset[,c(column)][dataset[,c(column)] == 0 | dataset[,c(column)] == ""] <- NA

# mean imputation for duration
dataset$duration[is.na(dataset$duration)] <- mean(dataset$duration[!is.na(dataset$duration)])
# mean imputation for director_facebook_likes
dataset$director_facebook_likes[is.na(dataset$director_facebook_likes)] <- mean(dataset$director_facebook_likes[!is.na(dataset$director_facebook_likes)])
# mean imputation for actor_1_facebook_likes
dataset$actor_1_facebook_likes[is.na(dataset$actor_1_facebook_likes)] <- mean(dataset$actor_1_facebook_likes[!is.na(dataset$actor_1_facebook_likes)])
# mean imputation for actor_2_facebook_likes
dataset$actor_2_facebook_likes[is.na(dataset$actor_2_facebook_likes)] <- mean(dataset$actor_2_facebook_likes[!is.na(dataset$actor_2_facebook_likes)])
# mean imputation for actor_3_facebook_likes
dataset$actor_3_facebook_likes[is.na(dataset$actor_3_facebook_likes)] <- mean(dataset$actor_3_facebook_likes[!is.na(dataset$actor_3_facebook_likes)])
# mean imputation for facenumber_in_poster
dataset$facenumber_in_poster[is.na(dataset$facenumber_in_poster)] <- mean(dataset$facenumber_in_poster[!is.na(dataset$facenumber_in_poster)])

# median imputation for aspect_ratio
dataset$aspect_ratio[is.na(dataset$aspect_ratio)] <- median(dataset$aspect_ratio[!is.na(dataset$aspect_ratio)])


#remove rows
dataset <- dataset[!is.na(dataset$gross),]
dataset <- dataset[!is.na(dataset$budget),]
dataset <- dataset[!is.na(dataset$plot_keywords),]
dataset <- dataset[!is.na(dataset$country),]
dataset <- dataset[!is.na(dataset$language),]
dataset <- dataset[!is.na(dataset$movie_facebook_likes),]
dataset <- dataset[!is.na(dataset$content_rating),]

# Detect outlier
# duration
summary(dataset$duration)
boxplot(dataset$duration, main = "Duration")

upper_bench <- 120 + 1.5 * IQR(dataset$duration)
upper_bench
lower_bench <- 95 - 1.5 * IQR(dataset$duration)
lower_bench

dataset <- dataset[dataset$duration > lower_bench & dataset$duration < upper_bench,]
boxplot(dataset$duration, main = "Duration", outline = FALSE)
# num_critic_for_reviews
summary(dataset$num_critic_for_reviews)
boxplot(dataset$num_critic_for_reviews, main = "Number Critic For Reviews")

bench <- 222 + 1.5 * IQR(dataset$num_critic_for_reviews)
bench

dataset <- dataset[dataset$num_critic_for_reviews < bench,]
# title_year
summary(dataset$title_year)
boxplot(dataset$title_year, main = "Title Year", outline = FALSE)

bench <- 1999 - 1.5 * IQR(dataset$title_year)
bench

dataset <- dataset[dataset$title_year > bench,]
boxplot(dataset$title_year, main = "Title Year", outline = FALSE)

# movie_facebook_likes
summary(dataset$movie_facebook_likes)
boxplot(dataset$movie_facebook_likes, main = "Movie Facebook Likes")

bench <- 16000 + 1.5 * IQR(dataset$movie_facebook_likes)
bench

dataset <- dataset[dataset$movie_facebook_likes < bench,]
boxplot(dataset$movie_facebook_likes, main = "Gross", outline = FALSE)




# content_rating
table(dataset$content_rating)

dataset$content_rating[dataset$content_rating == "M"] <- 'PG'
dataset$content_rating[dataset$content_rating == "GP"] <- 'PG'
dataset$content_rating[dataset$content_rating == "X"] <- 'NC-17'

dataset$content_rating[dataset$content_rating == "Approved"] <- 'R'
dataset$content_rating[dataset$content_rating == "Passed"] <- 'R'
dataset$content_rating[dataset$content_rating == "Unrated"] <- 'R'
dataset$content_rating[dataset$content_rating == "Not Rated"] <- 'R'

dataset$content_rating <- factor(dataset$content_rating)

table(dataset$content_rating)

# language
dataset$country <- factor(dataset$country)
table(dataset$country)
plot(dataset$country)

levels(dataset$country) <- c(levels(dataset$country), "Others")
dataset$country[dataset$country != "USA" & dataset$country != "UK"] <- "Others"

dataset$country <- factor(dataset$country)
table(dataset$country)
plot(dataset$country)



# DATA TRANSFORMATION
dataset$id <- seq.int(nrow(dataset))
# Attribute Subset Selection
head(dataset)
aggregate(gross~language,dataset,sum)
aggregate(gross~title_year,dataset,sum)

# Dicard columns
# movie_imdb_link
dataset$movie_imdb_link <- NULL
# color
xtabs(~ color, data = dataset)
barplot(xtabs(~ color, data = dataset), xlab = "color", ylab = "number of movies")
dataset$color <- NULL

xtabs(~ director_name, data = dataset)
barplot(xtabs(~ director_name, data = dataset), xlab = "color", ylab = "number of movies")
dataset$director_name <- NULL
dataset$director_facebook_likes <- NULL
dataset$actor_1_name <- NULL
dataset$actor_2_name <- NULL
dataset$actor_3_name <- NULL
dataset$actor_1_facebook_likes <- NULL
dataset$actor_2_facebook_likes <- NULL
dataset$actor_3_facebook_likes <- NULL
dataset$facenumber_in_poster <- NULL
dataset$genres <- NULL

dataset <- dataset %>%
  mutate(profit = gross - budget) %>%
  mutate(profit = ifelse(profit > 0, 1, 0))

#dataset$profit <- rescale(dataset$profit)

# Generalization

tidy_plot_keyword <- tibble(line = 1:length(dataset[,1]),text = as.character(dataset$plot_keywords))
tidy_plot_keyword <- unnest_tokens(tidy_plot_keyword,word,text)


# tibble line-sentiment value
sentiment_category <- tidy_plot_keyword %>%
  inner_join(get_sentiments("nrc")) %>%
  filter(sentiment %in% c("positive", 
                          "negative")) %>%
  mutate(sentiment_value = ifelse(sentiment %in% c('positive'), 1,
                                  -1)) %>%
  group_by(line) %>%
  summarise(sentiment_value = mean(sentiment_value)) %>%
  #mutate(sentiment_value = ifelse(sentiment_value > 0, 1, 0)) %>%
  mutate(id = line) %>%
  data.frame()

dataset <- full_join(dataset,sentiment_category, by = 'id')
dataset <- dataset[!is.na(dataset$sentiment_value),]

dataset$sentiment_value = as.double(dataset$sentiment_value)

#dataset <- dataset %>%
  #mutate(sentiment_value = ifelse(sentiment_value > 0, 1, 0))

# encoding categorical variable
dataset$country = factor(dataset$country,
                              levels = c("USA", "UK", "Others"),
                              labels = c(0, 1, 2))

dataset$content_rating = factor(dataset$content_rating,
                         levels = c("G", "NC-17", "PG", "PG-13", "R"),
                         labels = c(0, 1, 2, 3, 4))

#dataset$aspect_ratio = factor(dataset$aspect_ratio,
 #                             levels = c(""))

clean_dataset <- dataset %>%
  select("profit","budget","duration","sentiment_value","aspect_ratio","content_rating","aspect_ratio","movie_facebook_likes")



# Data Partition

# Splitting the dataset into the Training set and Test set
set.seed(123)
split = sample.split(clean_dataset$profit, SplitRatio = 2/3)
training_set = subset(clean_dataset, split == TRUE)
test_set = subset(clean_dataset, split == FALSE)

summary(glm(profit ~ . , training_set , family = binomial))


