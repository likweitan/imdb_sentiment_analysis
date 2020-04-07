library(tibble)
library(caTools)
library(dplyr)
library(tidytext)
library(tidyr)
library(ggplot2)
library(wordcloud)
library(reshape2)
library(VIM)          # kNN Imputation
library(quantmod)     # get currency

# DATA PREPROCESSING

# Read csv
dataset <- read.csv("documents/R Programming/data/movie_metadata.csv", stringsAsFactors = TRUE)
str(dataset)
# quick look
glimpse(dataset)
summary(dataset)

# Remove Duplicates
# calculate number of duplicate rows
sum(duplicated(dataset))
# delete duplicate rows
dataset <- distinct(dataset)

summary(dataset)

# NA imputation for all columns
for(column in 1:length(dataset))
  dataset[,c(column)][dataset[,c(column)] == 0 | dataset[,c(column)] == ""] <- NA



# Detect outlier
# duration
summary(dataset$duration)
hist(dataset$duration, main = "Duration")
boxplot(dataset$duration, horizontal = TRUE, main = "Duration")

upper_bench <- 118 + 1.5 * IQR(dataset$duration, na.rm = TRUE)
upper_bench
lower_bench <- 93 - 1.5 * IQR(dataset$duration, na.rm = TRUE)
lower_bench

dataset <- subset(dataset,duration < upper_bench & duration > lower_bench)

boxplot(dataset$duration, horizontal = TRUE,main = "Duration")
# num_critic_for_reviews
summary(dataset$num_critic_for_reviews)
boxplot(dataset$num_critic_for_reviews, horizontal = TRUE, main = "Number Critic For Reviews")

upper_bench <- 194 + 1.5 * IQR(dataset$num_critic_for_reviews, na.rm = TRUE)
upper_bench
lower_bench <- 51 - 1.5 * IQR(dataset$num_critic_for_reviews, na.rm = TRUE)
lower_bench

dataset <- subset(dataset,num_critic_for_reviews < upper_bench & num_critic_for_reviews > lower_bench)
# budget
summary(dataset$budget)
boxplot(dataset$budget, main = "Budget")

upper_bench <- 12220000000 + 1.5 * IQR(dataset$budget, na.rm = TRUE)
upper_bench
lower_bench <- 6000000 - 1.5 * IQR(dataset$budget, na.rm = TRUE)
lower_bench

dataset <- subset(dataset,budget < upper_bench & budget > lower_bench)
boxplot(dataset$budget, main = "Budget", outline = FALSE)
# gross
summary(dataset$gross)
boxplot(dataset$gross, main = "Gross")

upper_bench <- 58918501 + 1.5 * IQR(dataset$gross, na.rm = TRUE)
upper_bench
lower_bench <- 6105175 - 1.5 * IQR(dataset$gross, na.rm = TRUE)
lower_bench

dataset <- subset(dataset,gross < upper_bench & gross > lower_bench)
boxplot(dataset$gross, main = "Gross", outline = FALSE)


# Missing Values
# calculate total missing values
colSums(sapply(dataset, is.na))

# linear regression imputation
columns <- c()
for(i in 1:dim(dataset)[2])
{
  if(is.numeric(dataset[,i])|| is.integer(dataset[,i]))
  {
    columns[i]=T
  }
  else
  {
    columns[i]=F
  }
}
temp <- na.omit(dataset[,columns])

correlation <- c()
for(i in 1:dim(temp)[2])
{
  correlation[i] <- cor(temp[,i],temp[,'movie_facebook_likes'])
}

symnum(correlation)

# create indicator variable
Ind <- function(t)
{
  x <- dim(length(t))
  x[which(!is.na(t))] = 1
  x[which(is.na(t))] = 0
  return(x)
}
dataset$I <- Ind(dataset$movie_facebook_likes)

lm(movie_facebook_likes~num_critic_for_reviews, data = dataset)
summary(lm(movie_facebook_likes~num_critic_for_reviews, dataset))
# Missing Value imputation
for(i in 1:nrow(dataset))
  if(dataset$I[i] == 0)
    dataset$movie_facebook_likes[i] = round(-4051.6 + 102.2 * dataset$num_critic_for_reviews[i])
dataset$I <- NULL

# linear regression imputation for cast_total_facebook_likes
for(i in 1:dim(temp)[2])
{
  correlation[i] <- cor(temp[,i],temp[,'cast_total_facebook_likes'])
}

symnum(correlation)

# create indicator variable
dataset$I <- Ind(dataset$cast_total_facebook_likes)

lm(cast_total_facebook_likes~actor_1_facebook_likes, data = dataset)
summary(lm(cast_total_facebook_likes~actor_1_facebook_likes, dataset))
# Missing Value imputation
for(i in 1:nrow(dataset))
  if(dataset$I[i] == 0)
    dataset$cast_total_facebook_likes[i] = round(2238.182 + 1.125 * dataset$actor_1_facebook_likes[i],0)
dataset$I <- NULL

# mean imputation for director_facebook_likes
dataset$director_facebook_likes[is.na(dataset$director_facebook_likes)] <- round(mean(dataset$director_facebook_likes[!is.na(dataset$director_facebook_likes)]),0)
# mean imputation for actor_1_facebook_likes
dataset$actor_1_facebook_likes[is.na(dataset$actor_1_facebook_likes)] <- round(mean(dataset$actor_1_facebook_likes[!is.na(dataset$actor_1_facebook_likes)]),0)
# mean imputation for actor_2_facebook_likes
dataset$actor_2_facebook_likes[is.na(dataset$actor_2_facebook_likes)] <- round(mean(dataset$actor_2_facebook_likes[!is.na(dataset$actor_2_facebook_likes)]),0)
# mean imputation for actor_3_facebook_likes
dataset$actor_3_facebook_likes[is.na(dataset$actor_3_facebook_likes)] <- round(mean(dataset$actor_3_facebook_likes[!is.na(dataset$actor_3_facebook_likes)]),0)
# mean imputation for facenumber_in_poster
dataset$facenumber_in_poster[is.na(dataset$facenumber_in_poster)] <- round(mean(dataset$facenumber_in_poster[!is.na(dataset$facenumber_in_poster)]),0)
# mean imputation for cast_total_facebook_likes
dataset$cast_total_facebook_likes[is.na(dataset$cast_total_facebook_likes)] <- round(mean(dataset$cast_total_facebook_likes[!is.na(dataset$cast_total_facebook_likes)]),0)

# median imputation for aspect_ratio
dataset$aspect_ratio[is.na(dataset$aspect_ratio)] <- round(median(dataset$aspect_ratio[!is.na(dataset$aspect_ratio)]),2)

# convert currency
getFX("KRW/USD", from = Sys.Date()-2, to = Sys.Date()-2)
KRW <- as.data.frame(KRWUSD) %>%
  as.numeric()
getFX("JPY/USD", from = Sys.Date()-2, to = Sys.Date()-2)
JPY <- as.data.frame(JPYUSD) %>%
  as.numeric()
getFX("TRY/USD", from = Sys.Date()-2, to = Sys.Date()-2)
TRY <- as.data.frame(TRYUSD) %>%
  as.numeric()
getFX("HUF/USD", from = Sys.Date()-2, to = Sys.Date()-2)
HUF <- as.data.frame(HUFUSD) %>%
  as.numeric()
getFX("THB/USD", from = Sys.Date()-2, to = Sys.Date()-2)
THB <- as.data.frame(THBUSD) %>%
  as.numeric()

dataset <- transform(dataset, budget = ifelse(country == "South Korea", budget*KRW, budget))
dataset <- transform(dataset, budget = ifelse(country == "Japan", budget*JPY, budget))
dataset <- transform(dataset, budget = ifelse(country == "Turkey", budget*TRY, budget))
dataset <- transform(dataset, budget = ifelse(country == "Hungary", budget*HUF, budget))
dataset <- transform(dataset, budget = ifelse(country == "Thailand", budget*THB, budget))

dataset <- transform(dataset, gross = ifelse(country == "South Korea", gross*KRW, gross))
dataset <- transform(dataset, gross = ifelse(country == "Japan", gross*JPY, gross))
dataset <- transform(dataset, gross = ifelse(country == "Turkey", gross*TRY, gross))
dataset <- transform(dataset, gross = ifelse(country == "Hungary", gross*HUF, gross))
dataset <- transform(dataset, gross = ifelse(country == "Thailand", gross*THB, gross))

# remove rows
dataset <- dataset[!is.na(dataset$plot_keywords),]
dataset <- dataset[!is.na(dataset$language),]

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



dataset <- dataset %>%
  mutate(profit = ifelse(gross - budget > 0, 1, 0))

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

dataset$line <- NULL
dataset$id <- NULL

dataset$sentiment_value = as.double(dataset$sentiment_value)

# encoding categorical variable
dataset$duration <- cut(dataset$duration,
               breaks = c(0,60,120,180),
               labels = c(0,1,2))

dataset$country <- factor(dataset$country,
                              levels = c("USA", "UK", "Others"),
                              labels = c(0, 1, 2))

dataset$content_rating <- factor(dataset$content_rating,
                         levels = c("G", "NC-17", "PG", "PG-13", "R"),
                         labels = c(0, 1, 2, 3, 4))

dataset$sentiment_value <- cut(dataset$sentiment_value,
                        breaks = c(-2,0,2),
                        labels = c(0,1))

# Normalization
# Feature Scaling
dataset[-c(2,11,16,18,19)] <- scale(dataset[-c(2,11,16,18,19)])

# Feature Selection
# movie_imdb_link
dataset$movie_imdb_link <- NULL
# movie_title
dataset$movie_title <- NULL
# plot_keywords
dataset$plot_keywords <- NULL
# actor_1_name
dataset$actor_1_name <- NULL
# actor_2_name
dataset$actor_2_name <- NULL
dataset$actor_3_name <- NULL
dataset$genres <- NULL
xtabs(~ color, data = dataset)
barplot(xtabs(~ color, data = dataset), xlab = "color", ylab = "number of movies")
dataset$color <- NULL
xtabs(~ director_name, data = dataset)
barplot(xtabs(~ director_name, data = dataset), xlab = "color", ylab = "number of movies")
dataset$director_name <- NULL

# stepwise selection
model <- lm(profit~.,dataset)
stepAIC(model,direction = "both")

#write.csv(dataset, file="cleaned.csv")
# Data Partition

# Splitting the dataset into the Training set and Test set
set.seed(123)
split = sample.split(dataset$profit, SplitRatio = 2/3)
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)



