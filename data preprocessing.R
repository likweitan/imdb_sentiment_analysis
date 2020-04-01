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

dataset$duration <- as.numeric(dataset$duration)
dataset$budget <- as.numeric(dataset$budget)


# Discard rows
dataset <- dataset[dataset$plot_keywords!="",]

# Remove Duplicates
# calculate number of duplicate rows
sum(duplicated(dataset))
# delete duplicate rows
dataset <- distinct(dataset)

summary(dataset)
# Missing Values
# calculate total missing values
colSums(sapply(dataset, is.na))

# NA imputation for language
dataset$language[dataset$language == ""] <- NA
# NA imputation for content_rating
dataset$content_rating[dataset$content_rating == ""] <- NA

dataset$movie_facebook_likes[dataset$movie_facebook_likes == 0] <- NA

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

# kNN imputation for title_year
dataset <- kNN(dataset, variable = c("language","content_rating","num_critic_for_reviews","num_user_for_reviews","title_year","budget","gross","movie_facebook_likes"), k = 5, imp_var =FALSE)

dataset$title_year <- as.integer(dataset$title_year)



# Detect outlier
# duration
summary(dataset$duration)
boxplot(dataset$duration, main = "Duration")

upper_bench <- 115 + 1.5 * IQR(dataset$duration)
upper_bench
lower_bench <- 92 - 1.5 * IQR(dataset$duration)
lower_bench

dataset <- dataset[dataset$duration > lower_bench & dataset$duration < upper_bench,]
boxplot(dataset$duration, main = "Duration", outline = FALSE)
# num_critic_for_reviews
summary(dataset$num_critic_for_reviews)
boxplot(dataset$num_critic_for_reviews, main = "Number Critic For Reviews")

bench <- 196 + 1.5 * IQR(dataset$num_critic_for_reviews)
bench

dataset <- dataset[dataset$num_critic_for_reviews < bench,]
# title_year
summary(dataset$title_year)
boxplot(dataset$title_year, main = "Title Year", outline = FALSE)

bench <- 1999 - 1.5 * IQR(dataset$title_year)
bench

dataset <- dataset[dataset$title_year > bench,]
boxplot(dataset$title_year, main = "Title Year", outline = FALSE)
# budget
summary(dataset$budget)
boxplot(dataset$budget, main = "Budget", outline = FALSE)

bench <- 40000000 + 1.5 * IQR(dataset$budget)
bench

dataset <- dataset[dataset$budget < bench,]
boxplot(dataset$budget, main = "Budget", outline = FALSE)
# gross
summary(dataset$gross)
boxplot(dataset$gross, main = "Gross")

bench <- 54953159 + 1.5 * IQR(dataset$gross)
bench

dataset <- dataset[dataset$gross < bench,]
boxplot(dataset$gross, main = "Gross", outline = FALSE)
# movie_facebook_likes
summary(dataset$movie_facebook_likes)
boxplot(dataset$movie_facebook_likes, main = "Movie Facebook Likes")

bench <- 13000 + 1.5 * IQR(dataset$movie_facebook_likes)
bench

dataset <- dataset[dataset$movie_facebook_likes < bench,]
boxplot(dataset$movie_facebook_likes, main = "Gross", outline = FALSE)



# content rating
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

# Plot no of movies
ggplot(dataset,aes(title_year)) +
  geom_bar()

# DATA TRANSFORMATION
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

dataset_1 <- dataset %>%
              select(c("movie_title",))

dataset <- dataset %>%
  mutate(profit = gross - budget)

# Generalization
# create dummy variables
# genres
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
genres <- as.data.frame(as.matrix(dtm))
#dataset <- cbind(dataset,genres)

# plot_keyword
dataset$plot_keywords <- gsub("\\|", " ", dataset$plot_keywords)
corpus = VCorpus(VectorSource(dataset$plot_keywords))
corpus = tm_map(corpus, content_transformer(tolower))
as.character(corpus[[1]])
corpus = tm_map(corpus, removeNumbers)
corpus = tm_map(corpus, removePunctuation)
corpus = tm_map(corpus, removeWords, stopwords("en"))
corpus = tm_map(corpus, stripWhitespace)
# create the bag of words model
dtm <- DocumentTermMatrix(corpus)
dtm <- removeSparseTerms(dtm,0.999)
plot_keywords <- as.data.frame(as.matrix(dtm))
#dataset <- cbind(dataset,plot_keywords)

tidy_plot_keyword <- tibble(line = 1:4846,text = as.character(dataset$plot_keywords))
tidy_plot_keyword <- unnest_tokens(tidy_plot_keyword,word,text)

# Comparing the three sentiment dictionaries
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
  mutate(sentiment = positive - negative) %>%
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
  mutate(sentiment_value = ifelse(sentiment_value > 0.5, 1,
  ifelse(sentiment_value > 0, 2,
         ifelse(sentiment_value > -0.5, 3, 4)))) %>%
  mutate(id = line) %>%
  data.frame()

dataset <- full_join(dataset,line_sentiment_category, by = 'id')
dataset <- dataset[!is.na(dataset$sentiment_value),]

sentiment_profit <- dataset %>%
  select(profit,sentiment_value) %>%
  group_by(sentiment_value)

sentiment_profit %>%
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

# Data Partition

# Splitting the dataset into the Training set and Test set
set.seed(123)
split = sample.split(dataset$profit, SplitRatio = 2/3)
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)
