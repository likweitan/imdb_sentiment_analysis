# Descriptive Analytics

# Plot no of movies
ggplot(dataset,aes(title_year)) +
  geom_bar()

# Comparing the three sentiment dictionaries
bind_rows(tidy_plot_keyword %>%
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