library(tibble)
library(dplyr)
library(tidytext)
library(ggplot2)
library(wordcloud)
library(reshape2)

# tidy_plot_keyword
tidy_plot_keyword <- dataset$genres

plot_keywords_df <- dataset$plot_keywords
plot_keywords_df <- mutate(plot_keywords_df, text=dataset$plot_keywords)
plot_keywords_df <- data.frame("plot keywords" = as.character(dataset$plot_keywords))
plot_keywords_df <- dataset$plot_keywords

tidy_plot_keyword <- tibble(line = 1:5043,text = plot_keywords_df)
tidy_plot_keyword <- unnest_tokens(tidy_plot_keyword,word,text)

# Comparing the three sentiment dictionaries
afinn <- tidy_plot_keyword %>%
  group_by(index = line) %>%
  inner_join(get_sentiments("afinn")) %>% 
  summarise(sentiment = sum(value)) %>% 
  mutate(method = "AFINN")

bing_and_nrc <- bind_rows(tidygenres %>% 
                            unnest_tokens(word,text) %>%
                            inner_join(get_sentiments("bing")) %>%
                            mutate(method = "Bing"),
                          tidygenres %>% 
                            unnest_tokens(word,text) %>%
                            inner_join(get_sentiments("loughran")) %>%
                            mutate(method = "Loughran"),
                          tidygenres %>% 
                            unnest_tokens(word,text) %>%
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


tidygenres %>% 
  unnest_tokens(word,text) %>%
  inner_join(get_sentiments("nrc")) %>%
  filter(sentiment %in% c("positive", 
                          "negative")) %>% 
  count(sentiment)

tidygenres %>% 
  unnest_tokens(word,text) %>%
  inner_join(get_sentiments("afinn")) %>%
  count(value)

tidygenres %>% 
  unnest_tokens(word,text) %>%
  inner_join(get_sentiments("bing")) %>%
  filter(sentiment %in% c("positive", 
                          "negative")) %>% 
  count(sentiment)

tidygenres %>% 
  unnest_tokens(word,text) %>%
  inner_join(get_sentiments("loughran")) %>%
  filter(sentiment %in% c("positive", 
                          "negative")) %>% 
  count(sentiment)



bing_word_counts <- tidygenres %>%
  unnest_tokens(word,text) %>%
  inner_join(get_sentiments("nrc")) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()

bing_nrc <- bing_word_counts %>%
  filter(sentiment == "positive" | sentiment == "negative") %>%
  inner_join(get_sentiments("bing"))

##
bing_nrc <- bing_word_counts %>%
  filter(sentiment == "positive" | sentiment == "negative") %>%
  inner_join(get_sentiments("bing")) %>%
  filter(word == "love" | word == "death" | word == "murder") %>%
  count(word, sentiment, sort = TRUE)

bing_nrc %>%
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

bing_word_counts %>%
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

# Wordclouds
tidygenres %>%
  unnest_tokens(word,text) %>%
  anti_join(stop_words) %>%
  count(word) %>%
  with(wordcloud(word, n, max.words = 100, scale=c(0.5,0.75)))

tidygenres %>%
  unnest_tokens(word,text) %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  acast(word ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("gray20", "gray80"),
                   max.words = 100)
