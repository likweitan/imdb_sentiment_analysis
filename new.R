movie_dataset <- read.csv("documents/R Programming/data/movie_metadata.csv", stringsAsFactors = FALSE)
class(movie)
head(movie)
colnames(movie)
table(movie$movie_title)

#missing value analysis
# NA imputation for all columns
for(column in 1:length(movie_dataset))
  movie_dataset[,c(column)][movie_dataset[,c(column)] == 0 | movie_dataset[,c(column)] == ""] <- NA

colSums(sapply(movie_dataset, is.na))
sum(complete.cases(movie_dataset))


# mean imputation for gross
#movie_dataset$gross[is.na(movie_dataset$gross)] <- mean(movie_dataset$gross, na.rm = TRUE)
# mean imputation for budget
#movie_dataset$budget[is.na(movie_dataset$budget)] <- mean(movie_dataset$budget, na.rm = TRUE)

for(i in 1:nrow(movie_dataset))
  if(is.na(movie_dataset$actor_1_facebook_likes[i]) & is.na(movie_dataset$actor_2_facebook_likes[i]) & !is.na(movie_dataset$actor_3_facebook_likes[i]) & !is.na(movie_dataset$cast_total_facebook_likes[i]))
    movie_dataset$actor_1_facebook_likes[i] = movie_dataset$cast_total_facebook_likes[i] - movie_dataset$actor_2_facebook_likes[i] - movie_dataset$actor_3_facebook_likes[i]

movie_dataset$actor_1_facebook_likes[is.na(movie_dataset$actor_1_facebook_likes)] = movie_dataset$cast_total_facebook_likes - movie_dataset$actor_2_facebook_likes - movie_dataset$actor_3_facebook_likes
movie_dataset$actor_2_facebook_likes = movie_dataset$cast_total_facebook_likes - movie_dataset$actor_1_facebook_likes - movie_dataset$actor_3_facebook_likes
movie_dataset$actor_3_facebook_likes = movie_dataset$cast_total_facebook_likes - movie_dataset$actor_1_facebook_likes - movie_dataset$actor_2_facebook_likes


#cleaning
movie_dataset$movie_title <- (sapply(movie_dataset$movie_title,gsub,pattern="Â",replacement=""))
movie_dataset$genres <- (sapply(movie_dataset$genres,gsub,pattern="\\|",replacement=" "))
movie_dataset$plot_keywords <- (sapply(movie_dataset$plot_keywords,gsub,pattern="\\|",replacement=" "))

#remove duplicates
movie_dataset = movie_dataset[!duplicated(movie_dataset$movie_title),]

#clean revenue and budget
unique(movie_dataset$country)
summary(movie_dataset$gross)

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

movie_dataset <- transform(movie_dataset, budget = ifelse(country == "South Korea", budget*KRW, budget))
movie_dataset <- transform(movie_dataset, budget = ifelse(country == "Japan", budget*JPY, budget))
movie_dataset <- transform(movie_dataset, budget = ifelse(country == "Turkey", budget*TRY, budget))
movie_dataset <- transform(movie_dataset, budget = ifelse(country == "Hungary", budget*HUF, budget))
movie_dataset <- transform(movie_dataset, budget = ifelse(country == "Thailand", budget*THB, budget))

movie_dataset <- transform(movie_dataset, gross = ifelse(country == "South Korea", gross*KRW, gross))
movie_dataset <- transform(movie_dataset, gross = ifelse(country == "Japan", gross*JPY, gross))
movie_dataset <- transform(movie_dataset, gross = ifelse(country == "Turkey", gross*TRY, gross))
movie_dataset <- transform(movie_dataset, gross = ifelse(country == "Hungary", gross*HUF, gross))
movie_dataset <- transform(movie_dataset, gross = ifelse(country == "Thailand", gross*THB, gross))

movie_dataset$profit <- as.factor(ifelse((movie_dataset$gross > movie_dataset$budget),1,0))


symnum(cor(movie_dataset[,c("num_critic_for_reviews","num_user_for_reviews","duration",
                            "director_facebook_likes",
                            "actor_1_facebook_likes","actor_2_facebook_likes","actor_3_facebook_likes",
                            "imdb_score","title_year",
                            "budget","gross","aspect_ratio","movie_facebook_likes")], use = "complete.obs"))

# Genre
genre <- Corpus(VectorSource(movie_dataset$genres_2))
genre_dtm <- DocumentTermMatrix(genre)
genre_freq <- colSums(as.matrix(genre_dtm))
freq <- sort(colSums(as.matrix(genre_dtm)), decreasing=TRUE) 
genre_wf <- data.frame(word=names(genre_freq), freq=genre_freq)

ggplot(genre_wf, aes(x=reorder(word,-freq), y=freq))+ 
  geom_bar(stat="identity")+
  theme(axis.text.x=element_text(angle=45, hjust=1))+
  ggtitle("Movie Genre frequency graph")+
  xlab("Genre")+
  ylab("Frequency")

# Country

# IMDB
ggplot(movie_dataset,aes(imdb_score))+
  geom_histogram(bins=80)+
  ylab("Count of Movies")+
  xlab("IMDB Score")+
  ggtitle("Histogram: IMDB Score")

