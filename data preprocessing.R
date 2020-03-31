# DATA PREPROCESSING

# read csv
dataset <- read.csv("data/movie_metadata.csv")
str(dataset)
summary(dataset)

# Dicard unnecessary columns
dataset$movie_imdb_link <- NULL

# Remove Duplicates
# calculate number of duplicate rows
sum(duplicated(dataset))
# delete duplicate rows
dataset <- distinct(dataset)

# Missing Values
# calculate total missing values
colSums(sapply(dataset, is.na))


dataset <- dataset[dataset$movie_facebook_likes != 0, ]

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
