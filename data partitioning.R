# SAMPLING
# split dataset into train
toyota <- read.csv("data/ToyotaCorolla.csv")
# create a reference to a sample of 2, 80 and 20 split
samplediv <- sample(2,nrow(toyota),replace=TRUE,prob=c(0.8,0.2))

# assign a dataset of 80%
traindata <- toyota[samplediv == 1,]
# assign a dataset of 20%
validatedata <- toyota[samplediv == 2,]

head(traindata)

head(validatedata)