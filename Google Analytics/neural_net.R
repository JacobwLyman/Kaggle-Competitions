
####Load Data####
library(tidyverse)

setwd("/home/jacob_w_lyman/home")

train <- read_csv("my_train.csv") %>%
  select(-X1)
test <- read_csv("my_test.csv") %>%
  select(-X1)


####Clean Data####
train <- train %>%
  mutate(channelGrouping = as.factor(channelGrouping)
         , date = date
         , fullVisitorId = as.factor(fullVisitorId)
         , visitId = as.factor(visitId)
         , visitNumber = as.factor(visitNumber)
         , visitStartTime = as.factor(visitStartTime)
         , browser = as.factor(browser)
         , operatingSystem = as.factor(operatingSystem)
         , isMobile = as.logical(isMobile)
         , deviceCategory = as.factor(deviceCategory)
         , continent = as.factor(continent)
         , subContinent = as.factor(subContinent)
         , country = as.factor(country)
         , region = as.factor(region)
         , metro = as.factor(metro)
         , city = as.factor(city)
         , networkDomain = as.factor(networkDomain)
         , hits1 = as.numeric(hits1)
         , pageviews = as.numeric(pageviews)
         , bounces = as.numeric(bounces)
         , newVisits = as.numeric(newVisits)
         , sessionQualityDim = as.numeric(sessionQualityDim)
         , timeOnSite = as.numeric(timeOnSite)
         , transactions = as.numeric(transactions)
         , transactionRevenue = as.numeric(transactionRevenue)
         , totalTransactionRevenue = as.numeric(totalTransactionRevenue)
         , campaign = as.factor(campaign)
         , source = source
         , medium = as.factor(medium)
         , keyword = as.factor(keyword)
         , referralPath = referralPath 
         , isTrueDirect = isTrueDirect
         , adContent = as.factor(adContent)
  )

train <- train %>%
  mutate(timeOnSite = if_else(bounces == 1, 0, timeOnSite))

train$isTrueDirect[is.na(train$isTrueDirect)] <- 'False'
train <- train %>%
  mutate(isTrueDirect = as.logical(isTrueDirect))

sum(is.na(train$date))

df_dates <- as.data.frame(unique(train$date))

# train2 <- train %>%
#   mutate(date = as.Date.numeric(date, "%Y%m%d"))

# Remove NAs
train$transactionRevenue[is.na(train$transactionRevenue)] <- 0
train$transactions[is.na(train$transactions)] <- 0
train$totalTransactionRevenue[is.na(train$totalTransactionRevenue)] <- 0


#### Feature Engineering ####


####Neural Network #####
install.packages('caret')
library(caret)

#Create dummy variables of important factor columns for matrix
newtrain <- train %>%
  select(channelGrouping
         , browser
         , operatingSystem)
newtrain <- model.matrix(~., data = newtrain)
newtrain <- as.data.frame(newtrain)

newtrain2 <- train %>%
  select(isMobile
         , deviceCategory
         , country)
newtrain2 <- model.matrix(~., data = newtrain2)
newtrain2 <- as.data.frame(newtrain2)

names(train)

newtrain3 <- train %>%
  select(region
         , campaign)
newtrain3 <- model.matrix(~., data = newtrain3)
newtrain3 <- as.data.frame(newtrain3)

train <- cbind(train,newtrain[,-1], newtrain2[,-1], newtrain3[,-1])

rm(newtrain,newtrain2,newtrain3)

#Scale data

maxs <- apply(train,2,max)
mins <- apply(train,2,min)
scaled.data <- as.data.frame(scale(train,center=mins,scale=maxs-mins))

trainIndex <- createDataPartition(scaled.data$transactions, p = 0.75, list = FALSE)

train_set <- scaled.data[trainIndex,]
validation_set <- scaled.data[-trainIndex,]

features <- colnames(scaled.data[,2:length(scaled.data)])
f <- paste(features, collapse = ' + ')
f <- paste("transactions ~", features)
set.seed(911)

my_neuralnetwork <- neuralnet(f,data=train_set[1:10000,],hidden=c(500,300,100),linear.output=T)

pr.nn <- compute(nn, validation_set[2:ncol(validation_set)])
pr.nn2 <- pr.nn$net.result*(max(nndata$attempts_range)-min(nndata$attempts_range))+min(nndata$attempts_range)
pr.nn2 <- round(pr.nn2)
test.r <- (validation_set$attempts_range)*(max(nndata$attempts_range)-min(nndata$attempts_range))+min(nndata$attempts_range)
MAE.nn <- mean(abs(test.r - pr.nn2))

print(MAE.nn)

#### Final Test Predictions ####

# Clean Test Data for Model Predictions
