
####Load Data####
library(tidyverse)
library(caret)
library(neuralnet)

setwd("/home/jacob_w_lyman/home")

train <- read_csv("my_train.csv") %>%
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

train$isTrueDirect[is.na(train$isTrueDirect)] <- 'False'
train <- train %>%
  mutate(isTrueDirect = as.logical(isTrueDirect)
         , isTrueDirect = as.numeric(isTrueDirect)
         , isMobile = as.numeric(isMobile))

# Remove NAs
train$transactionRevenue[is.na(train$transactionRevenue)] <- 0
train$transactions[is.na(train$transactions)] <- 0
train$totalTransactionRevenue[is.na(train$totalTransactionRevenue)] <- 0

# NOTE: These next few lines are replacing NAs with 0 but should treated differently
train$timeOnSite[is.na(train$timeOnSite)] <- 0
train$newVisits[is.na(train$newVisits)] <- 0
train$bounces[is.na(train$bounces)] <- 0
train$pageviews[is.na(train$pageviews)] <- 0
train$sessionQualityDim[is.na(train$sessionQualityDim)] <- 0

#### Feature Engineering ####

# Scale continuous columns
train_scaled <- train %>%
  select(hits1
         , pageviews
         , bounces
         , newVisits
         , sessionQualityDim
         , timeOnSite
         , transactions
         , transactionRevenue)

maxs <- apply(train_scaled,2,max)
mins <- apply(train_scaled,2,min)
scaled_data <- as.data.frame(scale(train_scaled,center=mins,scale=maxs-mins))

rm(train_scaled, maxs, mins)

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

train <- cbind(newtrain[,-1], newtrain2[,-1], newtrain3[,-1])

rm(newtrain,newtrain2,newtrain3)

# FinalScaled Data Set
train <- cbind(scaled_data,train)
rm(scaled_data)

# Split Train Set
# names(train) <- gsub(x = names(train),
#                          pattern = " ",
#                          replacement = "")
# names(train_set) <- gsub(x = names(train_set),
#                      pattern = ";",
#                      replacement = "")
# names(train_set) <- gsub(x = names(train_set),
#                          pattern = ":",
#                          replacement = "")
# names(train_set) <- gsub(x = names(train_set),
#                          pattern = "-",
#                          replacement = "")
# names(train_set) <- gsub(x = names(train_set),
#                          pattern = "]",
#                          replacement = "")
# names(train_set) <- gsub(x = names(train_set),
#                          pattern = "/",
#                          replacement = "")
# names(train_set) <- gsub(x = names(train_set),
#                          pattern = "&",
#                          replacement = "")
train <- train %>%
  select(transactions, everything())
train <- train[,1:200]
new_names <- 2:200
new_names <- as.character(new_names)
colnames(train)[2:200] <- new_names


# train_set <- train_set %>%
#   rename(newName = 'browser[UsedefaultUseragentstringLIVRENPOCHE')
# train_set <- train_set %>%
#   rename(newName2 = 'countryCôted’Ivoire')
# train_set <- train_set %>%
#   rename(newName3 = 'campaignValueShoppersAffinity')
# train_set <- train_set %>%
#   rename(newName4 = 'campaignTechnologyTechnophiles')

trainIndex <- caret::createDataPartition(train$transactions, p = 0.75, list = FALSE)
train_set <- train[trainIndex,]
validation_set <- train[-trainIndex,]

rm(train,trainIndex)

####Neural Network #####
features <- colnames(train_set[,2:length(train_set)])
f <- paste(features, collapse = ' + ')
f <- paste('transactions ~', f)
set.seed(911)

n <- Sys.time()
my_neuralnetwork <- neuralnet(f,data=train_set[1:10000,],hidden=c(500,300,100),linear.output=T)
model_processing <- Sys.time() - n
model_processing

pr.nn <- compute(nn, validation_set[2:ncol(validation_set)])
pr.nn2 <- pr.nn$net.result*(max(nndata$attempts_range)-min(nndata$attempts_range))+min(nndata$attempts_range)
pr.nn2 <- round(pr.nn2)
test.r <- (validation_set$attempts_range)*(max(nndata$attempts_range)-min(nndata$attempts_range))+min(nndata$attempts_range)
MAE.nn <- mean(abs(test.r - pr.nn2))

print(MAE.nn)

# OLS model

my_OLS <- lm(transactions ~ .,data = train_set)

#### Final Test Predictions ####

test <- read_csv("my_test.csv") %>%
  select(-X1)

# Clean Test Data for Model Predictions
