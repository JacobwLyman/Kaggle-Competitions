                                              ##Kaggle - PUBG##

####Import Data####
setwd("C:/Users/Jacob/Pictures/Camera Roll/HTO55/MOJE/Data Science/PUBG")
#setwd("E:/Data Analytics II/PUBG")

#install.packages("dplyr")
library(dplyr)
#install.packages("caret")
library(caret)
#install.packages("randomForest")
library(randomForest)
#install.packages("caret")
library(caret)
#install.packages("ggplot2")
library(ggplot2)
#install.packages("csv")
library(csv)

#train <- read.csv("train.csv")

#solo <- train[train$matchType=="solo" |train$matchType=="solo-fpp" | 
#                       train$matchType=="normal-solo" |train$matchType=="normal-solo-fpp",]
#duo <- train[train$matchType=="duo" |train$matchType=="duo-fpp" | 
#                      train$matchType=="normal-duo" |train$matchType=="normal-duo-fpp" 
#                    |train$matchType=="crashfpp" |train$matchType=="crashtpp",]
#squad <- train[train$matchType=="squad" |train$matchType=="squad-fpp" | 
#                        train$matchType=="normal-squad" |train$matchType=="normal-squad-fpp" 
#                      |train$matchType=="flarefpp" | train$matchType=="flaretpp",]

#write.csv(x=solo,file= "solo.csv")
#write.csv(x=duo, file="duo.csv")
#write.csv(x=squad,file= "squad.csv")

solo <- read.csv("solo.csv")
duo <- read.csv("duo.csv")
squad <- read.csv("squad.csv")

####Clean Data####

solo <- subset(solo, select= -c(X, Id, groupId, assists, DBNOs, matchType, numGroups, revives, teamKills))
duo <- subset(duo, select= -c(X, matchType, Id, teamKills, killPoints, rankPoints, winPoints))
squad <- subset(squad, select= -c(X, matchType, Id, teamKills, killPoints, rankPoints, winPoints))

##Solo Dataframes

    solo <- solo %>% 
      group_by(matchId) %>%
      mutate(rank_kills = rank(kills), rank_walkDistance= rank(walkDistance), rank_weaponsAcquired=rank(weaponsAcquired),
             rank_headshotKills = rank(headshotKills), rank_boosts= rank(boosts), rank_heals=rank(heals),
             rank_damageDealt = rank(damageDealt), rank_DBNOs = rank(DBNOs))
    
    set.seed(911)
    
    solo.matches <- as.character(unique(solo$matchId))
    x <- sample(solo.matches, 200, replace=FALSE)
    solo.sample <- solo[as.character(solo$matchId) %in% x,]
    
    inTrain <- createDataPartition(y=solo.sample$winPlacePerc,p=.75,list=FALSE)
    solo.train <- solo.sample[inTrain,]
    solo.test <- solo.sample[-inTrain,]
    
    #Don't forget to make a column for walkDistance^2 and to include it in my model

##Duo Dataframes

    duo2 <- duo %>%
      group_by(matchId, groupId) %>%
      summarise(assists = sum(assists), boosts=sum(boosts), damageDealt = sum(damageDealt),
                DBNOs = sum(DBNOs), headshotKills = sum(headshotKills), heals = sum(heals),
                killPlace = sum(killPlace), kills = sum(kills), killStreaks = sum(killStreaks),
                longestKill = sum(longestKill), revives = sum(revives), rideDistance = sum(rideDistance), 
                roadKills = sum(roadKills), swimDistance = sum(swimDistance), vehicleDestroys = sum(vehicleDestroys), 
                walkDistance = sum(walkDistance), weaponsAcquired = sum(weaponsAcquired), 
                matchDuration = max(matchDuration), maxPlace = max(maxPlace), numGroups = max(numGroups), 
                winPlacePerc = max(winPlacePerc))
    
    duo2 <- duo2 %>% 
      group_by(matchId) %>%
      mutate(rank_kills = rank(kills), rank_walkDistance= rank(walkDistance), rank_assists= rank(assists),
             rank_boosts=rank(boosts), rank_weaponsAcquired=rank(weaponsAcquired), rank_headshotKills = rank(headshotKills), 
             rank_DBNOs = rank(DBNOs), rank_heals=rank(heals), rank_damageDealt = rank(damageDealt))
    
    duo.matches <- as.character(unique(duo2$matchId))
    x.duo <- sample(duo.matches, 50, replace=FALSE)
    duo.sample <- duo2[as.character(duo2$matchId) %in% x.duo,]
    
    inTrain2 <- createDataPartition(y=duo.sample$winPlacePerc,p=.75,list=FALSE)
    duo.train <- duo.sample[inTrain2,]
    duo.test <- duo.sample[-inTrain2,]

##Squad Dataframes

    squad2 <- squad %>%
      group_by(matchId, groupId) %>%
      summarise(assists = sum(assists), boosts=sum(boosts), damageDealt = sum(damageDealt),
                DBNOs = sum(DBNOs), headshotKills = sum(headshotKills), heals = sum(heals),
                killPlace = sum(killPlace), kills = sum(kills), killStreaks = sum(killStreaks),
                longestKill = sum(longestKill), revives = sum(revives), rideDistance = sum(rideDistance), 
                roadKills = sum(roadKills), swimDistance = sum(swimDistance), vehicleDestroys = sum(vehicleDestroys), 
                walkDistance = sum(walkDistance), weaponsAcquired = sum(weaponsAcquired), 
                matchDuration = max(matchDuration), maxPlace = max(maxPlace), numGroups = max(numGroups), 
                winPlacePerc = max(winPlacePerc))
    
    squad2 <- squad2 %>% 
      group_by(matchId) %>%
      mutate(rank_kills = rank(kills), rank_walkDistance= rank(walkDistance), rank_assists= rank(assists),
             rank_boosts=rank(boosts), rank_weaponsAcquired=rank(weaponsAcquired), rank_headshotKills = rank(headshotKills), 
             rank_DBNOs = rank(DBNOs), rank_heals=rank(heals), rank_damageDealt = rank(damageDealt))
    
    squad.matches <- as.character(unique(squad2$matchId))
    x.squad <- sample(squad.matches, 50, replace=FALSE)
    squad.sample <- squad2[as.character(squad2$matchId) %in% x.squad,]

    inTrain3 <- createDataPartition(y=squad.sample$winPlacePerc,p=.75,list=FALSE)
    squad.train <- squad.sample[inTrain3,]
    squad.test <- squad.sample[-inTrain3,]

####Models####
    
##Solo Models##
      
      model <- randomForest(winPlacePerc ~ .-matchId,
                             data = solo.train, mtry = 3, ntree = 500, importance = TRUE)
      
      model
      
      ##Predictions!!!##
      
      y<- solo.test[,"winPlacePerc"]
      
      rf.predictions <- predict(model,newdata=solo.test)
      
      temp <- as.vector(y)
      temp <- temp$winPlacePerc
      
      mean(abs(temp-rf.predictions))

##Duo Models##

      duo.model <- randomForest(winPlacePerc ~ .-matchId -groupId,
                            data = duo.train, mtry = 3, ntree = 500, importance = TRUE)
      
      duo.model
      
      ##Predictions!!!##
      
      y.duo<- duo.test[,"winPlacePerc"]
      
      rf.duo.predictions <- predict(duo.model,newdata=duo.test)
      
      temp.duo <- as.vector(y.duo)
      temp.duo <- temp.duo$winPlacePerc
      
      mean(abs(temp.duo-rf.duo.predictions))
      
#Model 2
#install.packages("gbm")
#library(gbm)
      
#boost.duo <- gbm(winPlacePerc~.,data=duo.train, distribution="gaussian",
#                           n.trees=1000,interaction.depth=4,shrinkage = .01)
#boost.duo

##Squad Models##
      
      squad.model <- randomForest(winPlacePerc ~ .-matchId -groupId,
                                data = squad.train, mtry = 3, ntree = 500, importance = TRUE)
      
      squad.model
      
      ##Predictions!!!##
      
      y.squad<- squad.test[,"winPlacePerc"]
      
      rf.squad.predictions <- predict(squad.model,newdata=squad.test)
      
      temp.squad <- as.vector(y.squad)
      temp.squad <- temp.squad$winPlacePerc
      
      mean(abs(temp.squad-rf.squad.predictions))

####Testing####

test <- read.csv("test.csv")
      
test.solo <- test[test$matchType=="solo" |test$matchType=="solo-fpp" | 
                       test$matchType=="normal-solo" |test$matchType=="normal-solo-fpp",]
test.duo <- test[test$matchType=="duo" |test$matchType=="duo-fpp" | 
                      test$matchType=="normal-duo" |test$matchType=="normal-duo-fpp" 
                     |test$matchType=="crashfpp" |test$matchType=="crashtpp",]
test.squad <- test[test$matchType=="squad" |test$matchType=="squad-fpp" | 
                       test$matchType=="normal-squad" |test$matchType=="normal-squad-fpp" 
                      |test$matchType=="flarefpp" | test$matchType=="flaretpp",]


test.solo2 <- subset(test.solo, select= -c(Id, groupId, assists, DBNOs, matchType, numGroups, revives, teamKills))
test.duo2 <- subset(test.duo, select= -c(matchType, Id, teamKills, killPoints, rankPoints, winPoints))
test.squad2 <- subset(test.squad, select= -c(matchType, Id, teamKills, killPoints, rankPoints, winPoints))

##Test.Solo Dataframes

test.solo2 <- test.solo2 %>% 
  group_by(matchId) %>%
  mutate(rank_kills = rank(kills), rank_walkDistance= rank(walkDistance), rank_weaponsAcquired=rank(weaponsAcquired),
         rank_headshotKills = rank(headshotKills), rank_boosts= rank(boosts), rank_heals=rank(heals),
         rank_damageDealt = rank(damageDealt), rank_DBNOs = rank(DBNOs))

##Test.Duo Dataframes

test.duo2 <- test.duo2 %>%
  group_by(matchId, groupId) %>%
  summarise(assists = sum(assists), boosts=sum(boosts), damageDealt = sum(damageDealt),
            DBNOs = sum(DBNOs), headshotKills = sum(headshotKills), heals = sum(heals),
            killPlace = sum(killPlace), kills = sum(kills), killStreaks = sum(killStreaks),
            longestKill = sum(longestKill), revives = sum(revives), rideDistance = sum(rideDistance), 
            roadKills = sum(roadKills), swimDistance = sum(swimDistance), vehicleDestroys = sum(vehicleDestroys), 
            walkDistance = sum(walkDistance), weaponsAcquired = sum(weaponsAcquired), 
            matchDuration = max(matchDuration), maxPlace = max(maxPlace), numGroups = max(numGroups))

test.duo2 <- test.duo2 %>% 
  group_by(matchId) %>%
  mutate(rank_kills = rank(kills), rank_walkDistance= rank(walkDistance), rank_assists= rank(assists),
         rank_boosts=rank(boosts), rank_weaponsAcquired=rank(weaponsAcquired), rank_headshotKills = rank(headshotKills), 
         rank_DBNOs = rank(DBNOs), rank_heals=rank(heals), rank_damageDealt = rank(damageDealt))

##Test.Squad Dataframes

test.squad2 <- test.squad2 %>%
  group_by(matchId, groupId) %>%
  summarise(assists = sum(assists), boosts=sum(boosts), damageDealt = sum(damageDealt),
            DBNOs = sum(DBNOs), headshotKills = sum(headshotKills), heals = sum(heals),
            killPlace = sum(killPlace), kills = sum(kills), killStreaks = sum(killStreaks),
            longestKill = sum(longestKill), revives = sum(revives), rideDistance = sum(rideDistance), 
            roadKills = sum(roadKills), swimDistance = sum(swimDistance), vehicleDestroys = sum(vehicleDestroys), 
            walkDistance = sum(walkDistance), weaponsAcquired = sum(weaponsAcquired), 
            matchDuration = max(matchDuration), maxPlace = max(maxPlace), numGroups = max(numGroups))

test.squad2 <- test.squad2 %>% 
  group_by(matchId) %>%
  mutate(rank_kills = rank(kills), rank_walkDistance= rank(walkDistance), rank_assists= rank(assists),
         rank_boosts=rank(boosts), rank_weaponsAcquired=rank(weaponsAcquired), rank_headshotKills = rank(headshotKills), 
         rank_DBNOs = rank(DBNOs), rank_heals=rank(heals), rank_damageDealt = rank(damageDealt))

##Predictions!!!##

##Solo##
test.solo.predictions <- predict(model,newdata=test.solo2)
test.solo.predictions <- data.frame("Id" = test.solo$Id, "winPlacePerc" = test.solo.predictions)

##Duo##
test.duo.predictions <- predict(duo.model,newdata=test.duo2)

final.duo <- data.frame("groupId" = test.duo2$groupId, "winPlacePerc" = test.duo.predictions)

test.duo <- subset(test.duo, select= -c(matchId, assists, boosts, damageDealt, DBNOs,
                                        headshotKills, heals, killPlace, killPoints, kills, killStreaks,
                                        longestKill, matchDuration, matchType, maxPlace, numGroups, rankPoints,
                                        revives, rideDistance, roadKills, swimDistance, teamKills, swimDistance, teamKills,
                                        vehicleDestroys, walkDistance, weaponsAcquired, winPoints))

duo.temp <- merge(test.duo, final.duo, by = "groupId")

test.duo.predictions <- subset(duo.temp, select= -groupId)

##Squad##

test.squad.predictions <- predict(squad.model,newdata=test.squad2)

final.squad <- data.frame("groupId" = test.squad2$groupId, "winPlacePerc" = test.squad.predictions)

test.squad <- subset(test.squad, select= -c(matchId, assists, boosts, damageDealt, DBNOs,
                                        headshotKills, heals, killPlace, killPoints, kills, killStreaks,
                                        longestKill, matchDuration, matchType, maxPlace, numGroups, rankPoints,
                                        revives, rideDistance, roadKills, swimDistance, teamKills, swimDistance, teamKills,
                                        vehicleDestroys, walkDistance, weaponsAcquired, winPoints))

squad.temp <- merge(test.squad, final.squad, by = "groupId")

test.squad.predictions <- subset(squad.temp, select= -groupId)

####Submission####
submission <- rbind(test.solo.predictions, test.duo.predictions, test.squad.predictions)
write.csv(submission, "submission.csv", row.names = FALSE)
