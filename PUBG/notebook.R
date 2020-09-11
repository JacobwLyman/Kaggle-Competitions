# %% [code] {"_execution_state":"idle"}
## Importing packages

# This R environment comes with all of CRAN and many other helpful packages preinstalled.
# You can see which packages are installed by checking out the kaggle/rstats docker image: 
# https://github.com/kaggle/docker-rstats

library(tidyverse)
library(caret)
library(kableExtra)
library(Metrics)
library(e1071)
library(ranger)
library(gbm)

# %% [code]
train <- read_csv("../input/pubg-finish-placement-prediction/test_V2.csv")
test <- read_csv("../input/pubg-finish-placement-prediction/test_V2.csv")

# %% [markdown]
# # EDA

# %% [code]
str(train)
str(test)

# %% [code]
dim(train)

# %% [code]
train %>%
  head(10)

#train %>%
 # head(100) %>%
  #kable(align = "c") %>%
  #kable_styling(bootstrap_options = c("striped", "hover"),full_width = F) %>%
  #kable_styling()

# %% [code]
train %>%
    ggplot(aes(x=matchType)) +
    geom_bar() +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, size = 12, hjust = 1))

# %% [code]
train  %>%
  group_by(maxPlace) %>%
  summarize(median_view_time_perc = median(kills)
            ,mean_view_time_perc = mean(kills)) %>%
  ggplot() +
  geom_point(aes(x = maxPlace, y = median_view_time_perc)) +
  geom_point(aes(x = maxPlace, y = mean_view_time_perc), shape = 1) +
  theme_bw()

# %% [code]


# %% [code]


# %% [code]


# %% [markdown]
# # Clean Data

# %% [code]


# %% [markdown]
# First we are going to have to split the data into the different match styles

# %% [code]
####Clean Data####

# Solo matches
solo_df <- train %>% 
    filter(matchType == c('solo'
                          ,'solo-fpp'
                          ,'normal-solo'
                          ,'normal-solo-fpp')) %>%
    dplyr::select(-c('Id'
               , 'groupId'
               , 'assists'
               , 'matchType'
               , 'numGroups'
               , 'revives'
               , 'teamKills'))

solo_df <- solo_df %>% 
    group_by(matchId) %>%
    mutate(rank_kills = rank(kills)
         , rank_walkDistance = rank(walkDistance)
         , rank_weaponsAcquired=rank(weaponsAcquired)
         , rank_headshotKills = rank(headshotKills)
         , rank_boosts= rank(boosts)
         , rank_heals=rank(heals)
         , rank_damageDealt = rank(damageDealt)
         , rank_DBNOs = rank(DBNOs))
    
set.seed(911)

solo.matches <- as.character(unique(solo_df$matchId))
x <- sample(solo.matches, 200, replace=FALSE)
solo.sample <- solo_df[as.character(solo_df$matchId) %in% x,]

inTrain <- createDataPartition(y=solo.sample$winPlacePerc,p=.75,list=FALSE)
solo.train <- solo.sample[inTrain,]
solo.test <- solo.sample[-inTrain,]

# Duo matches ####
duo_df <- train %>%
    filter(matchType == c('duo'
                          ,'duo-fpp'
                          ,'normal-duo'
                          ,'normal-duo-fpp'
                          ,'crashfpp'
                          ,'crashtpp')) %>%
    dplyr::select(-c('matchType'
                , 'Id'
                , 'teamKills'
                , 'killPoints'
                , 'rankPoints'
                , 'winPoints'))

duo_df <- duo_df %>%
  group_by(matchId, groupId) %>%
  summarise(assists = sum(assists)
            , boosts=sum(boosts)
            , damageDealt = sum(damageDealt)
            , DBNOs = sum(DBNOs)
            , headshotKills = sum(headshotKills)
            , heals = sum(heals)
            , killPlace = sum(killPlace)
            , kills = sum(kills)
            , killStreaks = sum(killStreaks)
            , longestKill = sum(longestKill)
            , revives = sum(revives)
            , rideDistance = sum(rideDistance)
            , roadKills = sum(roadKills)
            , swimDistance = sum(swimDistance)
            , vehicleDestroys = sum(vehicleDestroys)
            , walkDistance = sum(walkDistance)
            , weaponsAcquired = sum(weaponsAcquired)
            , matchDuration = max(matchDuration)
            , maxPlace = max(maxPlace)
            , numGroups = max(numGroups)
            , winPlacePerc = max(winPlacePerc))

duo_df <- duo_df %>% 
  group_by(matchId) %>%
  mutate(rank_kills = rank(kills)
         , rank_walkDistance= rank(walkDistance)
         , rank_assists= rank(assists)
         , rank_boosts=rank(boosts)
         , rank_weaponsAcquired=rank(weaponsAcquired)
         , rank_headshotKills = rank(headshotKills)
         , rank_DBNOs = rank(DBNOs)
         , rank_heals=rank(heals)
         , rank_damageDealt = rank(damageDealt))

duo.matches <- as.character(unique(duo_df$matchId))
x.duo <- sample(duo.matches, 50, replace=FALSE)
duo.sample <- duo_df[as.character(duo_df$matchId) %in% x.duo,]

inTrain2 <- createDataPartition(y=duo.sample$winPlacePerc,p=.75,list=FALSE)
duo.train <- duo.sample[inTrain2,]
duo.test <- duo.sample[-inTrain2,]

# Squad matches ####
squad_df <- train %>%
    filter(matchType == c('squad'
                          ,'squad-fpp'
                          ,'normal-squad'
                          ,'normal-squad-fpp'
                          ,'flarefpp'
                          ,'laretpp')) %>%
    dplyr::select(-c('matchType'
                , 'Id'
                , 'teamKills'
                , 'killPoints'
                , 'rankPoints'
                , 'winPoints'))

squad_df <- squad_df %>%
  group_by(matchId, groupId) %>%
  summarise(assists = sum(assists)
            , boosts=sum(boosts)
            , damageDealt = sum(damageDealt)
            , DBNOs = sum(DBNOs)
            , headshotKills = sum(headshotKills)
            , heals = sum(heals)
            , killPlace = sum(killPlace)
            , kills = sum(kills)
            , killStreaks = sum(killStreaks)
            , longestKill = sum(longestKill)
            , revives = sum(revives)
            , rideDistance = sum(rideDistance)
            , roadKills = sum(roadKills)
            , swimDistance = sum(swimDistance)
            , vehicleDestroys = sum(vehicleDestroys)
            , walkDistance = sum(walkDistance)
            , weaponsAcquired = sum(weaponsAcquired)
            , matchDuration = max(matchDuration)
            , maxPlace = max(maxPlace)
            , numGroups = max(numGroups)
            , winPlacePerc = max(winPlacePerc))

squad_df <- squad_df %>% 
  group_by(matchId) %>%
  mutate(rank_kills = rank(kills)
         , rank_walkDistance= rank(walkDistance)
         , rank_assists= rank(assists)
         , rank_boosts=rank(boosts)
         , rank_weaponsAcquired=rank(weaponsAcquired)
         , rank_headshotKills = rank(headshotKills)
         , rank_DBNOs = rank(DBNOs)
         , rank_heals=rank(heals)
         , rank_damageDealt = rank(damageDealt))

squad.matches <- as.character(unique(squad_df$matchId))
x.squad <- sample(squad.matches, 50, replace=FALSE)
squad.sample <- squad_df[as.character(squad_df$matchId) %in% x.squad,]

inTrain3 <- createDataPartition(y=squad.sample$winPlacePerc,p=.75,list=FALSE)
squad.train <- squad.sample[inTrain3,]
squad.test <- squad.sample[-inTrain3,]

# %% [markdown]
# # Modeling

# %% [code]
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

# %% [code]
####Testing####
solo_test <- test %>% 
    filter(matchType == c('solo','solo-fpp','normal-solo','normal-solo-fpp'))
duo_test <- test %>%
    filter(matchType == c('duo','duo-fpp','normal-duo','normal-duo-fpp','crashfpp','crashtpp'))
squad_test <- test %>%
    filter(matchType == c('squad','squad-fpp','normal-squad','normal-squad-fpp','flarefpp','laretpp'))

solo_test <- solo_test %>%
    select(-one_of(drop_cols))
duo_test <- duo_test %>%
    select(-one_of(drop_cols2))
squad_test <- squad_test %>%
    select(-one_of(drop_col2))

##Test.Solo Dataframes
solo_test <- solo_test %>% 
  group_by(matchId) %>%
  mutate(rank_kills = rank(kills)
         , rank_walkDistance= rank(walkDistance)
         , rank_weaponsAcquired=rank(weaponsAcquired)
         , rank_headshotKills = rank(headshotKills)
         , rank_boosts= rank(boosts)
         , rank_heals=rank(heals)
         , rank_damageDealt = rank(damageDealt)
         , rank_DBNOs = rank(DBNOs))

##Test.Duo Dataframes
duo_test <- duo_test %>%
  group_by(matchId, groupId) %>%
  summarise(assists = sum(assists)
            , boosts=sum(boosts)
            , damageDealt = sum(damageDealt)
            , DBNOs = sum(DBNOs)
            , headshotKills = sum(headshotKills)
            , heals = sum(heals)
            , killPlace = sum(killPlace)
            , kills = sum(kills)
            , killStreaks = sum(killStreaks)
            , longestKill = sum(longestKill)
            , revives = sum(revives)
            , rideDistance = sum(rideDistance)
            , roadKills = sum(roadKills)
            , swimDistance = sum(swimDistance)
            , vehicleDestroys = sum(vehicleDestroys)
            , walkDistance = sum(walkDistance)
            , weaponsAcquired = sum(weaponsAcquired)
            , matchDuration = max(matchDuration)
            , maxPlace = max(maxPlace)
            , numGroups = max(numGroups))

duo_test <- duo_test %>% 
  group_by(matchId) %>%
  mutate(rank_kills = rank(kills)
         , rank_walkDistance= rank(walkDistance)
         , rank_assists= rank(assists)
         , rank_boosts=rank(boosts)
         , rank_weaponsAcquired=rank(weaponsAcquired)
         , rank_headshotKills = rank(headshotKills)
         , rank_DBNOs = rank(DBNOs)
         , rank_heals=rank(heals)
         , rank_damageDealt = rank(damageDealt))

##Test.Squad Dataframes
squad_test <- squad_test %>%
  group_by(matchId, groupId) %>%
  summarise(assists = sum(assists)
            , boosts=sum(boosts)
            , damageDealt = sum(damageDealt)
            , DBNOs = sum(DBNOs)
            , headshotKills = sum(headshotKills)
            , heals = sum(heals)
            , killPlace = sum(killPlace)
            , kills = sum(kills)
            , killStreaks = sum(killStreaks)
            , longestKill = sum(longestKill)
            , revives = sum(revives)
            , rideDistance = sum(rideDistance)
            , roadKills = sum(roadKills)
            , swimDistance = sum(swimDistance)
            , vehicleDestroys = sum(vehicleDestroys)
            , walkDistance = sum(walkDistance)
            , weaponsAcquired = sum(weaponsAcquired)
            , matchDuration = max(matchDuration)
            , maxPlace = max(maxPlace)
            , numGroups = max(numGroups))

squad_test <- squad_test %>% 
  group_by(matchId) %>%
  mutate(rank_kills = rank(kills)
         , rank_walkDistance= rank(walkDistance)
         , rank_assists= rank(assists)
         , rank_boosts=rank(boosts)
         , rank_weaponsAcquired=rank(weaponsAcquired)
         , rank_headshotKills = rank(headshotKills)
         , rank_DBNOs = rank(DBNOs)
         , rank_heals=rank(heals)
         , rank_damageDealt = rank(damageDealt))

# %% [code]
##Final Predictions!!!##

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
