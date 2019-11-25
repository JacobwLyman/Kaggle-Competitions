library(data.table)
library(dplyr)
library(leaps)
library(readr)
library(Matrix)
library(lattice)
library(e1071)
library(RSNNS)

data <- fread("../input/train.csv")
testData <- fread("../input/test.csv")

##Scale data

data <- data/255
data$label <- data$label * 255
testData <- testData/255

##Convert all pixels to numeric

data <- mutate_all(data, function(x) as.numeric(as.character(x)))

nndata <- data

##Revert label back to a factor
data$label <- as.factor(data$label)

#Trim out the pixels with no features

data$label <- as.numeric(data$label)

nonzerofeature <- sapply(X = data, FUN = max) != 0
sum(nonzerofeature)
length(nonzerofeature)

data <- data[,nonzerofeature]

               ####Train Models####

data$label <- as.factor(data$label)
trainIndex <- caret::createDataPartition(data$label, p = 0.75, list = FALSE)
train <- data[trainIndex,]
test <- data[-trainIndex,]

#Convolutional Neural Network####

targetlabel <- decodeClassLabels(nndata[,1])
trainSet <- splitForTrainingAndTest(nndata[,2:ncol(nndata)], targetlabel)

trainSet <- normTrainingAndTestSet(trainSet, type= "0_1")

nn1 <- mlp(x = trainSet$inputsTrain, y = trainSet$targetsTrain, size = c(700, 300, 200), maxit = 8, inputsTest = trainSet$inputsTest, targetsTest = trainSet$targetsTest)

####Testing####

#Prep test data

testData <- mutate_all(testData, function(x) as.numeric(as.character(x)))

#Convolutional Neural Network

test.nnPred <- predict(nn1, newdata = testData)

nn.out <- as.data.frame(test.nnPred)

x <- max.col(nn.out)

x <- data.frame(x)

x <- x - 1

nn.submission <- data.frame("ImageId"=c(1:28000), "Label"=x)
names(nn.submission)[2]<-"Label"

####Submission####
submission <- nn.submission
write.csv(submission, "submission.csv", row.names = FALSE)
