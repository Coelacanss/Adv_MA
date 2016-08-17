### This script is for Adv Marketing Analytics assignment #3
### MSMA  Xinyuan Zhang

## load data
data.train <- read.csv(file = 'HW3 - Training Data.csv')
data.test <- read.csv(file = 'HW3 - Testing Data For Students.csv')

## data explorary
library(caret)
nearZeroVar(data.train, saveMetrics = T)
# calculate % of helpful ratings
data.train$helpfulPersent = data.train$number.of.helpfulness / data.train$number.of.votes
# deal with NAs
data.train$helpfulPersent[which(is.na(data.train$helpfulPersent))] = 0
# drop used variables since they are not in testing data
data.train$number.of.votes = NULL
data.train$number.of.helpfulness = NULL
# X.1 and X are all unique observations, so meaningless for modeling
data.train$X.1 = NULL
data.train$X = NULL


####################### Feature Engineering ###########################
##
library(stringi)
library(stringr)

## modify the data type 
data.train$post.date = as.factor(data.train$post.date)
data.train$post.month = as.factor(data.train$post.month)
data.train$post.year = as.factor(data.train$post.year)
data.train$product.index = as.factor(data.train$product.index)
data.train$review.index = as.factor(data.train$review.index)

## extract length of reviews (how many words)
reviewLength = c()
for (i in 1:length(data.train$review.text)) {
      review.current = data.train$review.text[i]
      reviewLength[i] = length(unlist(str_extract_all(review.current, ' '))) + 1
      print(i) # for tracking
}
data.train$reviewLength = reviewLength

## drop textual data
data.train$product.title = NULL
data.train$review.text = NULL
data.train$review.title = NULL

## do the same work on testing data to prepare for predicting
## modify the data type 
data.test$post.date = as.factor(data.test$post.date)
data.test$post.month = as.factor(data.test$post.month)
data.test$post.year = as.factor(data.test$post.year)
data.test$product.index = as.factor(data.test$product.index)
data.test$review.index = as.factor(data.test$review.index)

## extract length of reviews (how many words)
reviewLength = c()
for (i in 1:length(data.test$review.text)) {
      review.current = data.test$review.text[i]
      reviewLength[i] = length(unlist(str_extract_all(review.current, ' '))) + 1
      print(i) # for tracking
}
data.test$reviewLength = reviewLength

## drop textual data
data.test$product.title = NULL
data.test$review.text = NULL
data.test$review.title = NULL

## drop meaningless data
data.test$X.1 = NULL
data.test$X = NULL


####################### Model Fitting Part 1 ###########################
##
## load package for calculating mse
library(Metrics)

## rpart
library(rpart)
model.rpart = rpart(helpfulPersent~.,data=data.train,method='anova')
cartVec = predict(model.rpart, newdata = data.test)

## MARS
library(earth)
model.MARS = earth(helpfulPersent~.,data=data.train)
marsVec = predict(model.MARS, newdata = data.test)

## Bagged Trees
library(ipred)
model.bagged = bagging(helpfulPersent~.,data=data.train)
baggedVec = predict(model.bagged, newdata = data.test)

## Random Forest
library(randomForest)
trainingRF = data.train[,-5]
trainingRF$product.index = as.numeric(data.train$product.index)
model.rf = randomForest(helpfulPersent~., data=trainingRF)
forestVec = predict(model.rf, newdata = data.test)

## SVM
library(e1071)
model.svm = svm(helpfulPersent~., data = data.train)
svmVec = predict(model.svm, newdata = data.test)

## boosting
library(gbm)
model.boost = gbm(helpfulPersent~., data = data.train)
boostVec = predict(model.boost, newdata = data.test)

## Neural Networks
library(nnet)
model.nnet = nnet(helpfulPersent~., data = data.train, linout = 1, size = 5)
nnetVec = predict(model.nnet, newdata = data.test)

## Knn-regression
library('FNN')
library(caret)
model.knn = knnreg(cbind(data.train[,1:13], data.train[,15]), y = data.train[,14], k = 5)
knnVec = predict.knnreg(model.knn, newdata = data.test)


####################### Model Comparison Part 2 ###########################
##
## create training sample and testing sample
inSample = sample(1:nrow(data.train), 0.8*nrow(data.train))
training = data.train[inSample,]
testing = data.train[-inSample,]

## load package for calculating mse
library(Metrics)

## Refitting: for calculate time used, system.time would run regression again, so I think
## it's not a good choice. Instead, I used proc.time
## rpart
time.now = proc.time()
model.rpart2 = rpart(helpfulPersent~.,data=training,method='anova')
time.rpart = (proc.time() - time.now)[3]
cartVec2 = predict(model.rpart2, newdata = testing)
mse.rpart = mse(cartVec2, testing$helpfulPersent)
mseIn.rpart = mse(predict(model.rpart2), training$helpfulPersent)

## MARS
time.now = proc.time()
model.MARS2 = earth(helpfulPersent~.,data=training)
time.MARS = (proc.time() - time.now)[3]
marsVec2 = predict(model.MARS2, newdata = testing)
mse.MARS = mse(marsVec2, testing$helpfulPersent)
mseIn.MARS = mse(predict(model.MARS2), training$helpfulPersent)

## Bagged Trees
time.now = proc.time()
model.bagged2 = bagging(helpfulPersent~.,data=training)
time.bagged = (proc.time() - time.now)[3]
baggedVec2 = predict(model.bagged2, newdata = testing)
mse.bagged = mse(baggedVec2, testing$helpfulPersent)
mseIn.bagged = mse(predict(model.bagged2), training$helpfulPersent)

## Random Forest
trainingRF = training[,-5]
trainingRF$product.index = as.numeric(training$product.index)
time.now = proc.time()
model.rf2 = randomForest(helpfulPersent~., data=trainingRF)
time.rf = (proc.time() - time.now)[3]
testingRF = testing[,-5]
testingRF$product.index = as.numeric(testing$product.index)
forestVec2 = predict(model.rf2, newdata = testingRF)
mse.rf = mse(forestVec2, testing$helpfulPersent)
mseIn.rf = mse(predict(model.rf2), training$helpfulPersent)

## SVM
time.now = proc.time()
model.svm2 = svm(helpfulPersent~., data = training)
time.svm = (proc.time() - time.now)[3]
svmVec2 = predict(model.svm2, newdata = testing)
mse.svm = mse(svmVec2, testing$helpfulPersent)
mseIn.svm = mse(predict(model.svm2), training$helpfulPersent)

## boosting
time.now = proc.time()
model.boost2 = gbm(helpfulPersent~., data = training)
time.boost = (proc.time() - time.now)[3]
boostVec2 = predict(model.boost2, newdata = testing, n.trees = 100)
mse.boost = mse(boostVec2, testing$helpfulPersent)
mseIn.boost = mse(predict(model.boost2, n.trees = 100), training$helpfulPersent)

## Neural Networks
time.now = proc.time()
model.nnet2 = nnet(helpfulPersent~., data = training[,-5], linout = 1, size = 5)
time.nnet = (proc.time() - time.now)[3]
nnetVec2 = predict(model.nnet2, newdata = testing)
mse.nnet = mse(nnetVec2, testing$helpfulPersent)
mseIn.nnet = mse(predict(model.nnet2), training$helpfulPersent)

## Knn-regression
time.now = proc.time()
model.knn2 = knnreg(cbind(training[,1:13], training[,15]), y = training[,14], k = 5)
time.knn = (proc.time() - time.now)[3]
knnVec2 = predict.knnreg(model.knn2, newdata = testing[,-14])
mse.knn = mse(knnVec2, testing$helpfulPersent)
mseIn.knn = mse(predict.knnreg(model.knn2, newdata = training[,-14]), training$helpfulPersent)

## conclude
mseList = c(mse.rpart, mse.MARS, mse.bagged, mse.rf, mse.svm, mse.boost, mse.nnet, mse.knn)
mseInList = c(mseIn.rpart, mseIn.MARS, mseIn.bagged, mseIn.rf, mseIn.svm, mseIn.boost, mseIn.nnet, mseIn.knn)
timeList = c(time.rpart, time.MARS, time.bagged, time.rf, time.svm, time.boost, time.nnet, time.knn)
conclusion = data.frame(MSE.outofsample = mseList, MSE.insample = mseInList, Time = timeList)
rownames(conclusion) <- c('rpart', 'MARS', 'bagged', 'RandomForest', 'SVM', 'boost', 'nnet', 'knn')
View(conclusion)

## save
save(conclusion, file = 'conclusion.Rda')
bestVec = baggedVec
save(bestVec, file = 'bestVec.Rda')
save.image(file = 'hw3.Rda')