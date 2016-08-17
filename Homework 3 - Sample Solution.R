setwd('D:/Dropbox/Teaching Lectures')

trainingData = read.csv('HW3 - Training Data.csv')
trainingData$y = trainingData$number.of.votes/trainingData$number.of.helpfulness
trainingData = subset(trainingData,is.finite(trainingData$y))

install.packages('tm')
install.packages('SnowballC')
install.packages('wordcloud')

library(tm)
library(SnowballC)
library(wordcloud)

reviewCorpus = Corpus(VectorSource(trainingData$review.text))
reviewCorpus = tm_map(reviewCorpus, removeWords, stopwords('english'))

dtm <- DocumentTermMatrix(reviewCorpus)   

freq <- colSums(as.matrix(dtm))   
freq = sort(freq,decreasing=TRUE)

nWordsToCheck = 1000
corList = rep(NA,nWordsToCheck)
names(corList) = names(freq)[1:nWordsToCheck]
for(i in 1:nWordsToCheck){
	isPresent = grepl(names(freq)[i],trainingData$review.text)
	corList[i] = cor(isPresent,trainingData$y)
}

nWordsToTake = 20 
posCorWords = sort(corList,decreasing=TRUE)[1:nWordsToTake]
negCorWords = sort(corList)[1:nWordsToTake]

sort(corList)[1:nWordsToTake]

#Get the top/bottom 20 into our dataset

for(i in 1:nWordsToTake){
	trainingData$newCol = as.numeric(grepl(names(posCorWords)[i],trainingData$review.text))
	names(trainingData)[ncol(trainingData)] = paste('is',names(posCorWords)[i],sep='.')	
}

for(i in 1:nWordsToTake){
	trainingData$newCol = as.numeric(grepl(names(negCorWords)[i],trainingData$review.text))
	names(trainingData)[ncol(trainingData)] = paste('is',names(negCorWords)[i],sep='.')	
}




initialPredCols = c('review.index','star.rating','top.100.reviewer','top.1000.reviewer','verified.purchase')
predCol = which(names(trainingData)=='y')
predictors = c(initialPredCols,names(trainingData)[(predCol+1):ncol(trainingData)])
formula1 = as.formula(paste('y~',paste(predictors,collapse='+'),sep=''))
formula2 = as.formula(paste('y~',paste(initialPredCols,collapse='+'),sep=''))

formulaSet = list(formula1,formula2)
##################################################
#Random K-Means Clustering 
##################################################
set.seed('200')

for(formNum in 1:length(formulaSet)){
	randomSamples = runif(nrow(trainingData))
	trainingData$kMeanNum = floor(randomSamples*5)+1


	#Linear Regression (For comparison)
	lmPerformance = rep(NA,5)
	for(i in 1:5){
		lmFIT = lm(formulaSet[[formNum]], data=trainingData,subset=trainingData$kMeanNum!=i)
		currentSubset = subset(trainingData,trainingData$kMeanNum==i)
		lmPerformance[i] = mean((predict(lmFIT,currentSubset) - currentSubset$y)^2)
	}

	#CART TREE 
	install.packages("rpart")
	library(rpart)
	cartPerformance = rep(NA,5)
	for(i in 1:5){
		cartFIT = rpart(formulaSet[[formNum]], data=trainingData, method="anova",subset=trainingData$kMeanNum!=i)
		currentSubset = subset(trainingData,trainingData$kMeanNum==i)
		cartPerformance[i] = mean((predict(cartFIT,currentSubset) - currentSubset$y)^2)
	}


	#MARS Rgression
	install.packages("earth")
	library(earth)
	marsPerformance = rep(NA,5)
	for(i in 1:5){
		marsFIT = earth(formula=formulaSet[[formNum]], data=trainingData,subset=trainingData$kMeanNum!=i)
		currentSubset = subset(trainingData,trainingData$kMeanNum==i)
		marsPerformance[i] = mean((predict(marsFIT,currentSubset) - currentSubset$y)^2)
	}

	#Bagging
	install.packages("ipred")
	library(ipred)
	baggPerformance = rep(NA,5)
	for(i in 1:5){
		baggFIT = bagging(formula=formulaSet[[formNum]], data=trainingData,subset=trainingData$kMeanNum!=i)
		currentSubset = subset(trainingData,trainingData$kMeanNum==i)
		baggPerformance[i] = mean((predict(baggFIT,currentSubset) - currentSubset$y)^2)
	}


	#Random Forests 
	install.packages("randomForest")
	library(randomForest)
	randPerformance = rep(NA,5)
	for(i in 1:5){
		randFIT = randomForest(formula=formulaSet[[formNum]], data=trainingData,subset=trainingData$kMeanNum!=i)
		currentSubset = subset(trainingData,trainingData$kMeanNum==i)
		randPerformance[i] = mean((predict(randFIT,currentSubset) - currentSubset$y)^2)
	}

	#Support Vector Machine 
	install.packages("e1071")
	library(e1071)
	svmPerformance = rep(NA,5)
	for(i in 1:5){
		svmFIT = svm(formula=formulaSet[[formNum]], data=trainingData,subset=trainingData$kMeanNum!=i)
		currentSubset = subset(trainingData,trainingData$kMeanNum==i)
		svmPerformance[i] = mean((predict(svmFIT,currentSubset) - currentSubset$y)^2)
	}

	#Boosting
	install.packages("gbm")
	library(gbm)
	boostPerformance = rep(NA,5)
	for(i in 1:5){
		thisTrain = subset(trainingData,trainingData$kMeanNum!=i)
		boostFIT = gbm(formula=formulaSet[[formNum]], data=thisTrain)
		currentSubset = subset(trainingData,trainingData$kMeanNum==i)
		boostPerformance[i] = mean((predict(boostFIT,currentSubset,n.trees=100) - currentSubset$y)^2)
	}

	#Neural Network 
	install.packages("nnet")
	library(nnet)
	nnetPerformance = rep(NA,5)
	for(i in 1:5){
		nnetFIT = nnet(formula=formulaSet[[formNum]], data=trainingData,subset=trainingData$kMeanNum!=i,linout=TRUE,size=1,maxit=10000)
		currentSubset = subset(trainingData,trainingData$kMeanNum==i)
		nnetPerformance[i] = mean((predict(nnetFIT,currentSubset) - currentSubset$y)^2)
	}


	#KNN  
	install.packages("kknn")
	library(kknn)
	knnPerformance = rep(NA,5)
	for(i in 1:5){
		thisTrain = subset(trainingData,trainingData$kMeanNum!=i)
		thisTest = subset(trainingData,trainingData$kMeanNum==i)
		knnFit = kknn(formula=formulaSet[[formNum]],train=thisTrain,test=thisTest,thisTest)
		
		knnPerformance[i] = mean(( knnFit$fitted.values - thisTest$y)^2)
	}

	print(paste('Formula',formNum,'Results'))
	print(paste('LM Performance is:',mean(lmPerformance)))
	print(paste('CART Performance is:',mean(cartPerformance)))
	print(paste('MARS Performance is:',mean(marsPerformance)))
	print(paste('Random Forest Performance is:',mean(randPerformance)))
	print(paste('Bagging Performance is:',mean(baggPerformance)))
	print(paste('SVM Performance is:',mean(svmPerformance)))
	print(paste('Boosting Performance is:',mean(boostPerformance)))
	print(paste('NNET Performance is:',mean(nnetPerformance)))
	print(paste('K-NN Performance is:',mean(knnPerformance)))
}