setwd('D:/Dropbox/Teaching Lectures')
load('q1DB.Rdata')

#Install the package
install.packages('mlogit')
library('mlogit')

#Clean the data for mlogit
q1DB$choiceID = factor(paste(q1DB$panelID,q1DB$weekNum,sep='-'))
q1DB$isPurchase = q1DB$isPurchase == 1
q1DB$decision = 'purchase'

#The initial dataset only contains characteristics of the 'purchase' option
#I'm adding characteristics of the no purchase option - making this a 'long' DB
#Could also keep data in the previous format as a 'wide' DB
newQ1DB = q1DB
newQ1DB$isPurchase = q1DB$isPurchase==0
newQ1DB$decision = 'noPurchase'
newQ1DB$pricePerUnit = 0

#Combine the datasets 
fullDB = rbind(q1DB,newQ1DB)
fullDB$consumerID = NULL
fullDB$units = NULL


fullDB = fullDB[order(fullDB$choiceID),]

q1MDB = mlogit.data(fullDB ,choice = "isPurchase", shape = "long",varying='pricePerUnit',alt.var = "decision",chid.var='choiceID')

#Estiamte a standard logit model
mlogit(isPurchase ~ pricePerUnit, q1MDB)
#This is the 'mixed logit' - it specifies a normal distribution on 'pricePerUnit'
mlogit(isPurchase ~ pricePerUnit, q1MDB, rpar = c(pricePerUnit= 'n'))


data("Fishing", package = "mlogit")
rpl <- mlogit(mode ~ price+ catch | income, Fishing, varying = 2:9, shape = 'wide', rpar = c(price= 'n', catch = 'n'), correlation = TRUE, halton = NA, R = 10, tol = 10, print.level = 0)
summary(rpl)



#######################################################
#String Handling
#######################################################
fullReviewDB = read.csv('1757 cat toys reviews.csv')
fullReviewDB$is5 = fullReviewDB$star.rating==5
fullReviewDB$reviewLength = nchar(paste(fullReviewDB$review.text))
#Clear cases where number.of.votes = 0 
reviewDB = subset(fullReviewDB,fullReviewDB$number.of.votes >0)
reviewDB$percentHelpful = reviewDB$number.of.helpfulness/reviewDB$number.of.votes

#Look for strings with a digit, then the word 'cats'
grepl('[2-9] cats',reviewDB$review.text)

#Optionally, have additional digits - captures phrases like '12 cat'
grepl('[1-9][0-9]* cat',reviewDB$review.text)

#Allow for an optional 's' on cats  
grepl('[1-9][0-9]* cats*',reviewDB$review.text)

#Make sure cats is a complete word (check for a space, period, or comma after it)
grepl('[1-9][0-9]* cats*[ .,]',reviewDB$review.text)

#Use Grep to get a boolean variable 
grepl('[1-9][0-9]* cats*[ .,]',reviewDB$review.text)






#####################################
#Caret Example
#####################################
install.packages('rpart')
library('rpart')
rPartMod = rpart(percentHelpful~reviewLength,data=reviewDB,method='anova')
install.packages('partykit')
library('partykit')
rPartModParty = as.party(rPartMod)
plot(rPartModParty)

install.packages('caret')
library('caret')
cvControl <- trainControl(method = "repeatedcv", repeats = 10)
caretRpart = train(percentHelpful~reviewLength,data=reviewDB,method='rpart',trControl=cvControl)
plot(caretRpart)

#Confusion Matrix 

###################################################
#Predictive Examples
###################################################
fullVGData = read.csv('studentVGData.csv')
#CART
install.packages("rpart")
library(rpart)

cartFIT = rpart(week.4~week.1+week.2+week.3, data=fullVGData, method="anova") 
print(cartFIT)
text(cartFIT)

#MARS
#The name MARS is trademarked, so the packaged is called earth
install.packages("earth")
library(earth)
marsFIT = earth(week.4~week.1+week.2+week.3, data=fullVGData)
summary(marsFIT)


#Bagging
install.packages("ipred")
library(ipred)
bagging(week.4~week.1+week.2+week.3, data=fullVGData)


#Random Forests
install.packages("randomForest")
library("randomForest")
randForestFit <- randomForest(week.4~week.1+week.2+week.3, data=fullVGData) 
print(randForestFit) # view results 
importance(randForestFit) # importance of each predictor

#Support Vector Machine
install.packages("e1071")
library("e1071")
svmFit = svm(week.4~week.1+week.2+week.3, data = fullVGData)

#Boosting
install.packages("gbm")
library("gbm")
boostingModel = gbm(formula = week.4~week.1+week.2+week.3,data = fullVGData, distribution = "gaussian")
plot(boostingModel)
summary(boostingModel)

#Neural Network
install.packages("nnet")
library("nnet")
nnetFit = nnet(formula = week.4~week.1+week.2+week.3,data = fullVGData, linout=1,size = 2)

#K-Nearest Neighbor
install.packages("FNN")
library("FNN")
knnFit = nn.reg(cbind(fullVGData$week.1,fullVGData$week.2,fullVGData$week.3), y = fullVGData$week.4,k=5)
