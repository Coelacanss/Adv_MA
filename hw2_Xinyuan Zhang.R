### This script is for Adv Marketing Analytics assignment #2
### Xinyuan Zhang

## load raw data
data.unit <- read.csv(file = 'HW2 - Units.csv')
data.price <- read.csv(file = 'HW2 - Prices.csv')

############### Part A: Data Cleaning and Discrete Choice ##################
##
## Question 1
weekNum = c()
consumerID = c()
pricePerUnit = c()
units = c()
isPurchase = c()
for (weekIndex in 1:359) {
      # extract
      current.weekNum = rep(weekIndex, 100)
      current.consumerID = 1:100
      current.pricePerUnit = data.price[,weekIndex+1]
      current.units = data.unit[,weekIndex+1]
      current.isPurchase = ifelse(current.units == 0, 0, 1)
      # combine
      weekNum = c(weekNum, current.weekNum)
      consumerID = c(consumerID, current.consumerID)
      pricePerUnit = c(pricePerUnit, current.pricePerUnit)
      units = c(units, current.units)
      isPurchase = c(isPurchase, current.isPurchase)
      # for tracking
      print(weekIndex)
}
# put into data frame
data.all = data.frame(consumerID = consumerID, weekNum = weekNum, pricePerUnit = pricePerUnit, 
                      units = units, isPurchase = isPurchase)
q1DB = data.all
save(q1DB, file = 'q1DB.Rdata')

## Question 3
# build models
model.linear = lm(isPurchase~pricePerUnit, data = data.all)
model.logit = glm(isPurchase~pricePerUnit, data = data.all, family = binomial(link=logit))
model.probit = glm(isPurchase~pricePerUnit, data = data.all, family = binomial(link=probit))
# check output
summary(model.linear)
summary(model.logit)
summary(model.probit)
# predict on -5 to 5
eachPriceProb = data.frame(pricePerUnit = seq(-5,5,0.01))
eachPriceProb$linear = predict.lm(model.linear, newdata = eachPriceProb, type="response")
eachPriceProb$logit = predict.glm(model.logit, eachPriceProb, type="response")
eachPriceProb$probit = predict.glm(model.probit, eachPriceProb, type = "response")
# visualize
library(ggplot2)
ggplot(data = eachPriceProb) +
      theme_bw() +
      geom_line(aes(x = eachPriceProb$pricePerUnit, y = eachPriceProb$linear)) +
      geom_line(aes(x = eachPriceProb$pricePerUnit, y = eachPriceProb$logit), 
                color = 'green', lty = 2) +
      geom_line(aes(x = eachPriceProb$pricePerUnit, y = eachPriceProb$probit),
                color = 'red', lty = 4) +
      ggtitle('Probability at each price') +
      xlab('price per unit') +
      ylab('probability')

## Question 4
# get the price range in observed data
range(data.all$pricePerUnit)
ggplot(data = eachPriceProb) +
      theme_bw() +
      geom_line(aes(x = eachPriceProb$pricePerUnit, y = eachPriceProb$linear)) +
      geom_line(aes(x = eachPriceProb$pricePerUnit, y = eachPriceProb$logit), 
                color = 'green', lty = 2) +
      geom_line(aes(x = eachPriceProb$pricePerUnit, y = eachPriceProb$probit),
                color = 'red', lty = 4) +
      ggtitle('Probability at each price') +
      xlab('price per unit') +
      ylab('probability') +
      geom_vline(xintercept =  2.19) +
      geom_vline(xintercept =  3.63)

## Question 5: probablity change
probChange <- function(x1,x2,model) {
      prob1 = predict(model,data.frame(pricePerUnit = x1), type = 'response')
      prob2 = predict(model,data.frame(pricePerUnit = x2), type = 'response')
      return(prob2 - prob1)
}
# $1 to $1.1
probChange(1,1.1,model.linear) # linear: -0.008126749
probChange(1,1.1,model.logit) # logit: -0.01199331
probChange(1,1.1,model.probit) # probit: -0.01096658
# $3 to $3.1
probChange(3,3.1,model.linear) # linear: -0.008126749
probChange(3,3.1,model.logit) # logit: -0.007464124
probChange(3,3.1,model.probit) # probit: -0.007579642
# $5 to $5.1
probChange(5,5.1,model.linear) # linear: -0.008126749
probChange(5,5.1,model.logit) # logit: -0.003413129
probChange(5,5.1,model.probit) # probit: -0.003729476

## Question 6
# load new data
data.unit2 <- read.csv(file = 'HW2 - QA6 - Units.csv')
data.price2 <- read.csv(file = 'HW2 - QA6 - Prices .csv')
# create data frame
weekNum = c()
consumerID = c()
pricePerUnit = c()
units = c()
isPurchase = c()
for (weekIndex in 1:359) {
      # extract
      current.weekNum = rep(weekIndex, 200)
      current.consumerID = 1:200
      current.pricePerUnit = data.price2[,weekIndex+1]
      current.units = data.unit2[,weekIndex+1]
      current.isPurchase = ifelse(current.units == 0, 0, 1)
      # combine
      weekNum = c(weekNum, current.weekNum)
      consumerID = c(consumerID, current.consumerID)
      pricePerUnit = c(pricePerUnit, current.pricePerUnit)
      units = c(units, current.units)
      isPurchase = c(isPurchase, current.isPurchase)
      # for tracking
      print(weekIndex)
}
# put into data frame
data.all2 = data.frame(consumerID = consumerID, weekNum = weekNum, pricePerUnit = pricePerUnit, 
                       units = units, isPurchase = isPurchase)
# fit models and save
model.linear2 = lm(isPurchase~pricePerUnit, data = data.all2)
model.logit2 = glm(isPurchase~pricePerUnit, data = data.all2, family = binomial(link=logit))
model.probit2 = glm(isPurchase~pricePerUnit, data = data.all2, family = binomial(link=probit))
save(model.linear, file = 'model.linear.Rda')
save(model.linear2, file = 'model.linear2.Rda')
save(model.logit, file = 'model.logit.Rda')
save(model.logit2, file = 'model.logit2.Rda')
save(model.probit, file = 'model.probit.Rda')
save(model.probit2, file = 'model.probit2.Rda')
# check outputs
summary(model.linear2)
summary(model.logit2)
summary(model.probit2)

## Question 7
# predict when price at $5
100*predict.lm(model.linear, newdata = data.frame(pricePerUnit = 5), type = 'response') #1.943
200*predict.lm(model.linear2, newdata = data.frame(pricePerUnit = 5), type = 'response') #1.943
100*predict.glm(model.logit, newdata = data.frame(pricePerUnit = 5), type = 'response') #7.357
200*predict.glm(model.logit2, newdata = data.frame(pricePerUnit = 5), type = 'response') #7.707
100*predict.glm(model.probit, newdata = data.frame(pricePerUnit = 5), type = 'response') #6.765
200*predict.glm(model.probit2, newdata = data.frame(pricePerUnit = 5), type = 'response') #7.163


########################### Part B: Heterogeneity ########################
## 
## Question 1
library(plm)
model.pooling = plm(units~pricePerUnit,data=data.all,model="pooling")
model.random = plm(units~pricePerUnit,data=data.all,model="random")
model.within = plm(units~pricePerUnit,data=data.all,model="within")
model.differ = plm(units~pricePerUnit,data=data.all,model="fd")
summary(model.pooling)
summary(model.random)
summary(model.within)
summary(model.differ)

## Question 2
model.altPooling = lm(units~pricePerUnit,data=data.all)
model.altWithin = lm(units~pricePerUnit+factor(consumerID),data=data.all)
summary(model.altPooling)
summary(model.altWithin)

## Question 3
AIC(model.pooling)
AIC(model.random)
AIC(model.within)
AIC(model.differ)
AIC(model.altPooling) #67727.86
AIC(model.altWithin) #65390.89

## Question 4
data.purchased = data.all[data.all$isPurchase == 1,]
summary(plm(units~pricePerUnit,data=data.purchased,model="within"))

## Question 5
coefDesc = names(sort(model.altWithin$coefficients, decreasing = T))
totalDesc = totalPurchase$consumerID[order(totalPurchase$spend, decreasing = T)]
result.b5 = data.frame(fixedEffectOrderDesc = coefDesc[1:100], totalPurchaseOrderDesc = totalDesc)
View(result.b5)
# Not exactly in the same order, but they are strongly positively related.


########################### Part C: Consumer Types ########################
##
## Question 1
# calculate total purchase by each customer
data.all$spend = data.all$pricePerUnit * data.all$units
totalPurchase = aggregate(spend~consumerID, data = data.all, sum)
# segmentation
index1 = totalPurchase[order(totalPurchase$spend),1][1:50] # total spend less
index2 = totalPurchase[order(totalPurchase$spend),1][51:100] # total spend more
segment1 = data.all[consumerID %in% index1,]
segment2 = data.all[consumerID %in% index2,]
# fit models
fit1 = glm(isPurchase~pricePerUnit, data = segment1, family = binomial(link=logit))
fit2 = glm(isPurchase~pricePerUnit, data = segment2, family = binomial(link=logit))
summary(fit1)
summary(fit2)

## Question 3
# 2-segment model AIC
AIC(fit1) + AIC(fit2) #36088.78
# standard model AIC
AIC(model.logit) #37012.04

# AIC for a 5-segment model
AIC.sum = 0
for (i in 1:5) {
      firstIndex = (i-1)*20+1
      lastIndex = firstIndex+19
      indexCurrent = totalPurchase[order(totalPurchase$spend),1][firstIndex:lastIndex]
      segmentCurrent = data.all[consumerID %in% indexCurrent,]
      fitCurrent = glm(isPurchase~pricePerUnit, data = segmentCurrent, family = binomial(link=logit))
      AIC.sum = AIC.sum + AIC(fitCurrent)
}
AIC.sum #35617.08

# AIC for a 10-segment model
AIC.sum = 0
for (i in 1:10) {
      firstIndex = (i-1)*10+1
      lastIndex = firstIndex+9
      indexCurrent = totalPurchase[order(totalPurchase$spend),1][firstIndex:lastIndex]
      segmentCurrent = data.all[consumerID %in% indexCurrent,]
      fitCurrent = glm(isPurchase~pricePerUnit, data = segmentCurrent, family = binomial(link=logit))
      AIC.sum = AIC.sum + AIC(fitCurrent)
}
AIC.sum #35291.07
# AIC prefers 10-segment model among these models


### save
save.image(file = 'hw2backup.Rdata')
