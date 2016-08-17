rm(list=ls())

# Load packages
library(reshape)
library(plm)

######################################################################
# A Question 1
######################################################################

# Load up data
unitsData = read.csv('HW2 - Units.csv', header=T)
# Use consumerID as the name for the first column
colnames(unitsData)[1] = 'consumerID'

pricesData = read.csv('HW2 - Prices.csv', header=T)
colnames(pricesData)[1] = 'consumerID'

# Use melt() function to convert data into a 'tall' format
unitsDataNew = melt(unitsData, id='consumerID')
pricesDataNew = melt(pricesData, id='consumerID')

# Create new variables in the database q1DB
consumerID = unitsDataNew$consumerID
panelID = 1:nrow(unitsDataNew)
weekNum = rep(NA, nrow(unitsDataNew))
pricePerUnit = pricesDataNew$value
units = unitsDataNew$value
isPurchase = rep(NA, nrow(unitsDataNew))

# Create q1DB data frame
q1DB = data.frame(panelID, weekNum, consumerID, pricePerUnit, units, isPurchase)

# Fill in week number
# The first row of the units data contains the week number 
weekNumber = as.matrix(read.csv('HW2 - Units.csv', header=F, nrow=1))
# Remove the first NA
weekNumber = as.vector(t(weekNumber[,-1]))
# Expand the weekNumber vector by repeating each week number 100 times
weekNumber = rep(weekNumber, each=nrow(unitsData))
q1DB$weekNum = weekNumber

# Fill in isPurchase
q1DB$isPurchase = 1*(q1DB$units>0)

# Save workspace
save(q1DB, file='q1DB.Rdata')


######################################################################
# A Question 3
######################################################################

# Linear probability model
linearPrA3 = lm(isPurchase ~ pricePerUnit, data=q1DB)
summary(linearPrA3)

# Binary logit model
binaryLogitA3 = glm(isPurchase ~ pricePerUnit, data=q1DB, family=binomial(link=logit))
summary(binaryLogitA3)

# Binary probit model
binaryProbitA3 = glm(isPurchase ~ pricePerUnit, data=q1DB, family=binomial(link=probit))
summary(binaryProbitA3)

# Prediction
# Create a sequence of prices and put it in a data frame
pricePerUnit = seq(-5, 5, by=0.1)
predictData = data.frame(pricePerUnit)

# Use the three models to predict the purchase probabilities
linearPredict = predict(linearPrA3, newdata=predictData)
logitPredict = predict(binaryLogitA3, newdata=predictData, type='response')
probitPredict = predict(binaryProbitA3, newdata=predictData, type='response')

# Plot the predictions
plot(predictData$pricePerUnit, linearPredict, type='p', col='red', pch=0, 
     xlab='Prices', ylab='Probability', main='Predictions')
points(predictData$pricePerUnit, logitPredict, col='green', pch=17)
points(predictData$pricePerUnit, probitPredict, col='blue', pch=16)
legend('bottomleft', legend=c('linear model','binary logit','binary probit'), lwd=2, col=c('red','green','blue'),cex=0.8)


######################################################################
# A Question 5
######################################################################

# Create two price data frames containing 1, 3, 5 and 1.1, 3.1, 5.1
priceVector1 = data.frame(c(1, 3, 5))
colnames(priceVector1) = 'pricePerUnit'

priceVector2 = data.frame(c(1.1, 3.1, 5.1))
colnames(priceVector2) = 'pricePerUnit'

# Use the three models to predict the purchase probabilities and take difference
linearPredict1 = predict(linearPrA3, newdata=priceVector1)
linearPredict2 = predict(linearPrA3, newdata=priceVector2)
linearDiff = linearPredict1 - linearPredict2

logitPredict1 = predict(binaryLogitA3, newdata=priceVector1, type='response')
logitPredict2 = predict(binaryLogitA3, newdata=priceVector2, type='response')
logitDiff = logitPredict1 - logitPredict2

probitPredict1 = predict(binaryProbitA3, newdata=priceVector1, type='response')
probitPredict2 = predict(binaryProbitA3, newdata=priceVector2, type='response')
probitDiff = probitPredict1 - probitPredict2


######################################################################
# A Question 6
######################################################################

# Load up data
unitsDataQA6 = read.csv('HW2 - QA6 - Units.csv', header=T)
colnames(unitsDataQA6)[1] = 'consumerID'

pricesDataQA6 = read.csv('HW2 - QA6 - Prices.csv', header=T)
colnames(pricesDataQA6)[1] = 'consumerID'

# Use melt() function to convert data in a tall format
unitsDataNew = melt(unitsDataQA6, id='consumerID')
pricesDataNew = melt(pricesDataQA6, id='consumerID')

# Create new variables in the database q1DB
consumerID = unitsDataNew$consumerID
panelID = 1:nrow(unitsDataNew)
weekNum = rep(NA, nrow(unitsDataNew))
pricePerUnit = pricesDataNew$value
units = unitsDataNew$value
isPurchase = rep(NA, nrow(unitsDataNew))

# Create q6DB data frame
q6DB = data.frame(panelID, weekNum, consumerID, pricePerUnit, units, isPurchase)

# Fill in week number
weekNumber = as.matrix(read.csv('HW2 - QA6 - Units.csv', header=F, nrow=1))
weekNumber = as.vector(t(weekNumber[,-1]))
weekNumber = rep(weekNumber, each=nrow(unitsData))
q6DB$weekNum = weekNumber

# Fill in isPurchase
q6DB$isPurchase = 1*(q6DB$units>0)

# Save workspace
save(q6DB, file="q6DB.Rdata")

# Linear probability model
linearPrA6 = lm(isPurchase ~ pricePerUnit, data=q6DB)
summary(linearPrA6)

# Binary logit model
binaryLogitA6 = glm(isPurchase ~ pricePerUnit, data=q6DB, family=binomial(link=logit))
summary(binaryLogitA6)

# Binary probit model
binaryProbitA6 = glm(isPurchase ~ pricePerUnit, data=q6DB, family=binomial(link=probit))
summary(binaryProbitA6)


######################################################################
# A Question 7
######################################################################

# A new data frame containing price $5
price = data.frame(pricePerUnit = 5)

# Linear model demand
linearDemandA3 = predict(linearPrA3, newdata=price)*100
linearDemandA6 = predict(linearPrA6, newdata=price)*200
linearDemandDiff = linearDemandA3 - linearDemandA6

# Logit model demand
logitDemandA3 = predict(binaryLogitA3, newdata=price, type='response')*100
logitDemandA6 = predict(binaryLogitA6, newdata=price, type='response')*200
logitDemandDiff = logitDemandA3 - logitDemandA6

# Probit model demand
probitDemandA3 = predict(binaryProbitA3, newdata=price, type='response')*100
probitDemandA6 = predict(binaryProbitA6, newdata=price, type='response')*200
probitDemandDiff = probitDemandA3 - probitDemandA6


######################################################################
# B Question 1
######################################################################

# Transform data into a version that is suitable to plm package
q1DBplm = plm.data(q1DB, c('consumerID', 'weekNum'))

# Pooling model
poolingModel = plm(units ~ pricePerUnit, data=q1DBplm, model="pooling")
summary(poolingModel)

# Random effects model
randomModel = plm(units ~ pricePerUnit, data=q1DBplm, model="random")
summary(randomModel)

# The first difference model
differModel = plm(units ~ pricePerUnit, data=q1DBplm, model="fd")
summary(differModel)

# The within model
withinModel = plm(units ~ pricePerUnit, data=q1DBplm, model="within")
summary(withinModel)


######################################################################
# B Question 2
######################################################################

# lm pooling model
lmPoolingModel = lm(units ~ pricePerUnit, data=q1DB)
summary(lmPoolingModel)

# lm within model, include consumer fixed effects
lmWithinModel = lm(units ~ pricePerUnit + factor(consumerID), data=q1DB)
summary(lmWithinModel)


######################################################################
# B Question 3
######################################################################

# AIC values for both models in Q2; pooling model is preferred
lmPoolingAIC = AIC(lmPoolingModel)
print(lmPoolingAIC)

lmWithinAIC = AIC(lmWithinModel)
print(lmWithinAIC)


######################################################################
# B Question 4
######################################################################

# Subset of data
q1DBsubset = subset(q1DB, q1DB$isPurchase>0)

# Within model running on the subset of data
subsetWithinModel = lm(units ~ pricePerUnit + factor(consumerID), data=q1DBsubset)
summary(subsetWithinModel)


######################################################################
# B Question 5
######################################################################

# Create a sequence of consumerID number
consumerID = c(1:100)

# Fixed effects order
# Extract fixed effects coefficients
fixedEffects = lmWithinModel$coefficients[3:101]
# The first fixed effect coefficient is normalized to 0
fixedEffects = c(0, fixedEffects)
# Create a new data frame and order the fixed effects
fixedEffectsDB = data.frame(consumerID, fixedEffects)
# Get the ordered consumerID based on fixed effects
fixedEffectsOrder = fixedEffectsDB[order(fixedEffectsDB$fixedEffects),]$consumerID

# Total sales order
# Use apply() function to calculate total sales for each consumer
unitsDataQB5 = unitsData[,-1]
totalSales = apply(unitsDataQB5, 1, sum)
totalSalesDB = data.frame(consumerID, totalSales)
# Get the ordered consumerID based on total sales
totalSalesOrder = totalSalesDB[order(totalSalesDB$totalSales),]$consumerID

# Check if the order is same; if the following value is 0, both vectors have the same order
print(sum(abs(fixedEffectsOrder-totalSalesOrder)))


######################################################################
# C Question 1
######################################################################

# Median of the total sales
medianSales = median(totalSales)

# Subset of two groups
highSalesDB = subset(totalSalesDB, totalSales>medianSales)
lowSalesDB = subset(totalSalesDB, totalSales<medianSales)

# Get the consumerID for the two groups
indexHigh = highSalesDB$consumerID
indexLow = lowSalesDB$consumerID

# Get the two groups of data
groupHighSales = q1DB[q1DB$consumerID %in% indexHigh,]
groupLowSales = q1DB[q1DB$consumerID %in% indexLow,]

# Logit model
logitHighSales = glm(isPurchase ~ pricePerUnit, data=groupHighSales, family=binomial(link=logit))
summary(logitHighSales)

logitLowSales = glm(isPurchase ~ pricePerUnit, data=groupLowSales, family=binomial(link=logit))
summary(logitLowSales)

# Consumers in the low sales group are more price sensitive


######################################################################
# C Question 3
######################################################################

# Order consumers based on total sales
totalSalesDB = totalSalesDB[order(totalSalesDB$totalSales),]

# Use list to store the consumerID in each segment
# Two segments
twoSegment = list()
twoSegment$twoSegment1 = totalSalesDB[1:50,]$consumerID
twoSegment$twoSegment2 = totalSalesDB[51:100,]$consumerID

# Five segments
fiveSegment = list()
fiveSegment$fiveSegment1 = totalSalesDB[1:20,]$consumerID
fiveSegment$fiveSegment2 = totalSalesDB[21:40,]$consumerID
fiveSegment$fiveSegment3 = totalSalesDB[41:60,]$consumerID
fiveSegment$fiveSegment4 = totalSalesDB[61:80,]$consumerID
fiveSegment$fiveSegment5 = totalSalesDB[81:100,]$consumerID

# Ten segments
tenSegment = list()
tenSegment$tenSegment1 = totalSalesDB[1:10,]$consumerID
tenSegment$tenSegment2 = totalSalesDB[11:20,]$consumerID
tenSegment$tenSegment3 = totalSalesDB[21:30,]$consumerID
tenSegment$tenSegment4 = totalSalesDB[31:40,]$consumerID
tenSegment$tenSegment5 = totalSalesDB[41:50,]$consumerID
tenSegment$tenSegment6 = totalSalesDB[51:60,]$consumerID
tenSegment$tenSegment7 = totalSalesDB[61:70,]$consumerID
tenSegment$tenSegment8 = totalSalesDB[71:80,]$consumerID
tenSegment$tenSegment9 = totalSalesDB[81:90,]$consumerID
tenSegment$tenSegment10 = totalSalesDB[91:100,]$consumerID

# Put all segments together
allSegment = c(twoSegment, fiveSegment, tenSegment)

# Calculate AIC for each segment model
# Create a matrix to store the AIC for each model
allAIC = matrix(NA, nrow=17, ncol=1)

for(i in 1:17){
  
  # Get the subset of q1DB corresponding to each segment of consumers
  q1DBsegment = q1DB[q1DB$consumerID %in% allSegment[[i]], ]
  # Run logit model
  logitSegment = glm(isPurchase ~ pricePerUnit, data=q1DBsegment, family=binomial(link=logit))
  # Calculate AIC
  allAIC[i] = AIC(logitSegment)
  
}

# Calculate two segment AIC
twoSegmentAIC = sum(allAIC[1:2])
print(twoSegmentAIC)

# Calculate five segment AIC
fiveSegmentAIC = sum(allAIC[3:7])
print(fiveSegmentAIC)

# Calculate ten segment AIC
tenSegmentAIC = sum(allAIC[8:17])
print(tenSegmentAIC)

