rm(list=ls())

##################################################
#
# Question 1
#
##################################################

# (a) Load data
data = read.csv("Homework 1 Data.csv")

# (b) Create new column 'pricePerUnit'
data$pricePerUnit = data$dollars/data$units

# (c)
# Take log on price
data$logPrice = log(data$pricePerUnit)
# Week number squared
data$weekNum2 = data$weekNum^2
# Create four seasons 0, 1, 2, and 3 represent each season
data$season = floor(data$weekOfYear/13)

# Demand model, treat season as a categorical variable
possibleDemandModel = lm(log(units) ~ logPrice + weekNum2 + factor(season), data=data)
summary(possibleDemandModel)

# (d) "True" model
simpleDemand = lm(log(units) ~ pricePerUnit, data=data)
summary(simpleDemand)

##################################################
#
# Question 2
#
##################################################

# (a)
# Create pricePerUnit vector
pricePerUnit = seq(0, 2, by=0.01)
# Put pricePerUnit in a data frame
newData = data.frame(pricePerUnit)

# (b) Predict demand, the predict function returns values of log(units), so should take use exp(log(units)) to calculate the predict demand
predictDemand = exp(predict(simpleDemand, newData))

# (c) Expected profit = units*(price - marginal cost)
expectedProfit = predictDemand*(pricePerUnit-0.6)

# (d)
# Find the index of the maximum profit
maxPriceIndex = which.max(expectedProfit)
# Use the index to pick out the optimal price
maxPrice = newData$pricePerUnit[maxPriceIndex]

# (e) Optimal price function, the input is a estimated demand model
optimalPrice = function(demandModel){
  
  # Create a pricePerUnit vector and store it in a data frame seqPrice
  pricePerUnit = seq(0, 2, by=0.01)
  seqPrice = data.frame(pricePerUnit)
  # Calculate expected profit; predict() function can generate the predicted values of log(units) given the demandModel and data frame seqPrice
  predict_demand = predict(demandModel, seqPrice)
  expected_profit = exp(predict_demand)*(pricePerUnit-0.6)
  # Find the largest profit and its corresponding price
  index = which.max(expected_profit)
  optimal_price = seqPrice$pricePerUnit[index]
  
  return(optimal_price)
  
}

# optimal price for the "true" model in Quesiton 1 (d)
print(optimalPrice(simpleDemand))

##################################################
#
# Question 3
#
##################################################

# (a)
# Number of bootstrap iterations
nBootstraps = 1000
# Create a matrix to store the optimal price in each iteration of bootstrap
bsPrice1 = matrix(NA, nrow=nBootstraps, ncol=1)
# Sample size
nSamples = nrow(data)

for(i in 1:nBootstraps){  
  
  # Get the bs sample of the data
  bsSample = sample(nSamples,replace=TRUE)
  bsData = data[bsSample,]
  # Reestimate the 'true' demand model using bs sample
  lmModel = lm(log(units) ~ pricePerUnit, data=bsData)
  # Use optimalPrice function to calculate the optimal price
  bsOptimalPrice = optimalPrice(lmModel)
  # Store the optimal price
  bsPrice1[i] = bsOptimalPrice

}

# (b) Standar error of optimal prices
print(sd(bsPrice1))

# (c)
# Increase bootstrap iterations to 2000
nBootstraps = 2000
bsPrice2 = matrix(NA, nrow=nBootstraps, ncol=1)
nSamples = nrow(data)

for(i in 1:nBootstraps){  
  
  bsSample = sample(nSamples,replace=TRUE)
  #Get the bs sample of the data
  bsData = data[bsSample,]
  lmModel = lm(log(units) ~ pricePerUnit, data=bsData)
  bsOptimalPrice = optimalPrice(lmModel)
  bsPrice2[i] = bsOptimalPrice  

}

print(sd(bsPrice2))

# (d) Use hist() function to plot the histogram of optimal price
hist(bsPrice2,main="Bootstrap Distribution Of Optimal Price", xlab="Price Per Unit")

##################################################
#
# Question 4
#
##################################################

nBootstraps = 1000
bsPrice = matrix(NA, nrow=nBootstraps, ncol=1)
nSamples = nrow(data)

# try different starting values of sample size
# Starting value of sample size
sampleSize = 1000
# Initial value of standard error of optimal prices
sdPrice = 0.0358
# Number of iteration
n = 1

# The while loop will not stop until the standard error of optimal prices is less than 1 cent
while(sdPrice>=0.01){
  
  for(i in 1:nBootstraps){
    #Get the bs sample of the data
    bsSample = sample(1:nSamples, sampleSize, replace=TRUE)
    bsData = data[bsSample,]
    lmModel = lm(log(units) ~ pricePerUnit, data=bsData)
    bsOptimalPrice = optimalPrice(lmModel)
    bsPrice[i] = bsOptimalPrice
   }
  
  # Print current iteration
  print(n)
  # Print current sample size
  print(sampleSize)
  
  # Calculate the standard error of optimal prices
  sdPrice = sd(bsPrice)
  # Increase sample size by 100; could change the increment value inorder to narrwo down the search range
  sampleSize = sampleSize + 100
  # Increase the iteration by 1
  n = n+1

}

# Sample size = 4142

##################################################
#
# Question 5
#
##################################################

nBootstraps = 1000
# Create an empty matrix to store the optimal price in each iteration of bootstrap
bsPricePrime = matrix(NA, nrow=nBootstraps, ncol=1)
# Create an empty matrix to store the expected profit in each iteration of bootstrap
bsExpectedProfit = matrix(NA, nrow=nBootstraps, ncol=1)
# Change the name of the column in bsExpectedProfit matrix
colnames(bsExpectedProfit) = 'profitPrime'

# Calculate expected profit for pStar, pStar=0.83
# Create a new data frame with one column priceStar=0.83 and length=nBootstraps
priceStar = data.frame(rep(0.83, nBootstraps))
# Change column names to pricePerUnit in order to use the predict() function
colnames(priceStar) = 'pricePerUnit'
# Use predict() function to calculate the predict demand corresponding to priceStar
predictDemandStar = predict(simpleDemand, newdata=priceStar)
# Calculate expected profit corresponding to priceStar
expectProfitStar = exp(predictDemandStar)*(priceStar-0.6)
# Change column names
colnames(expectProfitStar) = 'expectProfitStar'

# Sample size = 360
nSamples = nrow(data)

for(i in 1:nBootstraps){
  
  #Get the bs sample of the data
  bsSample = sample(nSamples,replace=TRUE)
  bsData = data[bsSample,]
  # Reestimate the 'true' model using bs sample
  lmModel = lm(log(units) ~ pricePerUnit, data=bsData)
  # Calculate the optimal price 
  bsOptimalPrice = optimalPrice(lmModel)
  # Store optimal price
  bsPricePrime[i] = bsOptimalPrice
  
  # Create a data frame priceBin and temporarily store the bs optimal price
  priceBin = data.frame(bsOptimalPrice)
  # Change column names to pricePerUnit in order to use predict() function
  colnames(priceBin) = 'pricePerUnit'
  # Use pridct() function to get the demand corresponding to the bs optimal price
  bsPredictDemand = predict(simpleDemand, priceBin)
  # Calculate the expected profit corresponding to the bs optimal price and store it in the bsExpectedProfit matrix
  bsExpectedProfit[i] = exp(bsPredictDemand)*(bsPricePrime[i]-0.6)

}

# Calculate mean values of expected profits
print(mean(expectProfitStar$expectProfitStar))
print(mean(bsExpectedProfit))

# Profit loss
profitLoss1 = mean(expectProfitStar$expectProfitStar) - mean(bsExpectedProfit)
print(profitLoss1)

# Change sample size to 1000
nBootstraps = 1000
bsPricePrime = matrix(NA, nrow=nBootstraps, ncol=1)
bsExpectedProfit = matrix(NA, nrow=nBootstraps, ncol=1)
colnames(bsExpectedProfit) = 'profitPrime'

priceStar = data.frame(rep(0.83, nBootstraps))
colnames(priceStar) = 'pricePerUnit'
predictDemandStar = predict(simpleDemand, newdata=priceStar)
expectProfitStar = exp(predictDemandStar)*(priceStar-0.6)
colnames(expectProfitStar) = 'expectProfitStar'

nSamples = nrow(data)
# Increase the sample size form 360 to 1000
sampleSize = 1000

for(i in 1:nBootstraps){
  
  bsSample = sample(1:nSamples, sampleSize, replace=TRUE)
  #Get the bs sample of the data
  bsData = data[bsSample,]
  lmModel = lm(log(units) ~ pricePerUnit, data=bsData)
  bsOptimalPrice = optimalPrice(lmModel)
  bsPricePrime[i] = bsOptimalPrice
  
  priceBin = data.frame(bsOptimalPrice)
  colnames(priceBin) = 'pricePerUnit'
  bsPredictDemand = predict(simpleDemand, priceBin)
  bsExpectedProfit[i] = exp(bsPredictDemand)*(bsPricePrime[i]-0.6)
  
}

mean(expectProfitStar$expectProfitStar)
mean(bsExpectedProfit)

profitLoss2 = mean(expectProfitStar$expectProfitStar) - mean(bsExpectedProfit)
print(profitLoss2)
