### This script is for Adv Marketing Analytics Assignment #1
### MSMA  Xinyuan Zhang

################################## Question 1 ################################
##
## load data
data.raw <- read.csv(file = 'Homework 1 Data.csv')
# the column X is the row number in Excel and is meaningless, so drop it.
data.raw$X = NULL

## calculate the price per unit in each week
data.raw$pricePerUnit = data.raw$dollars/data.raw$units

## c
# convert weekOfYear into dummy variable
library(caret)
data.add = data.raw
data.add$weekNum = as.factor(data.add$weekNum)
data.add$weekOfYear = as.factor(data.add$weekOfYear)
dummies <- dummyVars(units ~ weekOfYear, data = data.add)
data.add = as.data.frame(predict(dummies, newdata = data.add))
# create a data frame including units and price, weekNum and dummy weekOfYear
data.model = cbind(units = data.raw$units, pricePerUnit = data.raw$pricePerUnit, weekNum = data.raw$weekNum, data.add)
# demand model
summary(lm(log(units)~., data = data.model))

## univariate linear regression
set.seed(1)
fit.lm = lm(log(units) ~ pricePerUnit, data = data.raw)
save(fit.lm, file = 'fit.lm.Rda')

################################## Question 2 ################################
##
## build a data frame with all possible prices
data.price <- data.frame(pricePerUnit = seq(0,2,0.01))
## use previous model to estimate demand for each price
data.price$estUnit = exp(predict(fit.lm, data.price))
## calculate expected profit
mc = 0.6
data.price$estProfit = (data.price$pricePerUnit - mc) * data.price$estUnit
## max-profit price
data.price[which.max(data.price$estProfit),] # It's $0.83, with profit = 371.5
## create a function
optPrice <- function(fit) {
      data.opt <- data.frame(pricePerUnit = seq(0,2,0.01))
      data.opt$estUnit = exp(predict(fit, data.opt))
      mc = 0.6
      data.opt$estProfit = (data.opt$pricePerUnit - mc) * data.opt$estUnit
      opt.price = data.opt[which.max(data.opt$estProfit),'pricePerUnit']
      return(opt.price)
}
## test
optPrice(fit.lm) # 0.83, correct

################################## Question 3 ################################
## 
## use bootstrap to calculate optimal price, with N=1000
bsOptPrice = rep(NA,1000)
for(i in 1:1000){
      bsSample = data.raw[sample(1:360,replace=TRUE),]
      fitSample = lm(log(units) ~ pricePerUnit, data = bsSample)
      bsOptPrice[i] = optPrice(fitSample)
}
## calculate standard error
sd(bsOptPrice) # 0.03599279
mean(bsOptPrice) # 0.83512

## increase N to N=2000
bsOptPrice = rep(NA,2000)
for(i in 1:2000){
      bsSample = data.raw[sample(1:360,replace=TRUE),]
      fitSample = lm(log(units) ~ pricePerUnit, data = bsSample)
      bsOptPrice[i] = optPrice(fitSample)
}
## calculate standard error
sd(bsOptPrice) # 0.03554943, not changed substantially, which means 1000 samples is enough.
mean(bsOptPrice) # 0.83264

## plot histogram
hist(bsOptPrice, main="Bootstrap Distribution Of Optimal Price",xlab="Mean Value")

################################## Question 4 ################################
##
## increase sample size
bsOptPrice = rep(NA,1000)
for(i in 1:1000){
      bsSample = data.raw[sample(1:360, 4600, replace=TRUE),]
      fitSample = lm(log(units) ~ pricePerUnit, data = bsSample)
      bsOptPrice[i] = optPrice(fitSample)
}
sd(bsOptPrice)


################################## Question 5 ################################
##
## create a funtion to calculate profit
profit <- function(price, fit, cost) {
      beta0 = fit$coefficients[1]
      beta1 = fit$coefficients[2]
      quantity = exp(beta0 + beta1*price)
      pi = (price-cost)*quantity
      return(pi)
}
## calculate the profit resulting from TRUE optimal price
profitStar = rep(profit(0.83,fit.lm,0.6),1000)
## calculate bootstraped optimal profit
profitBS = c()
bsOptPrice = rep(NA,1000)
for(i in 1:1000){
      bsSample = data.raw[sample(1:360,replace=TRUE),]
      fitSample = lm(log(units) ~ pricePerUnit, data = bsSample)
      bsOptPrice[i] = optPrice(fitSample)
      profitBS[i] = profit(bsOptPrice[i], fit.lm, 0.6)
}
sd(bsOptPrice)
## compare the differentce to get the average profit lost per week
profitDiff = profitStar - profitBS
mean(profitDiff) #3.996735

## change sample size into 1000
profitBS = c()
bsOptPrice = rep(NA,1000)
for(i in 1:1000){
      bsSample = data.raw[sample(1:360,1000,replace=TRUE),]
      fitSample = lm(log(units) ~ pricePerUnit, data = bsSample)
      bsOptPrice = optPrice(fitSample)
      profitBS[i] = profit(bsOptPrice, fit.lm, 0.6)
}
## compare the differentce to get the average profit lost per week
profitDiff = profitStar - profitBS
mean(profitDiff) #1.495332, expected profit loss would decrease

## save workspace
save.image(file = 'Assignment1.Rdata')
