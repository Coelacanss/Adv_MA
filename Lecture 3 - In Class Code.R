########################################################
#Custom Metrics: Linear Example
########################################################
#Initilize
setwd('D:/Dropbox/Teaching Lectures')
data = read.csv("Homework 1 Data.csv")
data$pricePerUnit = data$dollars/data$units

demandModel = lm(units ~ pricePerUnit, data=data)

#Step 1: Code the structure of the model.  This is just a function that generates a prediction
getLinearPrediction = function(parameters,currentData){
	prediction = exp(parameters[1] + currentData$pricePerUnit*parameters[2])
	return(prediction)
}

#Step 2: Code the metric you will use to evaluate the potential parameter sets
getMetricValue = function(parameters,currentData){
	prediction = getLinearPrediction(parameters,currentData)
	metric = pmax(currentData$units - prediction,0)*(currentData$pricePerUnit - .4) + .1*pmax(prediction - currentData$units,0)
	return(sum(metric))
}

#Step 3: Use optim to find the set of parameters that optimize this metric
optFit = optim(c(0,0),getMetricValue,NULL,data)

#Step 4: Bootstrap standard errors
nBootstrap = 100
bsParameters = matrix(0,nBootstrap,2)
for(i in 1:nBootstrap){
	currentData = data[sample(nrow(data),replace=TRUE),]
	bsOptFit = optim(c(0,0),getMetricValue,NULL,currentData)
	bsOptFit
	bsParameters[i,] = bsOptFit$par
}
sd(bsParameters[,1])
	
	


#True distribution: A is successful 80% of the time, B is successful 75% of the time, C is successful 50% of the time
########################################################
#Standard sampling
########################################################
sampleSize = 3000
aSamples = runif(sampleSize/3) < .8
bSamples = runif(sampleSize/3) < .75
cSamples = runif(sampleSize/3) < .5

#Compare statistically
t.test(aSamples,bSamples)
t.test(cSamples,bSamples)

totalLoss = 1000*(.8 - .75) + 1000*(.8 - .5)



########################################################
#Thompson Sampling
########################################################

getBSSamplingProbs = function(aSamples,bSamples,cSamples){
	nBootstrap = 10000
	outcomes = rep(NA,nBootstrap)
	for(i in 1:nBootstrap){
		currentAMean = mean(sample(aSamples,replace=TRUE))
		currentBMean = mean(sample(bSamples,replace=TRUE))
		currentCMean = mean(sample(cSamples,replace=TRUE))		
		outcomes[i] = which.max(c(currentAMean,currentBMean,currentCMean))
	}
	
	samplingPlan = c(mean(outcomes==1),mean(outcomes==2),mean(outcomes==3))
}
samplingPlan = c(1,1,1)/3
aSamples = c()
bSamples = c()
cSamples = c()
for(i in 1:(sampleSize/100)){
	#Figure out the number of new samples
	currentSamples = sample(1:3,100,replace=TRUE,samplingPlan)		
	numASamples = sum(currentSamples==1)
	numBSamples = sum(currentSamples==2)
	numCSamples = sum(currentSamples==3)
	
	#Add those samples
	aSamples = c(aSamples,runif(numASamples)<.8)
	bSamples = c(bSamples,runif(numBSamples)<.75)
	cSamples = c(cSamples,runif(numCSamples)<.5)
	
	#Use the bootstrap to evaluate probability that each is best
	samplingPlan = getBSSamplingProbs(aSamples,bSamples,cSamples)
}

t.test(aSamples,bSamples)
totalLoss = length(bSamples)*(.8 - .75) + length(cSamples)*(.8 - .5)



#############################################
#Logit Model Simulation
#############################################
#Simulate Some Logit Data
price = rep(1:4,25)
#Price Coefficient will be -2
purchaseProb = exp(4-2*price)/(1+exp(4-2*price))
purchaseData = runif(100) < purchaseProb

#Fit a logit model
logitModel = glm(purchaseData~price,family=binomial(link=logit))
#Can Look At Summary Statistics
summary(logitModel)
#Predict Choice Probabilities From GLM
predict(logitModel,type="response")

#Compare to the linear regression
summary(lm(purchaseData~price))


#############################################
#Probit Model Simulation
#############################################
#Simulate Some Probit Data
price = rep(1:4,25)
#Price Coefficient will be -2 
purchaseProb =  pnorm(4-2*price,0,1)
purchaseData = runif(100) < purchaseProb

#Fit a Probit model
probitModel = glm(purchaseData~price,family=binomial(link=probit))
#Can Look At Summary Statistics
summary(probitModel)

#Compare to the linear regression
lm(purchaseData~price)

#############################################
#Logit Model S Curve
#############################################
util = seq(-10,10,.01)
probs = exp(util)/(1+exp(util))
plot(probs)

#############################################
#Probit Model S Curve
#############################################
probs = pnorm(seq(-10,10,.01))
plot(probs)


#############################################
#Discrete Choice Models Ain't So Bad? 
#############################################
price = rep(c(1:4),25)
isPurchase = round(.9-.2*price+rnorm(100,0,1))
lm(isPurchase~price)

#############################################
#Tobit Model Code
#############################################
serviceQuality = rep(4:9,100)
consumerSat = 1.2*serviceQuality + rnorm(600,0,1)
hist(consumerSat)
observedConsumerSat = pmin(consumerSat,10)
hist(observedConsumerSat)

lm(consumerSat ~ serviceQuality)
lm(observedConsumerSat ~ serviceQuality)