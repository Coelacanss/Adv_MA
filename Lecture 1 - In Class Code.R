

###################################################
#Review of Hypothesis Testing
###################################################
randSamp = (c(exp(rnorm(100,0,1)),exp(rnorm(100,0,3))))
hist((randSamp),xlab="Dollars Spent",main="Histogram Of Spending")
tempMean = mean(randSamp)
tempVar = var(randSamp)
tempSD = (var(randSamp)/200)^.5


###################################################
#Simulate the sampling distribution
###################################################
#Set R to display two plots 
par(mfrow=c(1,2))
#Create a blank vector to store the sampled means
sampledMeans = rep(NA,1000)
nSamples = 10
for(i in 1:1000){
	#Get normal random samples with mean 6 and standard deviation 1
	sampledMeans[i] = mean(rnorm(nSamples,6,1))
}
#Get a histogram with custom breaks
hist(sampledMeans,main="Histogram Of A Mean With 10 Samples",breaks = seq(4,8,.01))

#Do the same with 1000 samples
sampledMeans = rep(NA,1000)
nSamples = 1000
for(i in 1:1000){
	#Get normal random samples with mean 6 and standard deviation 1
	sampledMeans[i] = mean(rnorm(nSamples,6,1))
}
hist(sampledMeans,main="Histogram Of A Mean With 1000 Samples",breaks = seq(4,8,.01))

###################################################
#Basic Example 1 - Standard Error Of The Mean
###################################################
#You wouldn't actually use the bootstrap here, just a familiar example
getRandomSample = function(nSamples){
	return(rnorm(nSamples,6,2))
}
#True mean is 6, true variance and standard deviation is 2
observedSample = getRandomSample(1000)
tempMean = mean(observedSample)
tempSD = (var(observedSample)/1000)^.5
hist(rnorm(100000,tempMean,tempSD),main="Histogram Of Distribution Of Mean Given Observed Data",xlab="Mean Value")

#Create a blank vector to place the bootstrap means
bsMeans = rep(NA,10000)
#Run 10000 bootstrap samples
for(i in 1:10000){
	#get a bootstrap sample with replacement
	#Note we sample the observedSample
	bsSample = sample(observedSample,replace=TRUE)
	#Get the mean of the bootstrap sample and store
	bsMeans[i] = mean(bsSample)
}
#Check the standard deviation
sd(bsMeans)
#Check the histogram
hist(bsMeans,main="Histogram Of Bootstrap Distribution Of Mean",xlab="Mean Value")


##################################################
#Example 2: Candy Crush Spending
##################################################
randSamp = (c(exp(rnorm(100,0,1)),exp(rnorm(100,0,3))))
#Let's ca

#Do 10000 samples
bsMeans = rep(NA,10000)
for(i in 1:10000){
	#Take a bootstrap sample with replacement
	bsSample = sample(randSamp,replace=TRUE)
	#Get the mean of that sample
	bsMeans[i] = mean(bsSample)
}
hist(bsMeans,main="Bootstrap Distribution Of Mean",xlab="Dollars Spent")

##################################################
#Example 3: Standard Error of the Median
##################################################
randSamp = (c(exp(rnorm(100,0,1)),exp(rnorm(100,0,3))))


#Do 10000 samples
bsMedians = rep(NA,10000)
for(i in 1:10000){
	#Take a bootstrap sample with replacement
	bsSample = sample(randSamp,replace=TRUE)
	#Get the mean of that sample
	bsMedians[i] = median(bsSample)
}
hist(bsMedians,main="Bootstrap Distribution Of Median",xlab="Dollars Spent")


##################################################
#Example 4: Confidence Interval For The Median
##################################################
randSamp = (c(exp(rnorm(100,0,1)),exp(rnorm(100,0,3))))
#Let's ca

bsMedians = rep(NA,100000)
for(i in 1:100000){
	#Take a bootstrap sample with replacement
	bsSample = sample(randSamp,replace=TRUE)
	#Get the mean of that sample
	bsMedians[i] = median(bsSample)
}
hist(bsMedians,main="Bootstrap Distribution Of Median",xlab="Dollars Spent")
quantile(bsMedians,c(0.025,0.975))
###################################################
#Example 5 - Standard Error Of Regression Coefficient
###################################################
#Simulate some data
nSamples = 100
price = runif(nSamples,1,2)
demand = 50 - price*5 + rnorm(nSamples)
#Run the regression
lmData = data.frame(demand,price)
summary(lm(demand~price,data=lmData))

#Let's resample to see if we get the same result
nBootstraps = 1000
bsPriceCoefficient = rep(NA,nBootstraps)
for(i in 1:nBootstraps){
	bsSample = sample(nSamples,replace=TRUE)
	#Get the bs sample of the data
	bsData = lmData[bsSample,]
	bsPriceCoefficient[i] = lm(demand~price,data=bsData)$coefficients[2]
}
sd(bsPriceCoefficient)


#A non-straight forward application - let's figure out a CI for revenue when the price is 2 dollars
nBootstraps = 1000
bsTwoDollarRevenue = rep(NA,nBootstraps)
for(i in 1:nBootstraps){
	bsSample = sample(nSamples,replace=TRUE)
	#Get the bs sample of the data
	bsData = lmData[bsSample,]
	lmModel = lm(demand~price,data=bsData)
	bsTwoDollarRevenue[i] = (lmModel$coefficients[1] - 2*lmModel$coefficients[2]) 
}
sd(bsTwoDollarRevenue)
#Why is this so much higher?






