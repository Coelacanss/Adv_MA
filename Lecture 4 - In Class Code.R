#####################################
#Panel Data Methods
#####################################
require('plm')
data("Grunfeld", package="plm")
withinModel = plm(inv~value+capital,data=Grunfeld,model="within")
altWithinModel = plm(inv~value+capital+factor(firm),data=Grunfeld)

randomModel = plm(inv~value+capital,data=Grunfeld,model="random")

differModel = plm(inv~value+capital,data=Grunfeld,model="fd")

poolingModel = plm(inv~value+capital,data=Grunfeld,model="pooling")
altPoolingModel = lm(inv~value+capital,data=Grunfeld)


#####################################
#Parametric Bootstrap
#####################################
#Generate Simulated Data
x = rep(c(1:4),25)
y = 1 + 2*x +rnorm(100)
simModel = lm(y~x)

#Set up a bootstrap
nBootstraps = 1000
bsPriceCoef = rep(NA,nBootstraps)
for(i in 1:1000){
	x = rep(c(1:4),25)
	#Draw a simulate y 
	bsY = simModel$coefficients[1]+simModel$coefficients[2]*x + rnorm(100,0,summary(simModel)$sigma)
	#Get the price coefficient
	bsPriceCoef[i] = lm(bsY~x)$coefficients[2]
}
#Confirm it works
sd(bsPriceCoef)
summary(simModel)



######################################
#Likelihood Function - 2 Heads 2 Tails
######################################

likeFunc = function(P){
return(P^2*(1-P)^2)
}
curve(likeFunc,0,1)



######################################
#Likelihood Function - More Data
######################################
#20 Points
heads = 10
tails = 10
likeFunc = function(P){
return(P^heads*(1-P)^tails)
}
curve(likeFunc,0,1)
#200 Points
heads = 100
tails = 100
curve(likeFunc,0,1)
#2000 Points
heads = 1000
tails = 1000
curve(likeFunc,0,1)

######################################
#Log-Likelihood Function - More Data
######################################
#20 Points
heads = 10
tails = 10
logLikeFunc = function(P){
return(heads*log(P)+tails*log(1-P))
}
curve(logLikeFunc,0,1)
#200 Points
heads = 100
tails = 100
curve(logLikeFunc,0,1)
#2000 Points
heads = 1000
tails = 1000
curve(logLikeFunc,0,1)



######################################
#Coin Flipping 
######################################
#Generate a set of coin flips for two different types of coins
nSamples =  1000
nCoins = 1000
pType1 = .3
pType2 = .7
coinProbs = c(pType1,pType2)
coin2Prob = .6
flipSeries = matrix(NA,nCoins,nSamples)
for(coinNum in 1:nCoins){
	#Figure out the coin type
	thisCoinType = runif(1)<coin2Prob
	thisCoinProb = coinProbs[1+thisCoinType]
	thisCoinSamples = runif(nSamples) < thisCoinProb
	flipSeries[coinNum,] = thisCoinSamples
}

#write a log-likelihood function
logLikeFunc = function(parameters){
	#Write a function that returns the likelihood for our three parameters P1, P2, and T
	pType1 = parameters[1]
	pType2 = parameters[2]
	coin2Prob = parameters[3]
	#Using logs for stability
	#Calculate the probability for each coin if it were Type 1 & Sum
	type1LogLikes = rowSums(flipSeries*log(pType1) + (1-flipSeries)*log(1-pType1))
	#Calculate the probability for each coin if it were Type 2 & Sum
	type2LogLikes = rowSums(flipSeries*log(pType2) + (1-flipSeries)*log(1-pType2))
	#Take a weighted sum.  The -1 is there because optim solve minimization problems
	overallLogLikelihood = -1*sum(log(exp(type1LogLikes)*(1-coin2Prob)+exp(type2LogLikes)*coin2Prob))
	#Return the log-likelihood
	return(overallLogLikelihood)
}
#Run a maximum likelihood estimation 
optim(c(.5,.5,.5),logLikeFunc)



