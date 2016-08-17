#Sample an initial dataset 

sampledData = rnorm(100,5,2)

#############################
#Non-Parametric Bootstrap
#############################

nBootstraps = 10000
bsMeans = rep(NA,nBootstraps)
for(bsNum in 1:nBootstraps){
	bsSample = sample(sampledData,replace=TRUE)
	bsMeans[bsNum] = mean(bsSample)
}
var(bsMeans)^.5

############################
#Parametric Bootstrap
############################

nBootstraps = 10000
#Assume a normal distribution
sampleMean = mean(sampledData)
sampleVar = var(sampledData)
bsMeans = rep(NA,nBootstraps)
for(bsNum in 1:nBootstraps){
	#Key change is how the data is generate - here it is from a normal distribution with estimated parameters
	bsSample = rnorm(100,sampleMean,sampleVar^.5)
	bsMeans[bsNum] = mean(bsSample)
}
var(bsMeans)^.5