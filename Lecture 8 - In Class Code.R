

###########################################
#Time Series - ACF and PACF Plots
###########################################
timeSeries = rep(NA,10000)
devs = rnorm(10000)
timeSeries[1:2] = rnorm(2)


for(i in 3:10000){
	timeSeries[i] =  timeSeries[i-1]*.95 + devs[i]
}
acf(timeSeries)
pacf(timeSeries)



timeSeries = rep(NA,100000)
devs = rnorm(100000)
timeSeries[1:2] = rnorm(2)


for(i in 3:100000){
	timeSeries[i] =   devs[i] + devs[i-1]*.3 
}
acf(timeSeries)
pacf(timeSeries)



timeSeries = rep(NA,100000)
devs = rnorm(100000)
timeSeries[1:2] = rnorm(2)


for(i in 3:100000){
	timeSeries[i] =  .2*timeSeries[i-2] + devs[i] + 1*((i %% 12) == 1)
}
acf(timeSeries)
pacf(timeSeries)

###########################################
#Fitting ARMA Models
###########################################
fit1 <- arima(presidents, c(1, 0, 0))
fit2 <- arima(presidents, c(0, 0, 1))
fit3 <- arima(presidents, c(1, 0, 1))
fit4 <- arima(presidents, c(2, 0, 1))
AIC(fit1)
AIC(fit2)
AIC(fit3)
AIC(fit4)

modelStat = data.frame(ar = rep(NA,25),ma = rep(NA,25),AIC = rep(NA,25))
rowNum = 1
for(arDegree in 0:4){
	for(maDegree in 0:4){
		currentFit = arima(presidents, c(arDegree, 0, maDegree))
		modelStat[rowNum,] = c(arDegree,maDegree,AIC(currentFit))
		rowNum = rowNum + 1
	}
}


###########################################
#Parallelization
###########################################
install.packages(parallel)
library(parallel)
costlyFunction = function(x){
	Sys.sleep(x)
}
system.time(sapply(1:5,costlyFunction))
no_cores <- detectCores() - 1
cl <- makeCluster(no_cores)
system.time(parLapply(cl,1:5, costlyFunction))


############################################
#Unsupervised Learning
############################################
setwd('C:/Users/Avery/Dropbox/Teaching Lectures')
countryData = read.csv('PCA Example - Countries of the World.csv')
countryData = subset(countryData,complete.cases(countryData))
pcaData = countryData
pcaData$logPopulation = countryData$population
pcaData$logArea = countryData$area
pcaData$Country = NULL
pcaData$Population = NULL 
pcaData$Area = NULL 
pcaData$Climate = NULL
rownames(pcaData) = countryData$Country

fit <- princomp(pcaData, cor=TRUE)
summary(fit) # print variance accounted for 
loadings(fit) # pc loadings 
plot(fit,type="lines") # scree plot 
fit$scores # the principal components
biplot(fit,cex=.5)

install.packages('psych')
library('psych')
fit <- principal(pcaData, nfactors=2, rotate="varimax")
fit # print results

#############################################
#Factor Analysis 
#############################################
fit <- factanal(pcaData, 2, rotation="varimax",scores='regression')
print(fit, digits=2, cutoff=.3, sort=TRUE)
# plot factor 1 by factor 2 
load <- fit$loadings[,1:2] 
plot(load,type="n") # set up plot 
text(load,labels=names(pcaData),cex=.7) # add variable names

plot(fit$scores,type="n") # set up plot 
text(fit$scores,labels=rownames(pcaData),cex=.7) 

#############################################
#Demonstration of Unsupervised Learning Issues
#############################################
coastPCAData = pcaData
coastPCAData$coast2 = coastPCAData$Coastline + rnorm(nrow(pcaData))
coastPCAData$coast3 = coastPCAData$Coastline + rnorm(nrow(pcaData))
coastPCAData$coast4 = coastPCAData$Coastline + rnorm(nrow(pcaData))
coastPCAData$coast5 = coastPCAData$Coastline + rnorm(nrow(pcaData))

fit <- factanal(coastPCAData, 2, rotation="varimax",scores='regression')
print(fit, digits=2, cutoff=.3, sort=TRUE)
# plot factor 1 by factor 2 
load <- fit$loadings[,1:2] 
plot(load,type="n") # set up plot 
text(load,labels=names(coastPCAData),cex=.7) # add variable names

plot(fit$scores,type="n") # set up plot 
text(fit$scores,labels=rownames(coastPCAData),cex=.7) 



