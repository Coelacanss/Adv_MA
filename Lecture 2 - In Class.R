#########################################
#Example 1 - Grid Search 1-D
#########################################
#Define a 1-D objective function
objFunc = function(x){
	return((x-3)*(x-5))
}
#True minimum occurs at x = 4 


#Define the grid and the vector to store the output
testX = seq(-10,10,.1)
objectiveValues = rep(NA,length(testX))
#For Loop I
for(i in 1:length(testX)){
	objectiveValues[i] = objFunc(testX[i])
}

#Find the minimum
testX[objectiveValues==min(objectiveValues)]

#Better way using sapply
objectiveValues = sapply(testX,objFunc)

#Find the minimum again
testX[objectiveValues==min(objectiveValues)]


#########################################
#Example 2 - Grid Search 2-D
#########################################
#Define a 2-D objective function
objFunc = function(x,y){
	return((x-3)*(x-5)+(y-3)*(y-6)-x*y)
}


#Define the grid and the vector to store the output
testX = seq(-10,10,.1)
testY = seq(-10,10,.1)

fullGrid = expand.grid(testX,testY)
objectiveValues = rep(NA,nrow(fullGrid))
#For Loop 
for(i in 1:nrow(fullGrid)){
	objectiveValues[i] = objFunc(fullGrid[i,1],fullGrid[i,2])
}

#Find the minimum
fullGrid[objectiveValues==min(objectiveValues),]

#Better way using apply
tempFunc = function(vec){
	return(objFunc(vec[1],vec[2]))
}
objectiveValues = apply(fullGrid,1,tempFunc)

#Find the minimum again
fullGrid[objectiveValues==min(objectiveValues),]

#############################################
#Example 3: Using Optim 
#############################################
#Define a 2-D objective function
objFunc = function(vec){
	x = vec[1]
	y = vec[2]
	return((x-3)*(x-5)+(y-3)*(y-6)-x*y)
}

optim(c(3,4),objFunc)
?optim

##############################################
#Example 4: Constrained Optimization Using NLOPTR
##############################################
#Define a 2-D objective function
objFunc = function(vec){
	x = vec[1]
	y = vec[2]
	return((x-3)*(x-5)+(y-3)*(y-6)-x*y)
}
#Unconstrained Optimization
constrOptim(c(7,7),objFunc,NULL)
#Constrained Optimization with the sum of the two parameters being less than 16
constraintLHS = matrix(c(-1,-1),1,2)
constraintRHS = -16
constrOptim(c(7,7),objFunc,NULL,ui=constraintLHS,ci=constraintRHS)
?constrOptim


##################################################
#In Class Excercise
##################################################
data = read.csv("Homework 1 Data.csv")

data$pricePerUnit = data$dollars/data$units
demandModel = lm(units ~ pricePerUnit, data=data)

lossFunction = function(parameters){
	prediction = parameters[1] + data$pricePerUnit*parameters[2]
	loss = (data$units > prediction) * (data$units - prediction)*(data$pricePerUnit - .4) + (data$units < prediction)*.1*(prediction - data$units)
	totalLoss = sum(loss)
	return(totalLoss)
}

optPar = optim(c(0.1,0.1),lossFunction)$par

lossFunction(c(demandModel$coefficients[1],demandModel$coefficients[1]))

#Compare the predictions
optPrediction = optPar[1] + data$pricePerUnit*optPar[2]
predict(demandModel)

##################################################
#Bootstrap Standard Errors
##################################################


##########################
#Bootstrap Standard Errors
##########################
estFunction = function(bsData){

	lossFunction = function(parameters){
		prediction = parameters[1] + bsData$pricePerUnit*parameters[2]
		loss = (bsData$units > prediction) * (bsData$units - prediction)*(bsData$pricePerUnit - .4) + (bsData$units < prediction)*.1*(prediction - bsData$units)
		totalLoss = sum(loss)
		return(totalLoss)
	}
	
	optPar = optim(c(0.1,0.1),lossFunction)$par
	return(optPar)
}

nBootstraps = 100
bsPars = matrix(0,nBootstraps,2)
for(i in 1:nBootstraps){
	bsSample = sample(1:nrow(data),replace=TRUE)
	bsData = data[bsSample,]
	bsPars[i,] = estFunction(bsData)
}

bsPredictionAtEighty = bsPars[,1] + bsPars[,2]*.8
