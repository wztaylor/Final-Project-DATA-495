########################################################
#                Neural Network                        #
########################################################
data <- SalariesClean[c(3:10)] %>% filter(Year == "2014")
completedata<- data[complete.cases(data),]


# Need to normalize data
completeNormZ <- as.data.frame(scale(completedata[1:6]))
completeNormZ$label = 0
completedata$label = factor(completedata$label)
completeNormZ$label = as.numeric(completeNormz$label)
completeNormZ$label[1:22334] = 1 ##Did some excel tweaking sorted FT/PT to label correctly

#Removing correlated variables
minimalData <- completeNormZ[,c("OvertimePay","OtherPay","Benefits", "label")]
minimalData$label = 0
minimalData$label[1:22334] = 1
minimalData$label = as.numeric(minimalData$label)
minimalData$label = factor(minimalData$label)

#check data types of columns
sapply(completeNormZ, class)

#indexes for regular data
index <- sample(1:nrow(completeNormZ),round(0.25*nrow(completeNormZ)))
train <- completeNormZ[index,]
test <- completeNormZ[-index,]
#had to convert label for testing
test$label <- as.factor(test$label)
test$label <- as.numeric(test$label)

#indexes for minimal data
indexNorm <- sample(1:nrow(minimalData),round(0.15*nrow(minimalData)))
trainNorm <- minimalData[indexNorm,]
testNorm <- minimalData[-indexNorm,]


# NeuralNet package
library(neuralnet)
library(NeuralNetTools)
# neural net training
f <- label ~ OvertimePay + OtherPay + Benefits
f1 <- label ~ OvertimePay + OtherPay + Benefits + BasePay + TotalPay + TotalPayBenefits
nn <- neuralnet(f,data=trainNorm, hidden= c(5,3), stepmax = 1e+06, threshold=0.01, rep=1,err.fct="sse")
nn1 <- neuralnet(f1,data=train, hidden= c(5,3), stepmax = 1e+06, threshold=0.01, rep=1,err.fct="sse")
#neural net prediciton
predict.nn1 <- compute(nn1,test[1:6])

##Look into Soft Max

# nnet package
library(nnet)
nnet <- nnet(f, data=trainNorm, size = 5, maxit=10000)
prediction <- predict(nnet, testNorm)
postResample(prediction, testNorm$label)
posPredValue(prediction, testNorm$label)
negPredValue(prediction, testNorm$label)

nnet <- nnet(f1, data=train, size = 10, maxit=10000)
prediction <- predict(nnet, test)
postResample(prediction, test$label)
posPredValue(prediction, test$label)
negPredValue(prediction, test$label)

#########################################################3
library(caret)
library(e1071)

postResample(prediction, testNorm$label)

postResample(predict.nn1$net.result, test$label)
posPredValue(nn1,test$label)
negPredValue(nn1,test$label)


