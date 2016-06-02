####################################################
#   Minimal Dataset removing correlated variables  #
####################################################

#removing totalpay totalpaybene and basepay
minimalData <- completedata[,c("OvertimePay","OtherPay","Benefits")]
minimalData$label = 0
minimalData$label[1:22334] = 1
minimalData$label = factor(minimalData$label)


library(rpart)
library(randomForest)
indexMin <- sample(1:nrow(minimalData),round(0.50*nrow(minimalData)))
trainMin <- minimalData[index,]
testMin <- minimalData[-index,]
fitMin = rpart(label ~ ., data=trainMin, method="class", cp = 0.01) # DT
fitMin <- randomForest(label ~ ., data=trainMin)    # RF
fitMin <- svm(label ~ ., data = trainMin, cost = 100, gamma = 1) # SVM
p1 <- predict(fitMin, testMin)

p1<- predict(fitMin, newdata=testMin, type="class")    #DT

library(caret)
library(e1071)
postResample(p1, testMin$label)
posPredValue(p1,testMin$label)
negPredValue(p1,testMin$label)
