#############################################
#            Random Forests                 #
#############################################
library(randomForest)
data <- SalariesClean[c(3:10)] %>% filter(Year == "2014")
completedata<- data[complete.cases(data),]
completedata = completedata[c(1:6)]
completedata$label = 0
completedata$label[1:22334] = 1
completedata$label = factor(completedata$label)



# Training decision tree and predictions
fit <- randomForest(label ~ ., data=train)
p1<- predict(fit, newdata=test, type="class")

#######

library(caret)
library(e1071)
postResample(p1, test$label)
posPredValue(p1,test$label)
negPredValue(p1,test$label)



# Plotting decision tree
plot(fit)
plot(p1)
getTree(fit,1,labelVar=TRUE)
