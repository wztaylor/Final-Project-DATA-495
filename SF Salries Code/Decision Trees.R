######################################################
#                   Decision Tree                    #  
######################################################
library(rpart)
data <- SalariesClean[c(3:10)] %>% filter(Year == "2014")
completedata<- data[complete.cases(data),]
completedata = completedata[c(1:6)]
completedata$label = 0
completedata$label[1:22334] = 1
completedata$label = factor(completedata$label)
#completedata$label = as.numeric(completedata$label)
##################################################
#         experimenting
##had to lower cp to get it to show more than one branch
completedataDT.rpart1= rpart(formula = completedataDT$label ~ ., data=completedataDT, method="class", cp = 0.001)
plotcp(completedataDT.rpart1)
printcp(completedataDT.rpart1)
plot(completedataDT.rpart1, uniform=TRUE)
text(completedataDT.rpart1)
#pruning
completedataDT.rpart2 <- prune(completedataDT.rpart1, cp = 0.02)
plot(completedataDT.rpart2, uniform=TRUE)
text(completedataDT.rpart2, use.n=TRUE,cex=0.75)
#############################################


index <- sample(1:nrow(completedata),round(0.50*nrow(completedata)))
train <- completedata[index,]
test <- completedata[-index,]
fit = rpart(label ~ ., data=train, method="class", cp = 0.01)
p1<- predict(fit, newdata=test, type="class")
summary(p1)
printcp(fit)
plot(fit, uniform=TRUE)
text(fit)

# Plot the tree	
library(rattle)
library(rpart.plot)

fancyRpartPlot(fit, main="Initial Decision Tree of Job Status")

#############
library(caret)
library(e1071)

postResample(p1, test$label)
posPredValue(p1,test$label)
negPredValue(p1,test$label)



