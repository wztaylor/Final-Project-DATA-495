########################################
#              SVM                     #
########################################
library(e1071)
library(rpart)
index <- sample(1:nrow(completedata),round(0.50*nrow(completedata)))
train <- completedata[index,]
test <- completedata[-index,]

## svm fit and prediction
svm.model <- svm(label ~ ., data = train, cost = 100, gamma = 1)
svm.pred <- predict(svm.model, test)

#Playing around with plotting
library(kernlab)
model.ksvm = ksvm(label~.,data = train, type="C-svc")
plot(model.ksvm, data=test)


# Accuracy
library(caret)
library(e1071)
postResample(svm.pred, test$label)
posPredValue(svm.pred,test$label)
negPredValue(svm.pred,test$label)

