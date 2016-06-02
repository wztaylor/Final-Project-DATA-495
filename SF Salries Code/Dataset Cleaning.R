########## Libraries  ###########
library(dplyr)
library(tidyr)
library(ggplot2)
library(readr)
library(stringr)
library(scales)
library(mixtools)

#################################
# Data set Adjustments
#################################

#Complete Dataset
Salaries <- read.csv("C:/Users/Zac/Google Drive/College Stuff/Cofc Spring '16/DATA 495/SF Salaries Data/Salaries.csv", na=c("Not Provided"))
glimpse(Salaries)

#Cleaned Dataset
SalariesClean <-read.csv("C:/Users/Zac/Google Drive/College Stuff/Cofc Spring '16/DATA 495/SF Salaries/SalariesClean.csv", na=c("Not Provided"))
glimpse(SalariesClean)
dim(SalariesClean)        
        
##check for missing values
apply(completedata,2,function(x) sum(is.na(x)))

# Check variables/classes are correct
sapply(SalariesClean, class)

# Data for 2014 #
data2014 <- SalariesClean[c(3:10)] %>% filter(Year == "2014")
data2014 <- Salaries %>% filter(Year == "2014")
completedata<- data2011[complete.cases(data2011),] 

#removing totalpay totalpaybene and basepay due to high correlation
minimalData <- completedata[,c("OvertimePay","OtherPay","Benefits")]
minimalData$label = 0
minimalData$label[1:22334] = 1
minimalData$label = factor(minimalData$label)

# Need to normalize data for certain algorithms
completeNormZ <- as.data.frame(scale(completedata[1:6]))
completeNormZ$label = 0
#completedata$label = factor(completedata$label)
completeNormZ$label[1:22334] = 1 ##Did some excel tweaking sorted FT/PT to label correctly

sapply(test, class)

index <- sample(1:nrow(completeNormZ),round(0.50*nrow(completeNormZ)))
train <- completeNormZ[index,]
test <- completeNormZ[-index,]

