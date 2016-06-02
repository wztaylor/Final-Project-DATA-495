#########################################
# Correlation Matrix and visualizations
#########################################

#create correlation matrix to see if there is an obvious pattern
#seems like several variables are really highly correlated
completedataDT$label <- as.numeric(completedataDT$label)
corMatrix  <- cor(completedataDT)
covMatrix <- cov(completedataDT)
varMatrix <- var(completedataDT)

print(corMatrix)


#different visualization packages for correlation matrix
install.packages("corrplot")
library(corrplot)
corrplot(corMatrix, type="upper", method="circle", diag=FALSE, order="hclust")
