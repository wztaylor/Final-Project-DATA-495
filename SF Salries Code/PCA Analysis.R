#############################
#       PCA Analysis        #
#############################
library(FactoMineR)
library(factoextra)
#scale data
PCAdata.scaled <- scale(completedata[1:6], center = TRUE, scale = TRUE)
PCAdata.scaled <- as.data.frame(PCAdata.scaled)

#compute correlation matrix
PCAcorMatrix <- cor(PCAdata.scaled)
#compute eigen vectors and values
PCAcorMatrix.eig <- eigen(PCAcorMatrix)
PCAcorMatrix.eig$values
PCAcorMatrix.eig$vectors
# Transpose eigenvectors
eigenvectors.t <- t(PCAcorMatrix.eig$vectors)
# Transpose the adjusted data
PCAdata.scaled.t <- t(PCAdata.scaled)
# The new dataset
PCAdata.new <- eigenvectors.t %*% PCAdata.scaled.t
# Transpose new data and rename columns
PCAdata.new <- t(PCAdata.new)
colnames(PCAdata.new) <- c("PC1", "PC2", "PC3", "PC4", "PCA5", "PCA6", "PCA7")
head(PCAdata.new)
#correlation of new data
cor(PCAdata.new)


library(ggplot2)
library(scales)
library(grid)
library(plyr)
library(gridExtra)


# extract parts for plotting
PC1 <- PCA$ind$coord[,1]
PC2 <- PCA$ind$coord[,2]
labs <- rownames(PCA$ind$coord)
PCs <- data.frame(cbind(PC1,PC2))
rownames(PCs) <- labs

# Showing the individual samples
ggplot(PCs, aes(PC1,PC2, label=rownames(PCs))) + 
  geom_text() 
#PCA plot based on color

