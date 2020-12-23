# Clear console
cat("\014")
dev.off()

# Remove all variables
rm(list=ls(all=TRUE)) 

# Set working directory
setwd('<FolderPath>')
# Load Data

#Install if the package doesn't exist 
if (!require(ISLR)) install.packages('ISLR')
library(ISLR) 
if (!require(MASS)) install.packages('MASS')
library(MASS)
if (!require(e1071)) install.packages('e1071')
library(e1071)
if (!require(caret)) install.packages('caret')
library(caret)
if (!require(lattice)) install.packages('lattice')
library(lattice)
if (!require(class)) install.packages('class')
library(class)
if (!require(ggplot2)) install.packages('ggplot2')
library(ggplot2)
if (!require(pROC)) install.packages('pROC')
library(pROC)
if (!require(boot)) install.packages('boot')
library(boot)
if (!require(lmtest)) install.packages('lmtest')
library(lmtest)
if (!require(hmeasure)) install.packages('hmeasure')
library(hmeasure)
if (!require(leaps)) install.packages('leaps')
library(leaps)
if (!require(glmnet)) install.packages('glmnet')
library(glmnet)


############ exploratory analysis #################
mali <- read.csv("mali.csv")
mali <- na.omit(mali)
str(mali)
pairs(mali)
cor(mali)
plot(mali$Family,mali$DistRD)
plot(mali$DistRD,mali$Cattle)


row_to_keep = !c(mali$Family>=140 | c(mali$DistRD>450) | c(mali$Cattle>=100))
which(!row_to_keep)
mali <- mali[row_to_keep,]

plot(mali$Family,mali$DistRD)
plot(mali$DistRD,mali$Cattle)

############ Checking if standardizing the variables before performing the analysis would be a good idea #################
str(mali)
standardized.mali <- as.data.frame(scale(mali, center=TRUE, scale=TRUE))
str(standardized.mali)

############ standardize the variables #################
pca <- prcomp(standardized.mali, center = T, scale = T)
names(pca)

pca$sdev
pca$rotation
pca$center
pca$scale
pca$x

dim(pca$x)
head(pca$x)

x.std <- apply(mali, 2, function(x){(x-mean(x))/sd(x)})
max(abs(pca$x - (x.std %*% pca$rotation)))

round(cov(pca$x), 3)

pca$rotation
biplot(pca, scale=0)

pca$rotation <- -pca$rotation
pca$x <- -pca$x
pca$rotation
biplot(pca, scale=0)

pc.var <- pca$sdev^2
pve <- pc.var/sum(pc.var)
pve
cumsum(pve)

plot(pve, xlab = "Principal Component", ylab = "Proportion of Variance Explained", ylim = c(0,1), type = 'b')
plot(cumsum(pve), xlab = "Principal Component", ylab = "Cumulative Proportion of Variance Explained", ylim = c(0,1), type = 'b')


############ Analyzing first two principal components #################
round(pca$rotation[,1:2],3)

head(pca$x[,1:2])
round(cov(pca$x[,1:2]), 3)
pve[1:2]
cumsum(pve)[1:2]
