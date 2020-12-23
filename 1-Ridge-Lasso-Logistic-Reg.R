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


############## standardizing the predictors ##################
str(Caravan)
Purchase <- Caravan[,length(Caravan)]
standardized.X <- as.data.frame(scale(Caravan[,-length(Caravan)], center=TRUE, scale=TRUE))
caravanData <- cbind(standardized.X,Purchase)
str(caravanData)
############## test data to optimally choose the penalty parameter of a shrinkage method ##################
testSamples=1:1000
test=caravanData[testSamples ,]
train=caravanData[-testSamples ,]
str(test)
str(train)
############## logistic regression model using the usual maximum likelihood method ##################
glm.fit <- glm(Purchase~.,data=train,family =binomial)
summary(glm.fit)
glm.prob =predict(glm.fit,test, type ="response")
glm.pred=rep ("No" ,1000)
glm.pred[glm.prob >.20]="Yes"
confusionMatrix(as.factor(glm.pred),test$Purchase)
summary(glm.fit)

############## logistic regression - Ridge penalty. ##################
y <- train$Purchase
x <- model.matrix(Purchase ~ ., train)[, -1]
grid <- 10^seq(10, -2, length = 100)


ridge.mod <- glmnet(x, y, family = "binomial", alpha = 0, lambda = grid,thresh=1e-12)
dim(coef(ridge.mod))
summary(ridge.mod)
plot(ridge.mod, xvar = "lambda")

# Check out the fits for the 1st value of lambda
ridge.mod$lambda[1]
coef(ridge.mod)[, 1]
sqrt(sum(coef(ridge.mod)[-1, 1]^2))
# Compare with the fits for 50th value of lambda
ridge.mod$lambda[50]
coef(ridge.mod)[, 50]
sqrt(sum(coef(ridge.mod)[-1, 50]^2))
# Get fit for a specific value of lambda not on the grid
predict(ridge.mod, s = 50, type = "coefficients")[1:20, ]


# Use a validation set approach to compute test MSE
# Create training and test sets
set.seed(10)
trainIndex <- sample(1:nrow(caravanData), nrow(caravanData)/2)
testIndex <- (-trainIndex)
length(trainIndex)
length(testIndex)
y <- as.numeric(caravanData$Purchase)
x <- model.matrix(Purchase ~ ., caravanData)[, -1]
ridge.mod <- glmnet(x[trainIndex, ], y[trainIndex], alpha = 0, lambda = grid, 
thresh = 1e-12) 
ridge.pred <- predict(ridge.mod, s = 4, newx = x[testIndex, ])
y.test <- y[testIndex]
mean((ridge.pred - y.test)^2)
set.seed(1)
cv.out <- cv.glmnet(x[trainIndex, ], y[trainIndex], alpha = 0)
plot(cv.out)
bestlam <- cv.out$lambda.min
bestlam
ridge.pred <- predict(ridge.mod, s = bestlam, newx = x[testIndex, ])
mean((ridge.pred - y.test)^2)

Ridge.prob <- probabilities
ridge.pred=rep ("No" ,1000)
ridge.pred <- ifelse(ridge.prob > 0.2, "Yes", "No")
table(test.Y,ridge.pred)

probabilities <- predict(ridge.mod,s=cv.out$lambda.min,newx = x[1:1000, ],type='response')
predicted.classes <- ifelse(probabilities > 0.2, "Yes", "No")
observed.classes <- Caravan$Purchase[1:1000]
ridge.prob <- probabilities
ridge.pred=rep ("No" ,1000)
ridge.pred <- ifelse(ridge.prob > 0.2, "Yes", "No")
confusionMatrix(as.factor(ridge.pred),test$Purchase)

out <- glmnet(x, y, alpha = 0)
predict(out, type = "coefficients", s = bestlam)[1:20, ]


############## logistic regression - Lasso penalty. ##################

lasso.mod <- glmnet(x, y, family = "binomial", alpha = 1, lambda = grid,thresh=1e-12)
plot(lasso.mod, xvar = "lambda")
lasso.mod$lambda[70]
coef(lasso.mod)[, 70]
set.seed(1)
cv.out <- cv.glmnet(x[trainIndex, ], y[trainIndex], alpha = 1)
plot(cv.out)
bestlam <- cv.out$lambda.min
bestlam
lasso.pred <- predict(lasso.mod, s = bestlam, newx = x[testIndex, ])
mean((lasso.pred - y.test)^2)

out <- glmnet(x, y, alpha = 1, lambda = grid)
lasso.coef <- predict(out, type = "coefficients", s = bestlam)[1:20, ]
lasso.coef

probabilities <- predict(lasso.mod,s=cv.out$lambda.min,newx = x[1:1000, ],type='response')
predicted.classes <- ifelse(probabilities > 0.2, "Yes", "No")
observed.classes <- Caravan$Purchase[1:1000]
lasso.prob <- probabilities
lasso.pred=rep ("No" ,1000)
lasso.pred <- ifelse(lasso.prob > 0.2, "Yes", "No")

confusionMatrix(as.factor(lasso.pred),test$Purchase)
