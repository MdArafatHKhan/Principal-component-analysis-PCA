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
if (!require(pls)) install.packages('pls')
library(pls)

############# linear model using the usual least squares ####################

standardized.X <- scale(Auto[,2:8], center=TRUE, scale=TRUE)
train.X <- standardized.X
train.Y <- Auto[,1]
train <- cbind(train.X,train.Y)
colnames(train)[colnames(train)=="train.Y"] <- "mpg"
train <- as.data.frame(train)
ls.fit = glm(mpg~.,data=train)
summary(ls.fit)
cv.error = cv.glm(train,ls.fit,K=10)
cv.error$delta[1]

############# best-subset selection ####################
totpred <- ncol(Auto)-2
bs.fit <- regsubsets(mpg~.,train,nvmax=totpred)
bs.fit.summary <- summary(bs.fit)
which.max(bs.fit.summary$rsq)
which.max(bs.fit.summary$adjr2)
predict.regsubsets <- function(object, newdata, id, ...) {
form <- as.formula(object$call[[2]])
mat <- model.matrix(form, newdata)
coefi <- coef(object, id = id)
xvars <- names(coefi)
mat[, xvars] %*% coefi
}
k <- 10
set.seed(1)
folds <- sample(1:k, nrow(Hitters), replace = TRUE) 
cv.errors <- matrix(NA, k, totpred, dimnames = list(NULL, paste(1:totpred)))
for (j in 1:k) {
# Best subset selection on the training folds
best.fit <- regsubsets(mpg ~ ., data = train[folds != j,], nvmax = totpred)
# Prediction on the test fold
for (i in 1:totpred) {
# Using the predict.regsubsets function written above
pred <- predict(best.fit, train[folds == j, ], id = i)
cv.errors[j, i] = mean((train$mpg[folds == j] - pred)^2)
}
}
mean.cv.errors <- apply(cv.errors, 2, mean)
mean.cv.errors[which.min(mean.cv.errors)]
plot(mean.cv.errors, type = "b")

fit.best <- regsubsets(mpg ~ ., data = train, nvmax = totpred)
coef(fit.best, 7)

############# ridge regression ####################
standardized.X <- scale(Auto[,2:8], center=TRUE, scale=TRUE)
train.X <- standardized.X
train.Y <- Auto[,1]
train <- cbind(train.X,train.Y)
colnames(train)[colnames(train)=="train.Y"] <- "mpg"
train <- as.data.frame(train)
y <- train.Y
x <- model.matrix(mpg ~.,train)[,-1]
z <- sample(1:nrow(x), nrow(x)/2)
y.test <- y[(-z)]

grid <- 10^seq(10,-2,length=100)

ridge.fit <- glmnet(x[z, ], y[z], alpha = 0, lambda = grid,thresh=1e-12)
plot(ridge.fit, xvar = "lambda")

cv.out <- cv.glmnet(x,y, alpha = 0)
plot(cv.out)

bestlam <- cv.out$lambda.min

ridge.pred <- predict(ridge.fit, s = bestlam, newx = x[(-z), ])
mean((ridge.pred - y.test)^2)
out <- glmnet(x, y, alpha = 0)
predict(out, type = "coefficients", s = bestlam)


############# lasso ####################
lasso.fit <- glmnet(x[z, ], y[z], alpha = 1, lambda = grid)
plot(lasso.fit, xvar = "lambda")
lasso.fit$lambda[70]
coef(lasso.fit)[, 70]
cv.out <- cv.glmnet(x[z, ], y[z], alpha = 1)
plot(cv.out)
bestlam <- cv.out$lambda.min
bestlam

lasso.pred <- predict(lasso.fit, s = bestlam, newx = x[(-z), ])
mean((lasso.pred - y.test)^2)

out <- glmnet(x, y, alpha = 1, lambda = grid)
lasso.coef <- predict(out, type = "coefficients", s = bestlam)
lasso.coef


############# PCR ####################
y.test <- y[(-z)]
pcr.fit <- pcr(mpg ~ ., data = train, subset = z, scale = TRUE, validation = "CV", segments = 10)
summary(pcr.fit)
MSEP(pcr.fit)
pcr.fit <- pcr(mpg ~ ., data = train, scale = TRUE, ncomp = 7)
summary(pcr.fit)
MSEP(pcr.fit)
coef(pcr.fit,ncomp=7,intercept=TRUE)

############# PLS ####################
pls.fit <- plsr(mpg ~ ., data = train, subset = z, scale = TRUE, validation = "CV", segments = 10)
summary(pls.fit)
validationplot(pls.fit, val.type = "MSEP")
pls.pred <- predict(pls.fit, x[(-z), ], ncomp = 5)
mean((pls.pred - y.test)^2)
pls.fit <- plsr(mpg ~ ., data = train, scale = TRUE, ncomp = 5)
summary(pls.fit)
coef(pls.fit,ncomp=5,intercept=TRUE)
