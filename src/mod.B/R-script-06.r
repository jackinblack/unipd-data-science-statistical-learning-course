###########################
# RIDGE REGRESSION
###########################

# load and clean data
library(ISLR2)
Hitters <- na.omit(Hitters)

# fit ls model
#
lm.mod <- lm(Salary~., data=Hitters)
summary(lm.mod)

# compute L1 and L2 norms for least square estimates
# (omit the intercetp)

beta.hat.ls <- coefficients(lm.mod)[-1]

# L1 norm
ell_1.ls <- sum(abs(beta.hat.ls))
ell_1.ls

# L2 norm
ell_2.ls <- sqrt(sum(beta.hat.ls^2))
ell_2.ls

# package for ridge regression
library(glmnet)


# grid of lambda values

grid <- 10^seq(10, -2, length=100)
plot(1:100, grid,  type="b", ylab = "lambda values", pch=20)

# look more closely to the smallest lambda values
plot(80:100, grid[80:100], type="b", ylab = "lambda values", pch=20)

# the grid is linear in the logarithm
plot(log10(grid), type="b", ylab="log-lambda values", pch=20)
summary(log10(grid))


# Apply glmnet() function
#

##################################################
#
#  Objects required to apply the glmnet() function
#
##################################################

# design matrix without the first column
# because we don not consider the intercept
#

X <- model.matrix(Salary~., data=Hitters)
X <- X[,-1]

# vector of responses

y <- Hitters$Salary


# glmnet() function

# some arguments
#
# 1. alpha: alpha=0 applies ridge, alpha=1 (default) applies lasso
#
# 2. standardize: with standardize=TRUE (default) the method is applied to
#                 standardized variables, use standardize=FALSE to turn off
#                 this setting
#
# 3.lambda: grid of lambda values, if it is not given then glmnet()
#           chooses its own sequence


ridge.mod <- glmnet(X, y, alpha=0, lambda=grid)

# plot method for glmnet, produces
# a coefficient profile plot.
#
#  xvar = what is on the X-axis
#
#  xvar = "norm"   plots against the L1-norm
#  xvar = "lambda" against the log-lambda sequence
#  xvar = "dev" against the percent deviance explained
#

plot(ridge.mod, xvar="lambda")

# add labels to identify the variables
plot(ridge.mod, xvar="lambda", label=TRUE)

plot(ridge.mod, xvar = "norm", label=TRUE)
plot(ridge.mod, xvar = "dev",  label=TRUE)

# coef() gives a matrix of coefficients

dim(coef(ridge.mod))

# coefficients for lambda[50]

ridge.mod$lambda[50]
coef(ridge.mod)[,50]

# L2 norm and amount of shrinkage
sqrt(sum(coef(ridge.mod)[-1,50]^2))
sqrt(sum(coef(ridge.mod)[-1,50]^2))/ell_2.ls

plot(ridge.mod, xvar="lambda", label=TRUE)
abline(v=log(ridge.mod$lambda[50]), lty=3, lwd=2)

# coefficients for lambda[70]

ridge.mod$lambda[70]
coef(ridge.mod)[,70]

# # L_2 norm and amount of shrinkage
sqrt(sum(coef(ridge.mod)[-1,70]^2))
sqrt(sum(coef(ridge.mod)[-1,70]^2))/ell_2.ls
abline(v=log(ridge.mod$lambda[70]), lty=3, lwd=2, col="red")

# coefficients for any lambda with interpolation
# s=lambda (interpolates over the grid of lambda used in fitting)

ridge.mod$lambda[50]
ridge.mod$lambda[49]
coef(ridge.mod, s=15000)


# use the predict function for a number of purposes
# s=lambda (interpolates over the grid of lambda used in fitting)
# type="coefficients" returns the beta-values
# type="response" returns the y.hat values


predict(ridge.mod, s=15000, type="coefficients")
y.hat <- predict(ridge.mod, s=15000, newx=X, type="response")

#########################################
#
# evaluate the MSE on a test set
#
#########################################


# select n/2 observations for training set
n <- nrow(X)
n/2

set.seed(1)
train <- sample(1:n, 131)
test  <- setdiff(1:n, train)


# fit ridge regression on the training set
ridge.mod <- glmnet(X[train, ], y[train], alpha = 0, lambda = grid)

plot(ridge.mod, xvar="lambda", label=TRUE)

# estimate the test MSE for one lambda value
# for example lambda = 4

abline(v=log(4), lty=3)
ridge.pred <- predict(ridge.mod, s = 4, newx = X[test, ], type="response")


mean((ridge.pred - y[test])^2)


# estimate the test MSE with lambda extremely large
# (lambda = 10^10 in this case) to obtain the
# model with intercept only

abline(v=log(10^10), lty=3)
ridge.pred <- predict(ridge.mod, s = 1e10, newx = X[test, ])
mean((ridge.pred - y[test])^2)

# check that the coefficients are equal to zero

round(coef(ridge.mod, s=1e10), 4)

# estimate the test MSE with lambda=0 so as to obtain the
# usual least square regression model
# notice that we use the argument exact=TRUE
# so the model is refitted and
# x = X[train, ], y = y[train]
# need to be supplied again
#

ridge.pred <- predict(ridge.mod, s = 0, newx = X[test, ],
                      exact = TRUE, x = X[train, ], y = y[train])
test.mse.ls <- mean((ridge.pred - y[test])^2)
test.mse.ls

########################################################
#
# use cross-validation to choose the value of lambda
#
##########################################################


# nfolds=10 performs ten-fold cross-validation,
# this is the default value if lambda is not given
# then glmnet chooses its own sequence


set.seed(1)
cv.out <- cv.glmnet(X[train, ], y[train], alpha = 0, nfold=10)

# some output values of this function:

# lambda values
length(cv.out$lambda)
cv.out$lambda[1:10]
summary(cv.out$lambda)

# The mean cross-validated error - a vector of length length(lambda).
# (estimates of) the standard errors of cvm

cv.out$cvm[1:10]
cv.out$cvsd[1:10]

# This plots the cross-validation curve (red dotted line) along with upper and lower standard deviation curves
# along the lambda sequence (error bars). Two special values along the lambda sequence are indicated by the vertical
# dotted lines. lambda.min is the value of lambda that gives minimum mean cross-validated error, while lambda.1se
# is the value of lambda that gives the most regularized model such that the cross-validated error is within one
# standard error of the minimum.

plot(cv.out)

# We show how the values of this plot are computed:
# consider the lambda value in position 50 of the sequence

# we have
lambda.50 <- cv.out$lambda[50]
mse.50 <-  cv.out$cvm[50]
mse.sd.50 <- cv.out$cvsd[50]

# see where these values are on the plot

# mean squared error
points(log(lambda.50), mse.50, pch=16)

# mse +/- one sd
points(log(lambda.50), mse.50+mse.sd.50, pch=16, col="blue")
points(log(lambda.50), mse.50-mse.sd.50, pch=16, col="green")

# identify the best lambda value

i.bestlam <- which.min(cv.out$cvm)
i.bestlam
bestlam <- cv.out$lambda[i.bestlam]
bestlam
abline(v=log(bestlam), lwd=2, lty=3, col="blue")
cv.out$cvm[i.bestlam]

min(cv.out$cvm)

# bestlambda is provided from the output of cv.glmnet

bestlam <- cv.out$lambda.min
bestlam

# estimated test MSE with bestlambda value

bestlam <- cv.out$lambda.min
ridge.pred <- predict(ridge.mod, s=bestlam, newx=X[test,])
test.mse.ridge <- mean((ridge.pred-y[test])^2)
test.mse.ridge

# compare the estimated test MSE of ridge and least squares with all predictors

test.mse.ridge
test.mse.ls

# fit on all the data and set lambda equal to bestlam

ridge.final <- glmnet(X, y, alpha = 0)
coef(ridge.final, s=bestlam)

plot(ridge.final, xvar="lambda", label=TRUE)
abline(v=log(bestlam), lty=3, lwd=2)
sqrt(sum(coef(ridge.final, s=bestlam)[-1,1]^2))/ell_2.ls

###########################
# LASSO REGRESSION
###########################

#######################################################
# Explicit solution of lasso estimator with a single
# covariate and standardized variables (soft thresholding)
###########################################################

# function to be minimized
f <- function(beta, lambda, beta.hat){
  y <- 0.5+0.5*beta^2-beta.hat*beta+lambda*abs(beta)
  return(y)
}

# soft thresholding function
soft.thresh <- function(beta.hat, lambda) sign(beta.hat)*max(abs(beta.hat)-lambda, 0)

# set a lambda value

lambda <- 5

# three different cases

# case 1 beta.hat > lambda
#
beta.hat <- 8

# case 2 beta.hat < -lambda
#
beta.hat <- - 8

# case 3 beta.hat -lambda < beta.hat < lambda

beta.hat <- 4
beta.hat <- -4


# see the different results of cases 1 to 3 above
beta.hat.L <- soft.thresh(beta.hat, lambda)
beta.hat.L

curve(f(x, lambda, beta.hat), -10, 10, ylab=expression(f[lambda](beta)), xlab=expression(beta))
abline(v=beta.hat.L, lty=3, col="red")
axis(1, at=beta.hat.L)


# application to Hitters data


# grid of lambda values

grid <- 10^seq(3, -2, length=100)


# alpha=0 is ridge regression
# alpha=1 is lasso regression
# thresh: convergence threshold for coordinate descent.
#
lasso.mod <- glmnet(X,y, alpha=1, lambda=grid, thresh= 1e-10)

plot(lasso.mod, xvar="lambda", label=TRUE)
plot(lasso.mod, xvar="norm", label=TRUE)
plot(lasso.mod, xvar="dev", label=TRUE)


# coefficients for different lambda values
##############################################
plot(lasso.mod, xvar="lambda", label=TRUE)

i <- 90 # try values 10, 30, 50, 70, 90

lasso.mod$lambda[i]
abline(v=log(lasso.mod$lambda[i]), lty=3)

beta.L <- round(coef(lasso.mod)[,i], 4)
beta.L <- beta.L[-1] # remove the intercept
# number of non-zero coefficients
sum(beta.L!=0)
# L1 norm
sum(abs(beta.L))


# Use cross-validation to select the value of lambda
#####################################################

# apply 10fold cross-validation to the training set

set.seed(10)
cv.out.lasso <- cv.glmnet(X[train,], y[train], alpha=1, nfold=10)
plot(cv.out.lasso)

# estimated test MSE

bestlam <- cv.out.lasso$lambda.min
lasso.pred <- predict(lasso.mod, s=bestlam, newx=X[test,])
test.mse.lasso <- mean((lasso.pred-y[test])^2)
test.mse.lasso

# compare the test MSE of lasso, ridge and ls with saturated model

test.mse.lasso
test.mse.ridge
test.mse.ls

# fit the lasso model with best-lambda on all the data

lasso.final <- glmnet(X, y, alpha = 1)
coef(lasso.final, s=bestlam)

lasso.pred <- predict(lasso.final, s=bestlam, newx=X, type="coefficients")



lasso.pred






