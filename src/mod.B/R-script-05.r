######################
# CROSS VALIDATION
######################

library(ISLR2)
data(Auto)
dim(Auto)
n <- dim(Auto)[1]

# regression of mpg on horsepower
plot(Auto$horsepower, Auto$mpg, xlab="horsepower", ylab="mpg", pch=20)

# linear model (degree 1)
lm.fit <- lm(mpg~horsepower, data=Auto)
summary(lm.fit)
abline(lm.fit, lwd=2, col="blue")


# compute the mean squared error
MSE <- mean(residuals(lm.fit)^2)
MSE

# or, equivalently,
deviance(lm.fit)/n

# polynomial degree 2
lm.fit <- lm(mpg ~ poly(horsepower, degree=2), data=Auto)
summary(lm.fit)
mean(residuals(lm.fit)^2)

# add the curve to the plot
range(Auto$horsepower)
x <- seq(46, 230, length=200)
new.x <- data.frame(horsepower=x)
y.hat <- predict(lm.fit, new.x)
lines(x, y.hat, lwd=2, col="blue")

# polynomial degree 3
lm.fit <- lm(mpg~poly(horsepower, degree=3), data=Auto)
summary(lm.fit)
mean(residuals(lm.fit)^2)

# add the curve to the plot
y.hat <- predict(lm.fit, new.x)
lines(x, y.hat, lwd=2, col="red")

# polynomial degree 15
lm.fit <- lm(mpg~poly(horsepower, degree=15), data=Auto)
summary(lm.fit)
mean(residuals(lm.fit)^2)

# add the curve to the plot
plot(Auto$horsepower, Auto$mpg, xlab="horsepower", ylab="mpg", pch=20)
y.hat <- predict(lm.fit, new.x)
lines(x, y.hat, lwd=2, col="blue")


# Compute the training-MSE for all the
# polynomials of degree from 1 to 10
#
MSE.train <- rep(0, 10)
for(i in 1:10){
  lm.fit <- lm(mpg~poly(horsepower, degree=i), data=Auto)
  MSE.train[i] <- mean(residuals(lm.fit)^2)
}

MSE.train


################################
# the validation set approach
################################

# randomly split the dataset into
# a train set and a validation set
# of the same size (n/2)

dim(Auto)
n/2
set.seed(100)
train <- sample(1:392, size=196, replace= FALSE)
Auto.train <- Auto[ train,]
Auto.val   <- Auto[-train,]

# fit the linear model on the training data
lm.fit <- lm(mpg~horsepower, data=Auto.train)

# predict with the validation set
y.pred <- predict(lm.fit, newdata=Auto.val)

# Compute the Mean Squared Error on the validation set
#
MSE <- mean((Auto.val$mpg-y.pred)^2)
MSE

# Compute the validation-MSE for all the
# polynomials of degree from 1 to 10
#
#
MSE.val   <- rep(0, 10)
for(i in 1:10){
  lm.fit <- lm(mpg~poly(horsepower, degree=i), data=Auto.train)
  y.pred <- predict(lm.fit, newdata=Auto.val)
  MSE.val[i] <- mean((Auto.val$mpg-y.pred)^2)
}

# compare the two MSEs (validation vs training)

MSE.val
MSE.train

plot(1:10, MSE.val, type="b", ylim=c(15,30), xlab="degree", ylab="MSE")
lines(1:10, MSE.train, type="b", col="blue")
legend("topright", lty=1, lwd=2, legend =c("validation MSE", "training MSE"), col=c("black", "blue"))

#####################################
# Leave-One-Out Cross-Validation
#####################################

# We are going to use the package "boot"
# that implements functions for cross-validation
# for GLMs, and, for this reason we need to fit linear
# regression models with the glm function, as follows

glm.fit <- glm(mpg~horsepower, data=Auto, family=gaussian)
coef(glm.fit)

# note that "gaussian" is the default
# value for the argument "family"
#
glm.fit <- glm(mpg~horsepower, data=Auto)

# this is equivalent to
#
lm.fit  <-  lm(mpg~horsepower, data= Auto)

# glm.fit and lm.fit are different R objects, but
# they both contain the same fitted linear models,
# compare for instance the coefficients of the
# fitted models

coefficients(glm.fit)
coefficients(lm.fit)

# We can now use the cv.glm() function
# from the package "boot"
#
library(boot)

# K = n means leave-one-out CV and this is
# the default value of K
#
cv.err <- cv.glm(Auto,glm.fit, K=392)
cv.err <- cv.glm(Auto,glm.fit)

# check the object cv.err

# $k
# is the number of groups
cv.err$K

# $delta
# is a vector of length two. The first component is the raw
# cross-validation estimate of the prediction error.
# The second component is the adjusted cross-validation estimate.
# The adjustment is designed to compensate for the bias introduced
# by not using all the observations.

cv.err$delta

# Compute the leave-one-out-CV-MSE for all the
# polynomials of degree from 1 to 10
#
#
cv.error.loo <- rep(0,10)
for (i in 1:10){
  glm.fit <- glm(mpg~poly(horsepower,i),data=Auto)
  cv.error.loo[i] <- cv.glm(Auto,glm.fit)$delta[1]
}

cv.error.loo

plot(1:10, cv.error.loo, type="b", ylim=c(15,30), xlab="degree", ylab="LOOCV error")


################################
# k-Fold Cross-Validation
###############################

# Compute the 10-fold-CV-MSE for all the
# polynomials of degree from 1 to 10
#
#
set.seed(17)
cv.error.10 <- rep(0,10)
for (i in 1:10){
  glm.fit <- glm(mpg~poly(horsepower,i, raw=TRUE),data=Auto)
  cv.error.10[i] <- cv.glm(Auto,glm.fit,K=10)$delta[1]
}
cv.error.10


# compare the leave-one-out-CV-MES with the 10-fold-CV-MSE

plot(1:10, cv.error.10, type="b", ylim=c(15,30), xlab="degree", ylab="CV error")
lines(1:10, cv.error.loo, type="b", col="blue")
legend("topright", lty=1, lwd=2, legend =c("10-fold-CV MSE", "LOOCV MSE"), col=c("black", "blue"))



###########################################
#
# VARIABLE SELECTION
#
###########################################

###################################
#
# Example of high-dimensional problem:
# breastcancer data
#
###################################

library(gRbase)

data(breastcancer)
dim(breastcancer)
table(breastcancer$code)

help(breastcancer)
pairs(breastcancer[, 1:5])

# difficulties in fitting
# (generalized) linear models to the
# breastcancer data

glm.out <- glm(code~., data=breastcancer, family=binomial)
summary(glm.out)

names(breastcancer)[1:5]

lm.out <- lm(A.1053_at~., data=breastcancer)

summary(lm.out)


###########################
#
# Example:  Hitter data
#
#############################


# Retrieve and clean data data
##############################

# load data

library(ISLR2)
View(Hitters)
names(Hitters)
help(Hitters)
dim(Hitters)

# check for NA's in the response

sum(is.na(Hitters$Salary))

# We see that Salary is missing for 59 players.
# The na.omit() function removes all of the rows that
# have missing values in any variable:


Hitters <- na.omit(Hitters)
dim(Hitters)
sum(is.na(Hitters))


# Best Subset Selection
###########################

# The regsubsets() function (part of the leaps library)
# performs best subset selection by identifying the best
# model that contains a given number of predictors,
# where best is quantified using RSS. The syntax is the
# same as for lm(). The summary() command outputs
# the best set of variables for each model size.


library(leaps)
regfit.full <- regsubsets(Salary~., data=Hitters)


# regsubsets() returns an object of class
# "regsubsets" containing no user-serviceable parts.
# It is designed to be processed by other functions
# such as summary.regsubsets()


reg.summary <- summary(regfit.full)
reg.summary$outmat



# An asterisk ("*") indicates that a given variable is
# included in the corresponding model. For instance, this
# output indicates that the best two-variable model
# contains only Hits and CRBI. By default, regsubsets()
# only reports results up to the best eight-variable model.
# But the nvmax option can be used in order to return as
# many variables as are desired. Here we fit up to a
# 19-variable model:

regfit.full <- regsubsets(Salary~., data=Hitters, nvmax=19)
reg.summary <- summary(regfit.full)
reg.summary$outmat

# check the components of the reg.summary objects
names(reg.summary)
reg.summary$rsq

# We see that the  R2  statistic increases from 32% when
# only one variable is included in the model to almost 55%
# when all variables are included.

# some useful plots
############################

# Plotting RSS, adjusted  R2,  Cp, and BIC
# for all of the models at once will help us
# decide which model to select.

par(mfrow=c(2,2))

# residual sum of squares
plot(reg.summary$rss,xlab="Number of Variables",ylab="RSS",type="l")

# adjusted-R^2 with its largest value
plot(reg.summary$adjr2,xlab="Number of Variables",ylab="Adjusted Rsq",type="l")
i <- which.max(reg.summary$adjr2)
points(i,reg.summary$adjr2[i], col="red",cex=2,pch=20)
text(i,reg.summary$adjr2[i], i, pos=1)

# Mallow's Cp with its smallest value
plot(reg.summary$cp,xlab="Number of Variables",ylab="Cp",type='l')
i <- which.min(reg.summary$cp)
points(i,reg.summary$cp[i],col="red",cex=2,pch=20)
text(i,reg.summary$cp[i], i, pos=3)

# BIC with its smallest value
plot(reg.summary$bic,xlab="Number of Variables",ylab="BIC",type='l')
i <- which.min(reg.summary$bic)
points(i,reg.summary$bic[i],col="red",cex=2,pch=20)
text(i,reg.summary$bic[i], i, pos=3)

par(mfrow=c(1,1))


# The regsubsets() function has a method for the plot() function
# that can be used to display the selected variables for
# the best model with a given number of predictors, ranked
# according to a chosen statistic. The top row of each plot
# contains a black square for each variable selected
# according to the optimal model associated with that
# statistic.


plot(regfit.full, scale="r2")
plot(regfit.full, scale="adjr2")
plot(regfit.full, scale="Cp")
plot(regfit.full, scale="bic")


#######################################
# In the following we only consider
# the "best" model according to BIC
#######################################


# The best model according to BIC is the one
# with 6 predictors

# coefficients of the best model according to BIC
coef(regfit.full, 6)

# fit the selected model
best.bic <- lm(Salary~AtBat+Hits+Walks+CRBI+Division+PutOuts, data=Hitters)
summary(best.bic)

# check how r-squared changes
reg.summary$rsq

# diagnostic plots

plot(best.bic, which = 1)
plot(best.bic, which = 2)
plot(best.bic, which = 3)

# rule of thumb for the identification of
# high leverage points i.e 3(p+1)/n

p.plus.1 <- length(coef(best.bic))
n <- nrow(Hitters)
th <- 3 *p.plus.1/n

plot(best.bic, which = 5)
abline(v = th, lty=3)


########################################
#
# Try to improve the selected model
# by dealing with the problems shown by
# by the diagnostic tools
#
########################################


# log-transform of the response to deal with heteroscedasticity
#################################################################

regfit.full.log <- regsubsets(log(Salary)~., data=Hitters, nvmax=19)
reg.summary.log <- summary(regfit.full.log)
reg.summary.log$bic
which.min(reg.summary.log$bic)
coef(regfit.full.log, 3)


best.bic.log <- lm(log(Salary)~Hits+Walks+Years, data=Hitters)
summary(best.bic.log)

# check how the r-squared changes
reg.summary.log$rsq

# diagnostic plots

plot(best.bic.log, which = 1)
plot(best.bic.log, which = 2)
plot(best.bic.log, which = 3)

# rule of thumb for the identification of
# high leverage points

p.plus.1 <- length(coef(best.bic.log))
th <- 3 *p.plus.1/n

plot(best.bic.log, which = 5)
abline(v = th, lty=3)
th <- 3 *(4+1)/n


# the "curvature" shown by the residual plot suggests
# the presence of non-linearities.
# We can try to model these by introducing interaction
# effects and/or polynomial terms. After a few tries,
# an adequate model seems to be:

best.bic.log.poly <- lm(log(Salary)~poly(Hits, 2)+Walks+poly(Years, 2), data=Hitters)
summary(best.bic.log.poly)


# diagnostic plots

plot(best.bic.log.poly, which = 1)
plot(best.bic.log.poly, which = 2)
plot(best.bic.log.poly, which = 3)

# rule of thumb for the identification of
# high leverage points

p.plus.1 <- length(coef(best.bic.log.poly))
th <- 3 *p.plus.1/n

plot(best.bic.log.poly, which = 5)
abline(v = th, lty=3)


#########################################
# validation set and cross-validation
##########################################

# In the following, for the sake of simplicity,
# we will neither consider possible transformations
# of the response nor interaction/polynomial terms

########################################################
#
# We first take a look at the function model.matrix()
# and how it works.

# Check the structure of the matrix "X" with
# special attention to the columns corresponding
# to the intercept an to categorical variables

X <- model.matrix(Salary~., data=Hitters)

# hence if you fit the model

out <- lm(Salary~., data=Hitters)

# then you can compute the fitted values as

beta  <- coef(out)
y.hat <- X%*%beta

# verify that these coincide with the
# output of the function fitted()

max(abs(y.hat-fitted(out)))

#
###############################################

# We can now follow a validation set approach

# split data into two subsets

n/2

set.seed(111)
#set.seed(222)
#set.seed(123)
#set.seed(231)

train <- sample(1:263,  size=132)
test  <- setdiff(1:263, train)

# apply best-subset selection to the training data

regfit.best <- regsubsets(Salary~., data=Hitters[train,], nvmax=19)

# compute the model matrix for the validation set

test.mat <- model.matrix(Salary~., data=Hitters[test,])

# This is a detailed explanation on how to compute
# the validation errors

# 1. we consider a model, for instance the best model
#    with 3 predictors, and obtain its coefficients

beta <- coef(regfit.best, id=3)

# 2. we compute the submatrix of test.mat corresponding to the
#    3 selected predictors plus the intercept and multiply it by
#    beta so as to obtain the predicted value for the validation set

pred <- test.mat[, names(beta)]%*%beta

# 3. finally we compute the validation error as

mean((Hitters$Salary[test]-pred)^2)

# we iterate this procedure to compute the validation error
# for the 19 selected models

val.errors <- rep(NA,19)
for(i in 1:19){
  beta <- coef(regfit.best, id=i)
  pred <- test.mat[,names(beta)]%*%beta
  val.errors[i] <- mean((Hitters$Salary[test]-pred)^2)
}
val.errors

plot(val.errors,type='b')
i <- which.min(val.errors)
points(i,val.errors[i], col="red",cex=2,pch=20)

# check the high variability of this plot for different
# choices of random seeds, that is for different
# training and validation sets.
#

coef(regfit.best, i)

# compare with the best model according to BIC
coef(regfit.full, 6)

###########################
# k-fold cross-validation
##########################

# set a value for k
k=10

# associate a fold number to each observation

set.seed(111)
#set.seed(222)
#set.seed(123)
#set.seed(231)

folds <- sample(1:k, nrow(Hitters), replace=TRUE)
folds[1:20]
table(folds)

# for each of the k=10 folds we are going to compute
# p=19 validation errors and thus we need a 10X19 matrix

cv.errors <- matrix(NA, k, 19)
colnames(cv.errors) <- 1:19

# apply the validation set approach k times,
# one for each fold
#
for(j in 1:k){
  best.fit <- regsubsets(Salary~., data=Hitters[folds!=j,], nvmax=19)
  test.mat <- model.matrix(Salary~., data=Hitters[folds==j,])
  for(i in 1:19){
    beta <- coef(best.fit, id=i)
    pred <- test.mat[,names(beta)]%*%beta
    cv.errors[j,i] <- mean((Hitters$Salary[folds==j]-pred)^2)
  }
}

# compute the mean of CV errors across the k folds
mean.cv.errors <- apply(cv.errors, 2, mean)
mean.cv.errors

# check the (low) variability of this plot for different
# choices of random seeds
#
plot(mean.cv.errors, type='b')
i <- which.min(mean.cv.errors)
points(i, mean.cv.errors[i], col="red",cex=2,pch=20)

# Note that in this case the procedure is much more stable
# than the validation set aproach: different random seed
# produce very similar results.

# fit the selected model on the full dataset
reg.best <- regsubsets(Salary~.,data=Hitters, nvmax=19)
coef(reg.best, i)


# Forward and Backward Stepwise Selection
##########################################

# forward inclusion
#
regfit.fwd <- regsubsets(Salary~., data=Hitters, nvmax=19, method="forward")
plot(regfit.fwd, scale="bic")

fwd.summary <- summary(regfit.fwd)
plot(fwd.summary$bic, xlab="Number of Variables", ylab="BIC",type='l')
i <- which.min(fwd.summary$bic)
i
points(i,reg.summary$bic[i],col="red",cex=2,pch=20)

# backward elimination
#
regfit.bwd <- regsubsets(Salary~., data=Hitters, nvmax=19, method="backward")
plot(regfit.bwd, scale="bic")

bwd.summary <- summary(regfit.bwd)
plot(bwd.summary$bic,xlab="Number of Variables",ylab="BIC",type='l')
i <- which.min(bwd.summary$bic)
i
points(i,bwd.summary$bic[i],col="red",cex=2,pch=20)


# compare models selected with the three methods

coef(regfit.full, 6)
coef(regfit.fwd, 6)
coef(regfit.bwd, 8)


#######################################
#
# stepwise with step() function
#
########################################

# works with GLMs also
# k is the penalty term (2=AIC and log(n)=BIC)
# trace = 0/1 is verbose or not
# steps= 1000 maximum number of steps

n <- nrow(Hitters)

mod.F <- lm(Salary~., data=Hitters)
mod.bwd.bic <- step(mod.F, direction="backward", k=log(n), trace=1, steps=1000)
summary(mod.bwd.bic)

# compare with backward obtained above
coef(regfit.bwd, 8)


###########################################
#
# A constraint  based backward elimination
# procedure
#
###########################################


mod.F <- lm(Salary~., data=Hitters)
summary(mod.F)


# step 1

mod.R <- update(mod.F, .~.-CHmRun)
anova(mod.R, mod.F)
summary(mod.R)

# step 2

mod.R <- update(mod.R, .~.-Years)
anova(mod.R, mod.F)
summary(mod.R)

# step 3

mod.R <- update(mod.R, .~.-NewLeague)
anova(mod.R, mod.F)
summary(mod.R)

# step 4

mod.R <- update(mod.R, .~.-RBI)
anova(mod.R, mod.F)
summary(mod.R)

# step 5

mod.R <- update(mod.R, .~.-CHits)
anova(mod.R, mod.F)
summary(mod.R)

# step 6

mod.R <- update(mod.R, .~.-HmRun)
anova(mod.R, mod.F)
summary(mod.R)

# step 7

mod.R <- update(mod.R, .~.-Errors)
anova(mod.R, mod.F)
summary(mod.R)

# step 8

mod.R <- update(mod.R, .~.-Runs)
anova(mod.R, mod.F)
summary(mod.R)

# step 9

mod.R <- update(mod.R, .~.-League)
anova(mod.R, mod.F)
summary(mod.R)

# step 10

mod.R <- update(mod.R, .~.-Assists)
anova(mod.R, mod.F)
summary(mod.R)

# step 11

mod.R <- update(mod.R, .~.-CAtBat)
anova(mod.R, mod.F)
summary(mod.R)

# step 12
# should we remove variable CRBI?

mod.RR <- update(mod.R, .~.-CRBI)
anova(mod.RR, mod.F)
summary(mod.RR)

# compare with the model selected using BIC

summary(mod.bwd.bic)

