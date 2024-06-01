
# THE ADVERTISING DATASET

Advertising <- read.csv("Advertising.csv")
attach(Advertising)

plot(TV, sales, pch=20)


# sample size
n <- length(sales)
n

#################
# PREDICTION
#################

plot(TV, sales, pch=20)
mod.out <- lm(sales~TV)
summary(mod.out)

abline(mod.out, lwd=2)


# use the regression line to predict the
# response for TV = 170

coefficients(mod.out)

7.03259355+0.04753664*170

points(170, 15.114, pch="X", col="red")

new.x <- data.frame(TV=170)
predict(mod.out, newdata=new.x)

predict(mod.out, newdata=new.x, interval ="confidence")
predict(mod.out, newdata=new.x, interval ="prediction")

# adding confidence and prediction intervals in the plot

range(TV)

xp <- seq(1, 295, length=100)
new.x <- data.frame(TV=xp)
new.conf <- predict(mod.out, newdata=new.x, interval = "confidence")

lines(xp, new.conf[,2], lty=2, col="red", lwd=2)
lines(xp, new.conf[,3], lty=2, col="red", lwd=2)

new.pred <- predict(mod.out, newdata=new.x, interval = "prediction")
lines(xp, new.pred[,2], lty=2, col="blue", lwd=2)
lines(xp, new.pred[,3], lty=2, col="blue", lwd=2)

###################################
# DIAGNOSTICS AND RESIDUAL PLOTS
###################################

plot(TV, sales, pch=20)
mod.out <- lm(sales~TV)
summary(mod.out)

# residual plot with covariate "TV" on the x-axis
#
par(mfrow=c(1,2))
plot(TV, sales)
abline(mod.out, col="blue", lwd=2)
plot(TV, residuals(mod.out), col="gray40", xlab="TV", ylab="residuals")
lines(loess.smooth(TV, residuals(mod.out)), col="blue", lwd=2)
abline(h=0, lty=2)
par(mfrow=c(1,1))

# residual plot with fitted.values on the x-axis
#
par(mfrow=c(1,2))
plot(TV, sales)
abline(mod.out, col="blue", lwd=2)
plot(fitted(mod.out), residuals(mod.out), col="gray40", xlab="fitted values", ylab="residuals")
lines(loess.smooth(fitted(mod.out), residuals(mod.out)), col="blue", lwd=2)
abline(h=0, lty=2)
par(mfrow=c(1,1))

# residual plots in R

# four residual plots
plot(mod.out)

# choose which you want to plot

plot(mod.out, which=1)
plot(mod.out, which=3)

# the four plots on the same window
par(mfrow=c(2,2))
plot(mod.out)
par(mfrow=c(1,1))

#
# AUTO DATASET
#

library(ISLR2)
data(Auto)
attach(Auto)
mod.out <- lm(mpg~horsepower)

summary(mod.out)

par(mfrow=c(1,2))
plot(horsepower, mpg)
abline(mod.out, col="blue", lwd=2)
plot(fitted(mod.out), residuals(mod.out), col="gray40", xlab="fitted values", ylab="residuals")
lines(loess.smooth(fitted(mod.out), residuals(mod.out)), col="blue", lwd=2)
abline(h=0, lty=2)
par(mfrow=c(1,1))


# try to use this model for prediction

plot(horsepower, mpg, pch=16)
abline(mod.out, lwd=2)
xp <- c(60, 130, 200)
new.x <- data.frame(horsepower=xp)
new.y <- predict(mod.out, newdata=new.x)
points(xp, new.y, pch=17, col="red", cex=1.4)

plot(fitted.values(mod.out), residuals(mod.out), pch=20)
abline(h=0)


# polynomial regression
# fit a polynomial of degree 2 instead
# than a line (that is a polynomial of degree 1)

mod.out2 <- lm(mpg  ~ horsepower + I(horsepower^2))
summary(mod.out2)

# plot the fitted model

plot(horsepower, mpg, pch=20)
range(horsepower)
xp <- seq(46, 230, length=100)
new.x <- data.frame(horsepower=xp)
yp <- predict(mod.out2, newdata=new.x)

lines(xp, yp, lwd=2, col="red")


par(mfrow=c(2,2))
plot(mod.out2)
par(mfrow=c(1,1))

# function poly() for more general polynomial regression
# Example: polynomial of degree 5

mod.out.poly <- lm(mpg~poly(horsepower, 5, raw=TRUE))

summary(mod.out.poly)

range(horsepower)
xp <- seq(46, 230, length=100)
new.x <- data.frame(horsepower=xp)
yp <- predict(mod.out.poly, newdata=new.x)
plot(horsepower, mpg, pch=20)
lines(xp, yp, lwd=2, col="blue")

###################################
# LOG-TRANSFORM OF RESPONSE
###################################

# an alternative way to deal with non linearity
# is that of applying transformations to the variables
# such as the log()

plot(horsepower, log(mpg), pch=20)

mod.out3 <- lm(log(mpg)  ~ horsepower)

summary(mod.out3)
abline(mod.out3, lwd=2)

plot(fitted.values(mod.out3), residuals(mod.out3), pch=20)
abline(h=0)

# plot the fitted curve

range(horsepower)
xp <- seq(46, 230, length=100)
new.x <- data.frame(horsepower=xp)
yp <- predict(mod.out3, newdata=new.x)

plot(horsepower, mpg, pch=20)
lines(xp, exp(yp), lwd=2, col="blue")

#####################################################
# MULTIPLE REGRESSION: LOG-TRANSFORM OF THE RESPONSE
#####################################################

# apply the linear regression with two covariates

mod.out <- lm(mpg ~ horsepower + weight)
summary(mod.out)

# use the fitted model for prediction
# example: weight=2000, horsepower=125

new.x <- data.frame(weight=2000, horsepower=125)
predict(mod.out, newdata=new.x, interval="confidence")

# check the residual plot

plot(fitted.values(mod.out), residuals(mod.out), pch=20)
abline(h=0)

# apply the log-transform to the response

mod.out2 <- lm(log(mpg) ~ horsepower + weight)
summary(mod.out2)

# plot the residual plot
plot(fitted.values(mod.out2), residuals(mod.out2), pch=20)
abline(h=0)

# compare the prediction obtained in this way
# with the one computed before the log-transformation
# was applied

y.pred <- predict(mod.out2, newdata=new.x, interval="confidence")
y.pred
exp(y.pred)





##################################
# EXAMPLE OF POLYNOMIAL REGRESSION
# mcycle dataset
# use of the function poly()
##################################

library(MASS)
data(mcycle)
attach(mcycle)


####

plot(times, accel, xlab="Time", ylab="Acceleration", pch=20)

# polynomial regression with the poly() function

# raw = TRUE  : raw data are used
# raw = FALSE : orthogonal polynomials are used

pol5 <- lm(accel~poly(times, degree = 5, raw=FALSE))
summary(pol5)


# FALSE is the default value for the argument "raw"
# hence we can omit it

pol5 <- lm(accel~poly(times, 5))
summary(pol5)

# compare fitted models as the number of degree
# of the polynomial increases

plot(times, accel, xlab="Time", ylab="Acceleration")

#
# degree = 1 (simple linear model)
#
pol1 <- lm(accel~poly(times, 1, raw=FALSE))
lines(times, pol1$fitted)
summary(pol1)

#
# degree = 5
#
pol5 <- lm(accel~poly(times, 5, raw=FALSE))
lines(times, pol5$fitted, lwd=1.5)
summary(pol5)

#
# degree = 10
#
pol10 <- lm(accel~poly(times, 10, raw=FALSE))
lines(times, pol10$fitted, lwd=1.5, col="red")
summary(pol10)

#
# degree = 15
#
pol15 <- lm(accel~poly(times, 15, raw=FALSE))
lines(times, pol15$fitted, lwd=1.5, col="blue")
summary(pol15)

# we fit a polynomial of degree 12

pol12 <- lm(accel~poly(times, 12))
summary(pol12)


# diagnostic plots

par(mfrow=c(2,2))
plot(pol12)
par(mfrow=c(1,1))


# graphical representation of the fitted regression

plot(times, accel, xlab="Time", ylab="Acceleration")
lines(times, pol12$fitted, lwd=1.5)

# use the model to prediction with confidence levels

pred <- predict(pol12, data.frame(times=20:30),
                interval="prediction", level=0.95)
pred

# add  the predicted values to the plot

points(20:30, pred[,1], col="red", pch=16)



########################################
# more on diagnostics
########################################

# THE ADVERTISING DATASET

Advertising <- read.csv("Advertising.csv")
attach(Advertising)

##################################
# check for heteroscedasticity
##################################


plot(TV, sales, pch=20)

mod.out <- lm(sales~TV)
summary(mod.out)

# residuals vs standardized residuals

e <- residuals(mod.out)
stnd.e <- rstandard(mod.out)
y.hat <- fitted.values(mod.out)

par(mfrow=c(1,2))
plot(y.hat, e, pch=16, ylab="residuals", xlab="fitted values")
plot(y.hat, stnd.e, pch=16, ylab="standardized residuals", xlab="fitted values")
par(mfrow=c(1,1))


# scale-location plot
plot(y.hat, sqrt(abs(stnd.e)), main="scale-location")
lines(loess.smooth(y.hat, sqrt(abs(stnd.e))), col="red")

# scale-location plot from the plot() function

par(mfrow=c(2,2))
plot(mod.out)
par(mfrow=c(1,1))

# heteroscedasticity
# log-transform of response

mod.out <- lm(log(sales)~TV)

par(mfrow=c(2,2))
plot(mod.out)
par(mfrow=c(1,1))

#######################
# outliers
#######################

# Construct a new version of the vector of the observed
# variables that contains an outlier in position 158

sales.outlier <- sales
sales.outlier[158] <- 50
plot(TV, sales.outlier, pch=16)

# fit the two linear models: with and without outliers
mod.out <- lm(sales~TV)
mod.out.outlier <- lm(sales.outlier~TV)

# check the difference in the fitted regression line
abline(mod.out, col="red", lwd=2, lty=2)
abline(mod.out.outlier)

# check the difference in RSE and R^2
summary(mod.out)
summary(mod.out.outlier)

par(mfrow=c(2,2))
plot(mod.out.outlier)
par(mfrow=c(1,1))

rstandard(mod.out.oulier)[158]

##############################
# high leverage point
##############################


sales.lever <- sales
sales.lever[1] <- 20
TV.lever <- TV
TV.lever[1] <- 500

plot(TV.lever, sales.lever, pch=16)

mod.out <- lm(sales~TV)
mod.out.lever <- lm(sales.lever~TV.lever)
abline(mod.out, col="red")
abline(mod.out.lever)

par(mfrow=c(2,2))
plot(mod.out.lever)
par(mfrow=c(1,1))

########################################
# collinearity
########################################

mod.out <- lm(sales~TV+radio)
summary(mod.out)

# include a third collinear variable

set.seed(321)
new.var <- (TV+radio*6)+rnorm(200, sd=1)
cor(cbind(TV, radio, new.var))

mod.out2 <- lm(sales~TV+radio+new.var)
summary(mod.out2)

# variance inflation factors

# the package "car" provides the function vif()
library(car)

vif(mod.out)
vif(mod.out2)

# consider the dataset Auto

library(ISLR2)
data(Auto)
#attach(Auto)

full.mod <- lm(mpg~.-name-origin, data=Auto)
summary(full.mod)
vif(full.mod)

red.mod1 <- lm(mpg~.-origin-name-weight, data=Auto)
summary(red.mod1)
vif(red.mod1)

red.mod2 <- lm(mpg~.-origin-name-weight-displacement, data=Auto)
summary(red.mod2)
vif(red.mod2)

################################
# INTERACTION EFFECTS
################################

mod.out <- lm(sales ~ TV + radio + newspaper)
summary(mod.out)

mod.out <- lm(sales ~ TV + radio)
summary(mod.out)

par(mfrow=c(2,2))
plot(mod.out)
par(mfrow=c(1,1))


mod.out <- lm(sales ~ TV + radio + TV:radio)
summary(mod.out)

par(mfrow=c(2,2))
plot(mod.out)
par(mfrow=c(1,1))

mod.out <- lm(sales ~  TV*radio)


summary(mod.out)


#################################
# CATEGORICAL REGRESSORS
##################################

library(faraway)
data(coagulation)
help(coagulation)

attach(coagulation)


summary(coagulation)
boxplot(coag~ diet, col="cyan", ylab="coagulation")

is.factor(diet)

contrasts(diet)

mod.out <- lm(coag~ diet)
summary(mod.out)

# contrasts and design matrix
diet
contrasts(diet)
model.matrix(mod.out)

# diagnostics

par(mfrow=c(2,2))
plot(mod.out)
par(mfrow=c(1,1))


# AUTO dataset: variable origin

library(ISLR2)
data(Auto)
attach(Auto)
mod.out <- lm(mpg ~ origin)
summary(mod.out)

# is origin used as a categorical variable (factor)?
help(Auto)
table(origin)
is.factor(origin)

# convert origin into a factor
origin.f <- factor(origin, levels=1:3,  labels=c("American", "European", "Japanese"))
is.factor(origin.f)
table(origin.f)

# linear model
mod.out <- lm(mpg ~ origin.f)
summary(mod.out)
contrasts(origin.f)


#########################################
# EXAMPLE: comparing linear regression
# with a binary covariate with the
# comparison of means for two populations
#########################################

normtemp <- read.csv("normtemp.txt", sep="", stringsAsFactors = TRUE)
attach(normtemp)
temp.C <- (temperature -32) *5/9
boxplot(temp.C~gender, col="cyan")

# two-sample t-test

var.test(temp.C~gender)
t.test(temp.C~gender, var.equal=TRUE)

contrasts(gender)
mod.out <- lm(temp.C~gender)
summary(mod.out)

########################
#  ANOVA
#########################

# test for equal variances

bartlett.test(coag~ diet)

aov.diet <- aov(coag~ diet)
summary(aov.diet)

# post-hoc analysis

tukey.diet <- TukeyHSD(aov.diet)
tukey.diet
plot(tukey.diet)

par(mfrow=c(1, 2))
plot(coag~ diet)
plot(tukey.diet)
par(mfrow=c(1,1))

#####################
# ANCOVA
###################

library(faraway)
data(sexab)
help(sexab)

# conditioning plot
coplot(ptsd ~ cpa | csa,  data=sexab)

# add a smooth line through the scatter plot
coplot(ptsd ~ cpa | csa, panel=panel.smooth, data=sexab)

# fit linear model WITH interaction
abuse <- lm(ptsd ~ cpa*csa, data=sexab)
summary(abuse)

attach(sexab)
l <- csa=="Abused"
plot(cpa, ptsd, pch=l+16, col=l+1, xlab="physical abuse", ylab="Post-traumatic stress disorder ")
beta.hat <- coefficients(abuse)
abline(beta.hat[1], beta.hat[2], col="red", lwd=2)
abline(beta.hat[1]+beta.hat[3], beta.hat[2]+beta.hat[4], col="black", lwd=2)
legend(4,1, legend = c("not sexually abused", "sexually abused"), col=1:2, pch=16:17)



# fit linear model WITHOUT interaction
abuse <- lm(ptsd ~ cpa+csa, data=sexab)
summary(abuse)

attach(sexab)
l <- csa=="Abused"
plot(cpa, ptsd, pch=l+16, col=l+1, xlab="physical abuse", ylab="Post-traumatic stress disorder ")
beta.hat <- coefficients(abuse)
abline(beta.hat[1], beta.hat[2], col="red", lwd=2)
abline(beta.hat[1]+beta.hat[3], beta.hat[2], col="black", lwd=2)
legend(4,1, legend = c("not sexually abused", "sexually abused"), col=1:2, pch=16:17)




