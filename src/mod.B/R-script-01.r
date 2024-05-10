# THE ADVERTISING DATASET

Advertising <- read.csv("Advertising.csv")
attach(Advertising)

plot(TV, sales, pch=20)


# sample size
n <- length(sales)
n

###############################
# COVARIANCE AND CORRELATION
###############################

# computation of the covariance between TV and sales

cov.st <- sum((sales-mean(sales))*(TV-mean(TV)))/(n-1)
cov.st
cov(sales, TV)

# HOW TO INTERPRET THE MAGNITUDE OF
# THE COVARIANCE BETWEEN TWO VARIABLES

# scatterplot when the two variables are centered

plot((TV-mean(TV)), (sales-mean(sales)), pch=20)
abline(h=0, v=0, lwd=2)


# different colors to distinguish point laying on the
# first-third quadrants from those laying in
# second-forth quadrants

par(mfrow=c(1, 2))

plot(TV, sales, pch=20)
abline(h=mean(sales), v=mean(TV), lwd=2)

plot((TV-mean(TV)), (sales-mean(sales)), col=3+sign((TV-mean(TV))*(sales-mean(sales))), pch=20)
abline(h=0, v=0, lwd=2)
par(mfrow=c(1, 1))


# example of non-linear association: motorcycle data

library(MASS)
data(mcycle)
attach(mcycle)
plot(times, accel, pch=20)

plot(times-mean(times), accel-mean(accel), pch=20)
abline(h=0, v=0, lwd=2)

cor(mcycle$times, mcycle$accel)

# COVARIANCE MATRIX, using a build-in R function

cov(Advertising)

cov(Advertising[,-1])

# explicit computation in R of a covariance matrix

X <- Advertising[, -1]
is.matrix(X)
is.data.frame(X)

X <- as.matrix(X)

# compute the vector of sample means
mean.vec <- apply(X, 2, mean)
mean.vec

# obtain a matrix of sample means
uv <- rep(1, n)
M <- uv%*%t(mean.vec)

dim(M)
M[1:5,]

# center the sample
Xc <- X- M

# check that variables are centered
round(apply(Xc, 2, sum), 5)

# compute the covariance matrix
cov.mat <- t(Xc)%*%Xc/(n-1)
cov.mat

# check that this gives the same result as the
# R function

cov(X)

# correlation matrix

cor(X)

# compute the correlation matrix from the covariance matrix
# by scaling to obtain diagonal entries equal to one

D <- diag(cov.mat)
D
D <- diag(D)
D

cor.mat <- solve(sqrt(D))%*%cov.mat%*%solve(sqrt(D))
cor.mat

# this is also done by the function cov2cor()
cov2cor(cov.mat)

# some ways to visualize a correlation matrix

library(corrplot)

corrplot(cor(X), method = 'number')
corrplot(cor(X), method = 'color')
corrplot(cor(X), method = 'ellipse')



# FUNCTION "pairs" for matrix plot

pairs(X)

pairs(X, diag.panel=panel.hist)

# define the functions "panel.hist" and "panel.cor"
# before using the following two commands.
# (the two functions can be found below but they are
# provided in the help of the function "pairs"
#

pairs(X, diag.panel=panel.hist, upper.panel=panel.cor)
pairs(X, diag.panel=panel.hist, upper.panel=panel.cor, lower.panel=panel.smooth)

## panel.hist function
## puts histograms on the diagonal

panel.hist <- function(x, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(usr[1:2], 0, 1.5) )
  h <- hist(x, plot = FALSE)
  breaks <- h$breaks; nB <- length(breaks)
  y <- h$counts; y <- y/max(y)
  rect(breaks[-nB], 0, breaks[-1], y, col = "cyan", ...)
}

## panel.cor function
## put (absolute) correlations on the upper panels,
## with size proportional to the correlations.

panel.cor <- function(x, y, digits = 2, prefix = "", cex.cor, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- abs(cor(x, y))
  txt <- format(c(r, 0.123456789), digits = digits)[1]
  txt <- paste0(prefix, txt)
  if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
  text(0.5, 0.5, txt, cex = cex.cor * r)
}

# Plot correlation matrix ellipses

# THE ADVERTISING DATASET

Advertising <- read.csv("Advertising.csv")
attach(Advertising)

plot(TV, sales, pch=20)


# sample size
n <- length(sales)
n


###############################
# SIMPLE LINEAR REGRESSION
###############################


x <- TV
y <- sales

plot(x, y, pch=20)

# regression coefficients

beta1.hat <- cov(x, y)/var(x)
beta0.hat <- mean(y)- beta1.hat*mean(x)

beta0.hat
beta1.hat
abline(beta0.hat, beta1.hat, col="blue", lwd=2)

# fitted values

y.hat <- beta0.hat+beta1.hat*x

points(x, y.hat, col="red", pch=20)

# residuals

e <- y -y.hat
sum(e)

# RSS

RSS <- sum(e^2)
RSS

# RSE

RSE <- sqrt(RSS/(200-2))
RSE

# the lm() function

reg.out <- lm(y~x)
summary(reg.out)

# extraction of useful quantities

e <- residuals(reg.out)
e[1:10]

y.hat <- fitted.values(reg.out)
y.hat[1:10]

beta.hat <- coefficients(reg.out)
beta.hat


# add regression line to the plot

plot(x, y, pch=16)
abline(beta.hat[1], beta.hat[2])

# more compactly
abline(reg.out)

# confidence intervals for the parameters

confint(reg.out)
confint(reg.out, level=0.8)


# checking properties of the residuals

e     <- residuals(reg.out)
y.hat <- fitted.values(reg.out)
sum(e)
sum(e*x)
sum(e*y.hat)

# Decomposition of variability and
# R-squared statistic

TSS <- sum((y-mean(y))^2)
RSS <- sum((y-y.hat)^2)

R2 <- (TSS-RSS)/TSS
R2


#############################
# MULTIPLE LINEAR REGRESSION
#############################


# relevant matrices and computation beta.hat

X <- cbind(1, Advertising[, 2:4])
X <- as.matrix(X)
X[1:5,]

y <- sales

n <- 200
p <- 3

# computation of the coefficients (least squares)


t(X) %*% X

beta.hat <- solve(t(X)%*%X) %*% t(X) %*% y
beta.hat

# residuals

y.hat <- X %*% beta.hat
e <- y - y.hat

# checking properties of the residuals

round(sum(e), 3)
round(t(e)%*%X, 3)
round(t(e)%*%y.hat, 3)

# two alternative ways to compute RSE

RSE <- sqrt(t(e) %*% e/(n-p-1))
RSE

RSE <- sqrt(sum(e^2)/(n-p-1))
RSE


# SE's of coefficient estimates

var.beta.hat <- RSE^2*solve(t(X)%*%X)
round(var.beta.hat, 7)
se.beta.hat <- sqrt(diag(var.beta.hat))
se.beta.hat



# t tests for H_0: beta_j=0

t.obs <- beta.hat/se.beta.hat
t.obs

p.values <- 2*pt(-abs(t.obs), n-p-1)
round(p.values, 4)

# confidence intervals

t_0.975 <- qt(0.975, n-p-1)

lower <- beta.hat -t_0.975*se.beta.hat
upper <- beta.hat +t_0.975*se.beta.hat

cbind(lower, upper)

confint(reg.out)


# the function lm() for multiple linear regression

reg.out <- lm(sales~1+TV+radio+newspaper)
summary(reg.out)

# the intercept is present by default...

reg.out <- lm(sales~TV+radio+newspaper)
summary(reg.out)

# ... but it is possible to remove it

reg.out.WithoutIntercept <- lm(sales~-1+TV+radio+newspaper)
summary(reg.out.WithoutIntercept)

# interpretation of the parameters: comparison
# between simple and multiple regression

simple.reg.out <- lm(sales~TV)
summary(simple.reg.out)


simple.reg.out <- lm(sales~newspaper)
summary(simple.reg.out)



# Simple linear regression
# test H0: beta1=0 using the t-test

n <- 200

full.mod <- lm(sales~TV)
summary(full.mod)

beta1.hat <-coefficients(full.mod)[2]
beta1.hat

e <- residuals(full.mod)
RSS <- sum(e^2)

s2 <- RSS/(n-2)
RSE <- sqrt(s2)
RSE

se.hat <- sqrt(s2/sum((TV-mean(TV))^2))
se.hat

t.obs <- beta1.hat/se.hat
t.obs

p.value <- 2*(pt(-abs(t.obs), n-2))
p.value

# Relationship between t and F

F.obs <- t.obs^2
F.obs
1 - pf(F.obs, 1, n-2)


# Simple linear regression
# test H0: beta1=0 using model comparison


# reduced model

red.mod <- lm(sales~+1)

e0<- residuals(red.mod)

RSS0 <- sum(e0^2)
RSS0

# check that RSS0 is equal to TSS
TSS <- sum((sales-mean(sales))^2)
TSS

# compute the F statistic
F.obs <- (RSS0-RSS)/(RSS/(n-2))
F.obs

summary(full.mod)



# multiple linear regression, test all-coefficients

reg.out1 <- lm(sales~TV+radio+newspaper)
reg.out0 <- lm(sales~+1)

RSS0 <- sum(residuals(reg.out0)^2)
RSS1 <- sum(residuals(reg.out1)^2)

F.obs <- ((RSS0-RSS1)/3)/(RSS1/(200-3-1))
F.obs

p.value <- 1 - pf(F.obs, 3, 196)
p.value

anova(reg.out0, reg.out1)


# F-test for the comparison of nested models


full.mod <- lm(sales~TV+radio+newspaper)
red.mod <-  lm(sales~TV)

anova(red.mod, full.mod)

# note that the R^2 increases (or better non-decreases)
# as more variables are added to the regression

out <- lm(sales~TV)
summary(out)$r.squared

out <- lm(sales~TV+radio)
summary(out)$r.squared

out <- lm(sales~TV+radio+newspaper)
summary(out)$r.squared

####################################################
# EXAMPLE: Auto dataset and selection of variables
####################################################

# package from the book "Introduction to Statistical Learning" 2nd edition
library(ISLR2)
data("Auto")
attach(Auto)

help(Auto)
names(Auto)


full.mod <- lm(mpg~cylinders+displacement+horsepower+weight+acceleration+year+origin+name)
summary(full.mod)

full.mod <- lm(mpg~., data=Auto)
summary(full.mod)

# remove the covariate "name"
full.mod <- lm(mpg~.-name, data=Auto)
summary(full.mod)


# we note that the variable "origin" is categorical
# and (for the moment) we remove it

full.mod <- lm(mpg~.-name-origin, data=Auto)
summary(full.mod)

# backward stepwise procedure
red.mod1 <- update(full.mod, ~.-horsepower)
summary(red.mod1)

red.mod2 <- update(red.mod1, ~.-cylinders)
summary(red.mod2)

red.mod3 <- update(red.mod2, ~.-displacement)
summary(red.mod3)

red.mod4 <- update(red.mod3, ~.-acceleration)
summary(red.mod4)

anova(red.mod4, full.mod)


# check for collinearity

round(cor(Auto[,-c(1, 8, 9)]), 2)
pairs(Auto[,-c(1, 8, 9)])

# remove weight from full model

red.mod <- update(full.mod, ~.-weight)
summary(red.mod)


