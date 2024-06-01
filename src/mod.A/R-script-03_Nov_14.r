#######################################
#    PROBABILITY DISTRIBUTIONS
# -- continuous Random Variables --
#######################################

# uniform distribution
#######################


# density function
dunif(0.2, 0, 1)

# plot the density function
curve(dunif, -1, 2, ylab="f(x)", ylim=c(0,1.5))
curve(dunif(x, min=-0.5, max=1.5), -1, 2, ylab="f(x)", ylim=c(0,1.5))

# cumulative distribution function
punif(0.2, 0, 1)

# plot the cumulative distribution function
curve(punif(x, min=-0.5, max=1.5),-1, 2, ylab="F(x)", ylim=c(0,1.5))
abline(h=1, lty=3)

# uniformly distributed random values
runif(3, -0.5, 1.5)

# quantiles of the uniform distribution
qunif(.3, -0.5, 1.5)


# how to see quantiles on the cumulative distribution function

pr <- 0.55
qunif(pr, -0.5, 1.5)

curve(punif(x, min=-0.5, max=1.5), -1, 2, ylab="F(x)", ylim=c(0,1.5))
abline(h=pr, v=qunif(pr, -0.5, 1.5), lty=3, col="green")


# exponential distribution
###########################

f <- function(x, lambda=1) lambda*exp(-lambda*x)

f(0.5)
f(0.5, lambda=1/3)

# or equivalently

dexp(0.5)
dexp(0.5, rate=1/3)

# numerical integration
# check the area under the curve is equal to 1

integrate(f, 0, Inf)

# plot the pdf

curve(f, from=0, to=6)
curve(f(x, 1/3), 0, 6)


# plot the cdf

curve(pexp, 0, 6)
curve(pexp(x, rate=1/3), 0, 6)

# quantiles of the exponential distribution


pr <- 0.5
qexp(pr, rate=1/3)
curve(pexp(x, rate=1/3), 0, 8, ylim=c(0, 1.1))
abline(h=pr, v=qexp(pr, rate=1/3), col="red",lty=3)

# E(X)

f <- function(y) y*exp(-y)
integrate(f, 0, Inf)

# E(Y^2)

f <- function(y) y^2*exp(-y)
integrate(f, 0, Inf)

# variance

f <- function(y) (y-1)^2*exp(-y)
integrate(f, 0, Inf)

# comparison of exponential densities

x <- seq(0, 2, length=40)
theta <- c(2, 1, .2, .1) # mean of distribution
y <- matrix(NA, 40, 4)
for (i in 1:4) {
  y[,i] <- dexp(x, 1/theta[i]) # parameter is the rate
}
matplot(x, y, type="l", xlab="x", ylab="p(x)", lty=1:4, col=1)
legend(1.2, 10, paste("theta =", theta), lty=1:4, cex=.75)



####################################
# normal distribution
###################################


# distribution for different values of mu and sigma

# some pdf's

curve(dnorm,-8,8)
curve(dnorm(x, mean=0, sd=2),add=TRUE, col=2)
curve(dnorm(x, mean=0, sd=4),add=TRUE, col=3)

curve(dnorm, -8, 8)
curve(dnorm(x, -2, 1), add=TRUE, col=2)
curve(dnorm(x,  2, 2), add=TRUE, col=3)


# and corresponding cdf's

curve(pnorm,-8,8)
curve(pnorm(x,0,2),add=TRUE, col=2)
curve(pnorm(x,0,4),add=TRUE, col=3)

# quantiles and pdf

par(mfrow=c(1,2))
q.15 <- qnorm(0.15)
curve(dnorm,-3,3)
abline(v=q.15, lty=2, col="blue")
pnorm(q.15)


q.60 <- qnorm(0.60)
abline(v=q.60, lty=2, col="red")
pnorm(q.60)

q.95 <- qnorm(0.95)
abline(v=q.95, lty=2, col="green")
pnorm(q.95)

# quantiles and cdf

curve(pnorm,-3,3)
lines(x=c(-3, q.15), y=c(0.15, 0.15), lty=2, col="blue")
lines(x=c(q.15, q.15), y=c(0, 0.15), lty=2, col="blue")

lines(x=c(-3, q.60), y=c(0.60, 0.60), lty=2, col="red")
lines(x=c(q.60, q.60), y=c(0, 0.60), lty=2, col="red")

lines(x=c(-3, q.95), y=c(0.95, 0.95), lty=2, col="green")
lines(x=c(q.95, q.95), y=c(0, 0.95), lty=2, col="green")

par(mfrow=c(1,1))

# empirical pdf  (function ecdf())

set.seed(123)
x <- rexp(4)
plot(ecdf(x), xlim=c(0,1.75), ylim=c(0, 1.1), axes=FALSE)
box()
axis(2, at=c(0, 0.25, 0.5, 0.75, 1))
axis(1, at=round(x, 3))

# exponential distribution
# comparison between epdf and "exact" pdf

x <- rexp(30)
plot(ecdf(x), xlim=c(0,4))
curve(pexp, add=TRUE, col="blue")

# normal distribution
# comparison between epdf and "exact" pdf

x <- rnorm(30)
plot(ecdf(x), xlim=c(-3,3))
curve(pnorm, add=TRUE, col="blue")

# comparison between ecdf from exponential
# and pdf from standard normal

x <- rexp(30)
plot(ecdf(x), xlim=c(-3,3))
curve(pnorm, add=TRUE, col="blue")



#############################
# Normal QQPLOT
#############################

# empirical cdf

par(mfrow=c(1,2))

x <- rexp(30)
plot(ecdf(x), xlim=c(-3,3))
curve(pnorm, add=TRUE, col="blue")


qqnorm(x)
#abline(0, 1)
qqline(x)
par(mfrow=c(1,1))

# comparison with different cases

# right skewed

x <- rexp(50)

par(mfrow=c(1,2))
hist(x, freq=FALSE, col="cyan")
qqnorm(x)
qqline(x)
par(mfrow=c(1,1))

# left skewed

x <- -rexp(50)

par(mfrow=c(1,2))
hist(x, freq=FALSE, col="cyan")
qqnorm(x)
qqline(x)
par(mfrow=c(1,1))


# light tails

x <- runif(30)


par(mfrow=c(1,2))
hist(x, freq=FALSE, col="cyan")
qqnorm(x)
qqline(x)
par(mfrow=c(1,1))

# heavy tails

x <- rt(50, 2)

par(mfrow=c(1,2))
hist(x, freq=FALSE, col="cyan")
qqnorm(x)
qqline(x)
par(mfrow=c(1,1))

# data animals and z.scores

library(MASS)
data("Animals")
attach(Animals)

l.body <- log10(body)


par(mfrow=c(1, 2))
qqnorm(body)
qqline(body)
qqnorm(l.body)
qqline(l.body)
par(mfrow=c(1,1))

# faithful gayser data and bimodality

data(faithful)
attach(faithful)

par(mfrow=c(1,2))
hist(waiting, freq=FALSE, col="cyan")
qqnorm(waiting)
qqline(waiting)
par(mfrow=c(1,1))

################################
# Loading data into R
################################

temp.data <- read.table("normtemp.txt", head=TRUE)
attach(temp.data)
temp.C <- (temperature-32)*5/9

# body temperatures and comparison with normal distribution

x <- rnorm(20)
qqnorm(x)
qqline(x)

normtemp <- read.table("normtemp.txt", head=TRUE)
attach(normtemp)
temp.C <- (temperature-32)*5/9


par(mfrow=c(1, 2))
qqnorm(temp.C, main="body temperature")
qqline(temp.C)

x <- rnorm(20)
qqnorm(x, main="normal data")
qqline(x)

par(mfrow=c(1,1))

# missing values
#
# default na.strings = "NA"
mydata <- read.table("normtemp-with-NA.txt", head=TRUE)

mydata$temperature[1:10]
is.vector(mydata$temperature)
is.character(mydata$temperature)
is.numeric(mydata$temperature)

# set na.strings="*"
mydata <- read.table("normtemp-with-NA.txt", head=TRUE, na.strings = "*")

mydata$temperature[1:10]
is.vector(mydata$temperature)
is.character(mydata$temperature)
is.numeric(mydata$temperature)

# factors (i.e. data type for categorical variables)
#

is.vector(mydata$gender)
is.character(mydata$gender)
is.factor(mydata$gender)

# define a factor
gender.f <- as.factor(mydata$gender)
is.factor(gender.f)

# different method  (i.e. behavior) for printing character
# vectors and factors
mydata$gender[1:10]
gender.f[1:10]

# different method (i.e. behavior) for summary() of
# character vectors and factors
summary(mydata$gender)
summary(gender.f)

# modify directly the attributes of the data.frame
mydata$gender <- as.factor(mydata$gender)

# ordered factor

mydata <- read.table("normtemp-with-ordinal-var.txt", head=TRUE, comment.char = "#")

is.character(mydata$age)
is.factor(mydata$age)

# convert into a factor (categorical variable)
age.f <- factor(mydata$age)
is.vector(age.f)
is.factor(age.f)

# check whether it is recognized as ordered
is.ordered(age.f)

# convert into an ordered factor
age.fo <- factor(mydata$age, ordered=TRUE, levels=  c("<30", "[30, 50)", "[50, 70)", ">=70" ))

is.ordered(age.fo)
summary(age.f)
summary(age.fo)

# change directly the data.frame
mydata$age <- factor(mydata$age, ordered=TRUE, levels=  c("<30", "[30, 50)", "[50, 70)", ">=70" ))


######################################################
# Probability distributions related to the normal one
######################################################

# chi-squared distribution generate simulated values

# generate a value from a chi-square distribution
# with 5 degrees of freedom

# applying the definition

df <- 5
x <- rnorm(df)
sum(x^2)

# built-in R function

rchisq(1, df=5)

# generate n=10000 independent observations
# from a chi-square distribution with
# 5 degrees of freedom

df <- 5 # degrees of freedom
n <- 10000 # sample size
sample <- c()
for (i in 1:n){
  x <- rnorm(df)
  y <- sum(x^2)
  sample <- c(sample, y)
}

# compare the histogram of the generated observations
# with the density function of the chi-square distribution

hist(sample, col="lightgray", freq=FALSE, main = "sample from chi-squared distribution")
curve(dchisq(x, df), add=TRUE, lwd=2, col="blue")

# equivalently using the rchisq() function
sample <- rchisq(n, df)
hist(sample, col="lightgray", freq=FALSE, main = "sample from chi-squared distribution")
curve(dchisq(x, df), add=TRUE, lwd=2, col="blue")

# student's t distribution

# density function

# 1 degree of freedom
curve(dt(x, 1),  xlim=c(-5, 5), ylab="pdf", xlab="", col="blue", lwd=1.5)

# 10 degrees of freedom
curve(dt(x, 10),  xlim=c(-5, 5), ylab="pdf", xlab="", col="blue", lwd=1.5)


# comparison with the normal distribution

curve(dnorm, xlim=c(-5, 5), ylab="pdf", xlab="", col="red", lwd=1.5)
curve(dt(x, 1), add=TRUE, lty=2, col="blue", lwd=1.5)
curve(dt(x, 4), add=TRUE, lty=3, col="black", lwd=1.5)

# compare densities
x <- seq(-5, 5, length=100)
Y <- cbind(dt(x,1), dt(x,4), dt(x,16), dt(x,64), dnorm(x))
matplot (x, Y, type="l", ylab="density", xlab="t", lty=c(2:5,1), col=c(4,1,1,1,2), lwd=1.5)

# add a legend
leg.label <- c("t df = 1", "t df = 4", "t df = 16", "t df = 64", "Normal" )
leg.lty   <- c(2, 3, 4, 5, 1)
leg.col <- c("blue", "black", "black", "black", "red")
# or equivalently: leg.col <- c(4,1,1,1,2)
legend (x="topleft", y=NULL, legend=leg.label, lty=leg.lty, col=leg.col, lwd=1.5, cex=.7)


# compare boxplots
x <- rnorm(1000)
y <- rt(1000, 5)

boxplot(x, y, names=c("normal dist.", "t dist."), col="lightgray")


###########################################################
# Linear conbination of rv's and the central limit theorem
###########################################################


# linear combination of iid rv's with exponential distributions

# X is exp(1)
# Y is the mean of n exp(1) iid rv's

n <- 3
y <- mean(rexp(n, rate=1)) # rate is lambda
y

# generate 10000 values from Y for different values of n
# and compare the distribution of Y with that of X
n <- 3
#n <- 10
n <- 50

y <- c()
for(i in 1:10000) y <- c(y, mean(rexp(n, 1)))

hist(y, breaks=20, freq=FALSE)
curve(dexp(x, 1), from=0.001, add=TRUE, col="blue", lwd=2)

# check empirically that E(Y)=E(X) where E(X)=1

mean(y)

# check empirically that Var(Y)=Var(X)/n where Var(X)=1

1/n
var(y)


# linear combination of iid normally distributed rv's


# X is N(1, 1)
# Y is the mean of n N(1,1) iid rv's


# generate 10000 values from Y for n=4
# and compare the distribution of Y with that of X

n <- 4

y <- c()
for(i in 1:10000) y <- c(y, mean(rnorm(n, mean=1, sd=1)))

hist(y, breaks=20, freq=FALSE, main="", xlim=c(-1.5, 3.5))

# normal density with mean=1 and variance=1
curve(dnorm(x, mean=1, sd=1), add=TRUE, col="red", lwd=1.5)


# check empirically that E(Y)=E(X) where E(X)=1

mean(y)

# check empirically that Var(Y)=Var(X)/n where Var(X)=1

1/n
var(y)

# normal density with mean=1 and variance=1/n so that sd=1/sqrt(n)
curve(dnorm(x, mean=1, sd=1/sqrt(n)), add=TRUE, col="blue", lwd=1.5)

# distribution of the mean for different sample sizes

samplingdist <- function(n) {
  curve(dnorm(x,5,1), xlim=c(2,8), ylim=c(0,2), col=2, lwd=1.5, ylab="")
  curve(dnorm(x,5,1/sqrt(n)), xlim=c(2,8), ylim=c(0,2), lwd=1.5,add=TRUE)
  Sys.sleep(0.2)}

# apply for a single value of n
samplingdist(5)

# apply for 20 values of n
ignore <- sapply(1:20, samplingdist)


# Central Limit Theorem

# empirical check with exponential distribution

# X is exp(1)
# Y is the mean of n exp(1) iid rv's

# generate 10000 values from Y for different values of n
# and compare the distribution of Y with that
# of the asymptotic normal distribution

n <- 2
#n <- 3
#n <- 6
#n <- 10
#n <- 50
#n <- 100

y <- c()
for(i in 1:10000) y <- c(y, mean(rexp(n, 1)))

hist(y, breaks=20, freq=FALSE, ylab="", main=paste("n =", n))

mu.y <- 1
sigma.y <- 1/sqrt(n)
curve(dnorm(x, mean=mu.y, sd=sigma.y), from=0.001, add=TRUE, col="blue", lwd=2)


###############################################################
# the Central Limit Theorem: Exercises
# (from file Exercises_01-The central limit theorem.pdf)
###############################################################


# EXERCISE n.1
###############

mu    <- 12.01
sigma <- 0.2

# mean of n=144 bottles

n <- 144
var.x.bar <- sigma^2/n
sd.x.bar  <- sqrt(var.x.bar)

pnorm(12, mean=mu, sd=sd.x.bar)


# EXERCISE n.2
###############

mu    <- 0.08
sigma <- 0.01

n <- 250

mu.y  <- n*mu
var.y <- n*sigma^2
sd.y  <- sqrt(var.y)

# a)

1-pnorm(20.2, mean=mu.y, sd=sd.y)

# b)

qnorm(0.1, mean=mu.y, sd=sd.y)
qnorm(0.9, mean=mu.y, sd=sd.y)

# c)

# There is not enough information:
# the distribution of page thickness in
# unknown

# EXERCISE n.10
###############

n <- 100
pr <- 0.3

# exact probability form Binomial distribution

1 - pbinom(35, n, pr)

# approximate probability using the CLT

mu.y <- n*pr
sigma2.y <- n*pr*(1-pr)

1 - pnorm(35, mean=mu.y, sd=sqrt(sigma2.y))


# compare the exact Binomial pdf with the
# approximating Normal pdf

x <- 0:100
pr.x <- dbinom(x, n, pr)
plot(x, pr.x, type="h", xlab="", ylab="", main="")
curve(dnorm(x,  mean=mu.y, sd=sqrt(sigma2.y)), add=TRUE, lwd=2, lty=2, col="red")



# EXERCISE n.3
###############

x <- 0:4
pr <- c(0.1, 0.3, 0.3, 0.2, 0.1)

# expected value of X
mu <- sum(x*pr)
mu

# variance of X
sigma2 <- sum((x-mu)^2*pr)
sigma2

# mean of 100 days

n <- 100
var.x.bar <- sigma2/n
sd.x.bar  <- sqrt(var.x.bar)
1-pnorm(2, mean = mu, sd = sd.x.bar)



# compute an approximate distribution of X.bar
# using simulations

# randomly generate one observation from X.bar

urn <- c(0, 1,1,1,2,2,2,3,3,4)
one.sample <- sample(urn, size=n, replace=TRUE)

one.sample[1:10]

mean(one.sample)

# repeat a large number of times (N times)

set.seed(123)
N <- 10^4
N.sample.means <- c()
for (i in 1:N){
  new.sample <- sample(urn, size=100, replace = TRUE)
  new.mean <- mean(new.sample)
  N.sample.means <- c(N.sample.means, new.mean)
}


# compare the kernel density estimation of the
# distribution of the mean
# with the approximating normal distribution

dens <- density(N.sample.means)
plot(dens, lwd=1.5, main="", ylim=c(0, 3.8))
curve(dnorm(x, mean = mu, sd = sqrt(sigma2/100)), add=TRUE, lwd=2, col="red", lty=2)

# approximated probability computed from simulations
# to be compared with the approximation from the CLT
sum(N.sample.means>2)/N


# EXERCISE n.9
################

q.95 <- qnorm(0.95)
q.95

n <- (q.95*0.5/0.1)^2
n





