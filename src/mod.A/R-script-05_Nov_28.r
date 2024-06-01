

##################################
# body temperature data
################################

temp.data <- read.table("normtemp.txt", head=TRUE)
attach(temp.data)
temp.C <- (temperature-32)*5/9


# check normality

hist(temp.C, prob=TRUE, col="lightgray")

qqnorm(temp.C)
qqline(temp.C)


#################################
# Conf. Int. for mu with sigma known
################################

# we assume X~N(mu, sigma^2) with sigma=0.45 known

x.bar <- mean(temp.C)
se <- 0.45/sqrt(length(temp.C))

# CI confidence level 95%

mu.lower <- x.bar - qnorm(0.975)*se
mu.upper <- x.bar + qnorm(0.975)*se

mu.lower
mu.upper

# more compact form

CI <- x.bar+c(-1, +1)*qnorm(0.975)*se
CI

# CI confidence level 1-alpha

alpha <- 0.05
# alpha <- 0.01

mu.lower <- x.bar - qnorm(1-alpha/2)*se
mu.upper <- x.bar + qnorm(1-alpha/2)*se

mu.lower
mu.upper


#################################
# Conf. Int. for mu with sigma unknown
################################

# we assume X~N(mu, sigma^2)

n <- length(temp.C)
x.bar <- mean(temp.C)
se <- sd(temp.C)/sqrt(n)

alpha <- 0.05

mu.lower <- x.bar - qt(1-alpha/2, df=n-1)*se
mu.upper <- x.bar + qt(1-alpha/2, df=n-1)*se

mu.lower
mu.upper

# use of the function t.test()

t.test(temp.C, conf.level=0.95)



#############################################
# R script form slides: interpretation of CIs
#############################################

# simulate 1000 samples of size 40 from a normal distribution
# and for every sample compute a confidence interval for the mean (sigma unknown)
# the level of the interval is 0.95 (i.e. 95%)

#
sampsize <- 40
mu.true <- 5
CI <- matrix(NA, ncol=2, nrow=1000)
for (i in 1:1000)
{
  x <- rnorm(sampsize, mu.true, 1)
  hat.mu <- mean(x)
  se <- sd(x)/sqrt(sampsize)
  mu.lower <- hat.mu - qt(0.975,sampsize-1)*se
  mu.upper <- hat.mu + qt(0.975,sampsize-1)*se
  CI[i,1] <- mu.lower
  CI[i,2] <- mu.upper
}

# compute the proportion of intervals that contain the true mean
# (should be close to the confidence level)
sum( (CI[,1] <= mu.true) & (CI[,2] >= mu.true) )/1000


# visual representation for the fist 100 intervals
#
plot(1:100,CI[1:100,1],ylim=range(CI), ylab=expression(hat(mu)), pch=20, col="blue")
points(1:100,CI[1:100,2], pch=20, col="red")
segments(1:100,CI[1:100,2],1:100,CI[1:100,1])
abline(h=mu.true,col=1, lwd=2)


###########################
# CI for body weight
###########################

library(MASS)
data("Animals")
attach(Animals)

# body is not normally distributed
qqnorm(body)
qqline(body)

t.test(body)

# log10(body) seems to be normally distributed
l.body <- log10(body)
qqnorm(l.body)
qqline(l.body)

# Conf. Int. for the mean of l.body

t.out <- t.test(l.body)
t.out$conf.int

# Conf. Int. for the geometric mean of body

10^t.out$conf.int


#####################################################################
# Exercises_01-Confidence intervals for the mean of a normal dist.pdf
#####################################################################


# Exercise 1
######################

#a
qt(0.95, df=11)
#b
qt(0.975, df=6)
#c
qt(0.995, df=1)
#d
qt(0.975, df=28)



# Exercise 3
######################

# a
alpha <- (1-pt(2.776, 4))*2

round(1-alpha, 3)

# b
alpha <- (1-pt(2.718, 11))*2
round(1-alpha, 3)



# c
alpha <- (1-pt(5.841, 3))*2
round(1-alpha, 3)

# d
alpha <- (1-pt(1.325, 20))*2
round(1-alpha, 3)

# e
alpha <- (1-pt(1.746, 16))*2
round(1-alpha, 3)


# Exercise 7
######################


sample <- c(204.999, 206.149, 202.102, 207.048, 203.496, 206.343, 203.496, 206.676, 205.831)

qqnorm(sample)
qqline(sample)

t.test(sample, conf.level = 0.95)


# Exercise 8
######################

n     <- 8
x.bar <- 3410.14
s     <- 1.018

#a
x.bar+c(-1,1)*qt(0.975, 7)*s/sqrt(n)

#b
x.bar+c(-1,1)*qt(0.99, 7)*s/sqrt(n)

#c
sample <- c(3409.76, 3409.80, 3412.66, 3409.79, 3409.76, 3409.77, 3409.80, 3409.78)
qqnorm(sample)
qqline(sample)


########################################
# Hypotheses testing
########################################

##############################
# test statistics and p-value
##############################


# null hypothesis        H_0: mu =  mu.0
# alternative hypothesis H_1: mu != mu.0

# value of mu.0 in the null hypothesis H_0
mu.0 <- 36.75


# the (unknown) true distribution is normal with
#################################################

# standard deviation
sigma <- 0.45

# and we consider two possible mean values

# a) case where H0 is true
mu.true <- 36.75

# case where H0 is not true
mu.true <- 36.30


# extract a sample
n <- 10
x <- rnorm(n, mean=mu.true, sd=sigma)

# compute the observed value of the test statistics
t.obs <- (mean(x)-mu.0)/(sd(x)/sqrt(n))
t.obs

# represent the distribution of t statistics under H0
curve(dt(x, df=9), xlim=c(-4, 4), ylab="", xlab="", main="distribution of t test under Ho")

# represent the observed value of t statistics
lines(x=c(-abs(t.obs), -abs(t.obs)), y=c(0, dt(-abs(t.obs), n-1)), lty=3, lwd=2, col="red")
lines(x=c(abs(t.obs), abs(t.obs)), y=c(0, dt(abs(t.obs), n-1)), lty=3, lwd=2, col="red")

# color tails corresponding to p.value

# right
x <- seq(abs(t.obs), 4, length=100)
y <- dt(x, df=n-1)
polygon(c(x, max(x), abs(t.obs)), c(y, 0, 0), col="yellow")

# area under the right tail
1-pt(abs(t.obs), n-1)

# left
x <- seq(-4, -abs(t.obs),  length=100)
y <- dt(x, df=n-1)
polygon(c(x,-abs(t.obs), min(x)), c(y, 0, 0), col="yellow")

# area under the left tail
pt(-abs(t.obs), n-1)

# compute p.value
p.value <- 2*pt(-abs(t.obs), df=n-1)
p.value



#############################################
# test H_0:mu=36.75 in body temperature data
#############################################


temp.data <- read.table("normtemp.txt", head=T)
attach(temp.data)
temp.C <- (temperature-32)*5/9

# check normality
qqnorm(temp.C)
qqline(temp.C)

# system of hypotheses

# H0: mu =  36.75
# H1: mu != 36.75

mu.0 <- 36.75

# define the decision rule

n <- length(temp.C)
significance.level <- 0.05
critical.value <- qt(0.975, df=n-1)
critical.value

# observed value of the t test statistic

x.bar <- mean(temp.C)
s <- sd(temp.C)

t.obs <- (x.bar-mu.0)/(s/sqrt(n))
t.obs

# is the empirical evidence in favor of H_1?

abs(t.obs) > critical.value

# visual representation

curve(dt(x, df=129), xlim=c(-6, 6))
lines(x=c(critical.value, critical.value), y=c(0, 0.2), lty=3, lwd=2, col="blue")
lines(x=c(-critical.value, -critical.value), y=c(0, 0.2), lty=3, lwd=2, col="blue")

lines(x=c(t.obs, t.obs), y=c(0, 0.2), lty=1, lwd=2, col="red")



# approach based on the p.value

# p.value

p.value <- pt(-abs(t.obs), df=129)*2
p.value

# is the empirical evidence in favor of H_1?

p.value < significance.level

# viusal representation

curve(dt(x, df=129), xlim=c(-6, 6))
lines(x=c(t.obs, t.obs), y=c(0, dt(t.obs, 129)), lty=2, lwd=2, col="red")
lines(x=c(-t.obs, -t.obs), y=c(0, dt(t.obs, 129)), lty=2, lwd=2, col="red")
x <- seq(t.obs, 4, length=100)
y <- dt(x, 120)
polygon(c(x, max(x), t.obs), c(y, 0, 0), col="yellow")
polygon(c(-x, -max(x), -t.obs), c(y, 0, 0), col="yellow")


# use the function t.test
##########################################

t.test(temp.C, mu=36.75, alt="")
help(t.test)

# possible alternative hypotheses
# alternative = c("two.sided", "less", "greater")

t.test(temp.C, mu=36.75, alt="t")
t.test(temp.C, mu=36.75, alt="l")
t.test(temp.C, mu=36.75, alt="g")

#####################################################################
# Exercises_02-t-test for the mean of a normal distribution.pdf
#####################################################################


# Exercise 3
######################

# a)

# H0: mu <= 5
# H1: mu > 5

# b)

# define the decision rule

significance.level <- 0.05
significance.level

# right-sided alternative hypothesis
n <- 8
critical.value <- qt(0.95, df=n-1)
critical.value


# compute the observed value of the
# test statistic

x.bar <- 6.5
s <- 1.9
mu.0 <- 5

t.obs <- (x.bar-mu.0)/(s/sqrt(n))
t.obs

# is the empirical evidence in favor of H_1?
t.obs > critical.value

# approach based on the p.value

p.value <- 1-pt(t.obs, n-1)
p.value

# is the empirical evidence in favor of H_1?

p.value < significance.level


# When the exact p.value is not available,
# a decision can be made by using the
# approximate p.value obtained from
# statistical tables.

t.obs
alpha <- c(0.80, 0.90, 0.95, 0.975, 0.99, 0.995)
qt(alpha, n-1)


# Exercise 4
######################

# a)

# H0: mu  = 23
# H1: mu != 23


# b)

# define the decision rule

significance.level <- 0.05
significance.level

# two-sided alternative hypothesis
n <- 10
critical.value <- qt(0.975, df=n-1)
critical.value

# compute the observed value of the
# test statistic


x.bar <- 23.2
s <- 0.2
mu.0 <- 23

t.obs <- (x.bar-mu.0)/(s/sqrt(n))
t.obs

# is the empirical evidence in favor of H_1?

abs(t.obs)> critical.value


# approach based on the p.value

p.value <- 2*pt(-abs(t.obs), n-1)
p.value

# is the empirical evidence in favor of H_1?

p.value < significance.level


# approximate p.value (if exact is not available)

t.obs
alpha <- c(0.80, 0.90, 0.95, 0.975, 0.99, 0.995)
qt(alpha, n-1)


# Exercise 5
######################


# a.1)

# H0: mu >= 10
# H1: mu <  10


# a.2)

# define the decision rule

significance.level <- 0.05
significance.level

# left-sided alternative hypothesis
n <- 20
critical.value <- qt(0.05, df=n-1)
critical.value

# compute the observed value of the
# test statistic

x.bar <- 6.7
s <- 3.9
mu.0 <- 10

t.obs <- (x.bar-mu.0)/(s/sqrt(n))
t.obs

# is the empirical evidence in favor of H_1?

t.obs< critical.value


# approach based on the p.value

p.value <- pt(t.obs, n-1)
p.value

# is the empirical evidence in favor of H_1?

p.value < significance.level

# approximate p.value (if exact is not available)

t.obs
alpha <- c(0.80, 0.90, 0.95, 0.975, 0.99, 0.995)
qt(alpha, n-1)



# b)

mu.0 <- 7.5

t.obs <- (x.bar-mu.0)/(s/sqrt(n))
t.obs

# is the empirical evidence in favor of H_1?

t.obs< critical.value


# approach based on the p.value

p.value <- pt(t.obs, n-1)
p.value

# is the empirical evidence in favor of H_1?

p.value < significance.level

# approximate p.value (if exact is not available)

t.obs
alpha <- c(0.80, 0.90, 0.95, 0.975, 0.99, 0.995)
qt(alpha, n-1)


# Exercise 6
######################


# H_1:


# a)

# H0: mu  = 3.5
# H1: mu  > 3.5


# b)

# define the decision rule

significance.level <- 0.05
significance.level

# right-sided alternative hypothesis

n <- 6
critical.value <- qt(0.95, df=n-1)
critical.value

# compute the observed value of the
# test statistic

sample <- c(3.45, 3.47, 3.57, 3.52, 3.40, 3.63)

n <- length(sample)
n

x.bar <- mean(sample)
x.bar

s <- sd(sample)
s

mu.0 <- 3.5

t.obs <- (x.bar-mu.0)/(s/sqrt(n))
t.obs

p.value <- 1-pt(t.obs, n-1)
p.value

t.test(sample, mu=3.5, alternative = "g")


# Exercise 8
######################

# a)

# H0: mu  <= 85
# H1: mu  > 85


# b)

# define the decision rule

significance.level <- 0.05
significance.level

# right-sided alternative hypothesis

n <- 6
critical.value <- qt(0.95, df=n-1)
critical.value

# compute the observed value of the
# test statistic


sample <- c(93.2, 87.0, 92.1, 90.1, 87.3, 93.6)

n <- length(sample)
n

x.bar <- mean(sample)
x.bar

s <- sd(sample)
s

mu.0 <- 85


t.obs <- (x.bar-mu.0)/(s/sqrt(n))
t.obs

p.value <- 1-pt(t.obs, n-1)
p.value

t.test(sample, mu=85, alternative = "g")



#############################
# INFERENCE FOR A PROPORTION
#############################


# vitamin C example
#############################

x <- 302
n <- 407

p.hat <- x/n
p.hat

se.hat <- sqrt(p.hat*(1-p.hat)/n)
se.hat

ic.upper <- p.hat + 1.96*se.hat
ic.lower <- p.hat - 1.96*se.hat
round(c(ic.lower, ic.upper), 3)

# R-function for exact CI
binom.test(x, n, conf.level=0.95)

# R-function for approximated CI
prop.test(x, n, conf.level=0.95)



# quality control example
#########################

n <- 40
x <- 11

p.hat <- x/n

p.0 <- 0.25


# Note that the standard
# error under the null hypothesis is
# computed using p.0 rather than p.hat
#
se.0 <- sqrt(p.0*(1-p.0)/n)

z <- (p.hat-p.0)/se.0
z
p.value <- 1-pnorm(z)
p.value


# R-function for exact test
binom.test(x, n, p=0.25, alternative="greater")

# R-function for approximated CI
prop.test(x, n, p=0.25, alternative="greater")


# mean of exponential
######################

x <- c(6, 0, 1, 7, 3, 5, 2, 1)
n <- length(x)
n

x.bar <- mean(x)
theta.hat <- x.bar

se.hat <- sqrt(theta.hat^2/n)


ic.upper <- theta.hat + 1.96*se.hat
ic.lower <- theta.hat - 1.96*se.hat
round(c(ic.lower, ic.upper), 3)

# a confindence interval for lambda=1/theta
# is given by

round(c(1/ic.upper, 1/ic.lower), 3)
