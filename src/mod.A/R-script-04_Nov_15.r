#####################
# sampling error
#####################
#
# We assume hat the distribution of the population
# (i.e. of the random variable X)  is known
# to be normal with mu=36.75 and sigma=0.45
#

mu <- 36.75
sigma <- 0.45

# we use the sample mean to estimate mu
# and compute the sampling error
#

# sample size and sample
samplesize <- 4
x <- rnorm(samplesize, mean=mu, sd=sigma)

# estimate
mu.hat <- mean(x)
mu.hat

# sampling error
sampling.error <- mu.hat-mu
sampling.error

#########################################
# sampling distribution of a statistic
#########################################
#
#
# As above, we assume that the distribution of
# the population variable X is known
# to be normal with mu=36.75 and sigma=0.45
#

mu <- 36.75
sigma <- 0.45

#################################################
# We obtain through simulations the sampling
# distribution of the mean as follows:
# 1. we extract 10^4 sample of size n=5
# 2. for each sample compute the arithmetic mean
# 3. represent the histogram of the 10^4 means
# 4. add a (dashed) vertical line corresponding to mu

samplesize <- 5 # sample size n

all.samples <- c()
for (i in 1:10000){
  x <- rnorm(samplesize, mean=mu, sd=sigma)
  all.samples <- c(all.samples, mean(x))
}

hist.title <- "sample mean"
hist(all.samples, freq=FALSE, xlab="", ylab="",col="lightgray", main=hist.title)
abline(v=mu, col="red", lty=2, lwd=3)

##################################
#
# We can do the same for any sampling statistic
#


#################################################
# parameter: population mean
# statistic: sample mean

hist.title <- "sample mean"
parameter.value <- mu
statistic.value <- function(x) mean(x)


#################################################
# parameter: population median
# statistic: sample median

hist.title <- "sample median"
parameter.value <- mu
statistic.value <- function(x) median(x)


#################################################
# parameter: population third quartile
# statistic: sample  third quartile

hist.title <- "sample third quartile"
parameter.value <- qnorm(0.75, mean=mu, sd=sigma)
statistic.value <- function(x) quantile(x, 0.75)


#################################################
# parameter: population standard deviation
# statistic: sample  standard deviation

hist.title <- "sample variance"
parameter.value <- sigma^2
statistic.value <- function(x) var(x)

#################################################
# parameter: population IQR
# statistic: sample  IQR

hist.title <- "sample IQR"
parameter.value <- qnorm(0.75, mean=mu, sd=sigma)-qnorm(0.25, mean=mu, sd=sigma)
statistic.value <- function(x) IQR(x)

#################################################
# We obtain through simulations the sampling
# distribution of the a generic statistic
# implemented in the function statistic.value()
# as follows:
# 1. we extract 10^4 sample of size n=5
# 2. for each sample compute statistic.value()
# 3. represent the histogram of the 10^4 statistic.values
# 4. add a (dashed) vertical line corresponding to
#    relevant the population parameter



#################################################
# empirical distribution based on 10000 samples
#
samplesize <- 5 # sample size n
all.samples <- c()
for (i in 1:10000){
  x <- rnorm(samplesize, mean=mu, sd=sigma)
  all.samples <- c(all.samples, statistic.value(x))
}

hist(all.samples, freq=FALSE, xlab="", ylab="", col="lightgray", main=hist.title)
abline(v=parameter.value, col="red", lty=2, lwd=3)



###########################################################
# sampling distribution of the mean for normal populations
###########################################################

# consider again the sampling distribution of the mean

hist.title <- "sample mean"
parameter.value <- mu
statistic.value <- function(x) mean(x)

samplesize <- 5 # sample size n
all.samples <- c()
for (i in 1:10000){
  x <- rnorm(samplesize, mean=mu, sd=sigma)
  all.samples <- c(all.samples, statistic.value(x))
}

hist(all.samples, freq=FALSE, xlab="", ylab="", col="lightgray", main=hist.title)
abline(v=parameter.value, col="red", lty=2, lwd=3)

# In the case where the population follows a normal distribution and
# the sample is i.i.d. the distribution of the sample mean is known to
# be normally distributed

curve(dnorm(x, mean=mu, sd=sigma/sqrt(samplesize)), add=TRUE, lwd=2, col="blue")




#######################################################
# standard error and comparison of different estimators
#######################################################

# as above we assume the population normally distributed

mu <- 36.75
sigma <- 0.45

# we consider three different estimators of mu:
#
# mu.hat.1 = sample mean
# mu.hat.2 = sample median
# mu.hat.3 = midpoint of the range of the sample
#
# hence simulate the sampling distributions of these three
# estimators based on 10^4 samples with n=16

samplesize <- 16

mu.hat.1 <- c()
mu.hat.2 <- c()
mu.hat.3 <- c()

for (i in 1:10000) {
  x <- rnorm(samplesize, mean=mu, sd=sigma)
  mu.hat.1 <- c(mu.hat.1, mean(x))
  mu.hat.2 <- c(mu.hat.2, median(x))
  mu.hat.3 <- c(mu.hat.3, mean(range(x)))
}

par(mfrow=c(2,2))
hist(mu.hat.1, main=expression(hat(mu)[1]~(mean~of~values)), freq=F, col="cyan", ylab="")
hist(mu.hat.2, main=expression(hat(mu)[2]~(median~of~values)), freq=F, col="cyan", ylab="")
hist(mu.hat.3, main=expression(hat(mu)[3]~(mean~of~range)), freq=F, col="cyan", ylab="")
d1 <- density(mu.hat.1)
d2 <- density(mu.hat.2)
d3 <- density(mu.hat.3)
plot(d1, lwd=2, main="", xlim=range(mu.hat.3), xlab="", ylab="")
abline(v=mu, lty=2)
lines(d2, lwd=2, col="blue")
lines(d3, lwd=2, col="red")
par(mfrow=c(1,1))


# comparison of empirical distributions

plot(d1, lwd=2, main="", xlim=range(mu.hat.3), xlab="", ylab="")
legend("topleft", lty=c(1,1,1), col=c("black", "blue", "red"), cex=0.75, legend=c("mean", "median", "mid-range"))
abline(v=mu, lty=2)
lines(d2, lwd=2, col="blue")
lines(d3, lwd=2, col="red")

# compute the mean and the standard deviations
# (i.e. the standard errors) of the estimators

mean(mu.hat.1)
sd(mu.hat.1)

mean(mu.hat.2)
sd(mu.hat.2)

mean(mu.hat.3)
sd(mu.hat.3)


################################################################
# sampling distribution of the variance for normal populations
################################################################

samplesize <- 10
s2 <- c()
for (i in 1:10000) s2 <- c(s2, var(rnorm(samplesize, mu, sigma)))

hist(s2, freq=FALSE)
var(s2)


y <- (samplesize-1)*s2/(sigma^2)

hist(y, freq=FALSE)
curve(dchisq(x, samplesize-1), add=TRUE, lwd=2, col="red")

