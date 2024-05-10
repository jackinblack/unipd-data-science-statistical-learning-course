#######################
# MUON DECAY EXAMPLE
#######################

muon.data <- c(0.41040018,  0.91061564, -0.61106896,  0.39736684,  0.37997637, 0.34565436,
               0.01906680, -0.28765977, -0.33169289,  0.99989810, -0.35203164, 0.10360470,
               0.30573300,  0.75283842, -0.33736278, -0.91455101, -0.76222116, 0.27150040,
               -0.01257456,  0.68492778, -0.72343908,  0.45530570,  0.86249107, 0.52578673,
               0.14145264,  0.76645754, -0.65536275,  0.12497668,  0.74971197, 0.53839119)

#
# function for density of muon decay
#

dmuon <- function(x, alpha){
  d <- (1+alpha*x)/2
  return(d)
}

# compute dmuon() for some values of x
x <- muon.data[1]
x
dmuon(x, alpha=0.7)

# this function works also if x is a vector
x <- muon.data[1:4]
dmuon(x, alpha=0.7)

# plot the density for alpha=0.7
curve(dmuon(x, alpha=0.7), from=-1, to=1, ylab=expression(p(x)), main="Density for muon decay")

#
# function for log-likelihood
#
lmuon <- function(alpha, data){
  l <- sum(log(1+alpha*data))-length(data)*log(2)
  return(l)
}

# compute for some value of alpha

lmuon(-0.4, data=muon.data)


# this function doesn't work as expected
# if one tries to use  it with a vector
# of values or plot a curve.

alpha <- c(-0.4, 0.2, 0.7)
lmuon(alpha, data=muon.data)

curve(lmuon(x, data=muon.data), -1, 1)

# we need to "vectorize" the function

lmuon.v <- function(alpha, data){
  l <- c()
  for (a in alpha) l <- c(l, lmuon(a, data))
  return(l)
}

lmuon(-0.4, data=muon.data)
alpha <- c(-0.4, 0.2, 0.7)
lmuon.v(alpha, data=muon.data)

xl <- expression(alpha)
yl <- expression(l(alpha))
mt <- "log-likelihood for muon decay"

curve(lmuon.v(x, data=muon.data), from=-1, to=1, xlab=xl, ylab=yl, main=mt)

# "Vectorize" function

lmuon.v <- Vectorize(FUN=lmuon, vectorize.args = "alpha")
lmuon.v

lmuon.v(alpha, data=muon.data)
curve(lmuon.v(x, data=muon.data), from=-1, to=1, xlab=xl, ylab=yl, main=mt)


# score function

smuon <- function(alpha, data){
  l <- sum(data/(1+alpha*data))
  return(l)
}

smuon.v <- Vectorize(FUN=smuon, vectorize.args = "alpha")
curve(smuon.v(x, data=muon.data), -0.8, 1, xlab=xl)
abline(h=0, lty=2)



#########################
# bisection method
#########################

# finding roots by applying the bisection method

# note that this function uses muon.data,
# that is an external object not given as an
# argument. This should be avoided.


bsec.root.muon <- function(lower, upper, tol=10e-7){
  while(upper-lower>tol){
    mid <- (upper+lower)/2
    if (sign(smuon(mid, data=muon.data))==sign(smuon(lower, data=muon.data))){
      lower <- mid
    }else{
      upper <- mid
    }
  }
  return(mid)
}

bsec.root.muon(lower=-1, upper=1)

options(digits = 15)
bsec.root.muon(lower=-1, upper=1)

bsec.root.muon(lower=-1, upper=1, tol=10e-15)

options(digits=7)

# more general with function as argument and "three dots"

bsec.root <- function(FUN, lower, upper, tol=10e-7,...){
  while(upper-lower>tol){
    mid <- (upper+lower)/2
    if (sign(FUN(mid, ...))==sign(FUN(lower, ...))){
      lower <- mid
    }else{
      upper <- mid
    }
  }
  return(mid)
}

bsec.root(FUN=smuon, lower=-1, upper=1, data=muon.data)



# some examples

f  <- function(x) log(x)
f  <- function(x) log(x) - exp(-x)
f  <- function(x) cos(x)-2*x
f  <- function(x) x^3-x-1

curve(f, -10, 10, lwd=2, col="blue")
abline(h=0, lty=2)

r <- bsec.root(FUN=f, 0, 2)
r
f(r)

# function for bisection with additional arguments

bsec.root2 <- function(FUN, lower, upper, tol = 10e-07, max.iter = 1000, ...){
  if (FUN(lower, ...)*FUN(upper, ...)>0) stop("the endpoints of the interval don't have opposite sign")
  N <- 0
  while(upper-lower>tol){
    mid <- (upper+lower)/2
    if (sign(FUN(mid, ...))==sign(FUN(lower, ...))){
      lower <- mid
    }else{
      upper <- mid
    }
    N <- N+1
    if (N > max.iter) stop("max number of steps exceeded")
  }

  return(c(root=mid, n.iter= N))
}

bsec.root2(FUN=smuon, -1, 0, data=muon.data)
bsec.root2(FUN=smuon, -1, 1, data=muon.data, tol=10e-15)
bsec.root2(FUN=smuon, -1, 1, data=muon.data, tol=10e-20)


#################################
#  Newton-Raphson method
#################################

# second derivative of log-likelihood for muon decay

d.smuon <- function(alpha, data){
  sprime <- -sum(data^2/(1+alpha*data)^2)
  return(sprime)
}

# Newton-Raphson for muon decay

nr.muon <- function(start, tol=10e-07){
  diff <- 1
  x.new <- start
  while(abs(diff) > tol){
    x.old <- x.new
    x.new <- x.old - smuon(x.old, data=muon.data)/d.smuon(x.old, data=muon.data)
    diff  <- x.new - x.old
  }
  return(x.new)
}


nr.muon(0.5)

# Newton-Raphson more general version

nr.root <- function(start, FUN, d.FUN, tol=10e-07, ...){
  diff <- 1
  x.new <- start
  while(abs(diff) > tol){
    x.old <- x.new
    x.new <- x.old - FUN(x.old, ...)/d.FUN(x.old, ...)
    diff  <- x.new - x.old
  }
  return(x.new)
}

nr.root(0.5, smuon, d.smuon, data=muon.data)


# some examples

f  <- function(x) log(x)
fp <- function(x) 1/x


f  <- function(x) log(x) - exp(-x)
fp <- function(x) 1/x + exp(-x)

f  <- function(x)  cos(x)-2*x
fp <- function(x) -sin(x) -2


f <- function(x) x^3-x-1
fp <- function(x) 3*x^2-1

curve(f, -10, 10, lwd=2, col="blue")
abline(h=0, lty=2)

r <- nr.root(.5, FUN=f, d.FUN=fp)
r
f(r)


##########################################
# built-in functions for root/optimization
##########################################

# The function uniroot() searches the interval from
# lower to upper for a root (i.e., zero)

uniroot(f=smuon, data=muon.data, lower=-1, upper=1)

uniroot(f=smuon, data=muon.data, interval=c(-1,1))


# optimization functions that by default perform minimization


minus.lmuon <- function(alpha, data) -lmuon(alpha, data)

nlminb(start=0.6, objective=minus.lmuon, data=muon.data, lower=-1, upper=1)

optim(0.6, fn=minus.lmuon, data=muon.data, lower=-1, upper=1)
optim(0.6, fn=minus.lmuon, method="Brent",data=muon.data, lower=-1, upper=1)

##########################################
# Muon decay - asymptotic distribution MLE
##########################################

# recall the log-likelihood function
lmuon <- function(alpha, data){
  l <- sum(log(1+alpha*data))-length(data)*log(2)
  return(l)
}

minus.lmuon <- function(alpha, data) -lmuon(alpha, data)

# compute the MLE alpha.hat of alpha
alpha.hat <- nlminb(0.6, minus.lmuon, data=muon.data, lower=-1, upper=1)$par

# compute the observed information at alpha.hat
j <- sum(muon.data^2/(1+alpha.hat*muon.data)^2)
j

# asymptotic approximation of the standard error
se <- sqrt(1/j)
se

# approximate 95% confidence interval

alpha.hat +c(-1, 1)*1.96*se

# is the distribution uniform over the interval (-1;1)?
#
# H_0: alpha=0
# H_1: alpha!=0

t.obs <- (alpha.hat-0)/se
p.value <- 2*pnorm(-abs(t.obs))
p.value


# numerical derivative

library(numDeriv)

H <- hessian(func = lmuon, x = alpha.hat, data = muon.data)
j <- -H[1,1]
sqrt(1/j)

##########################################
#
# Example:  survival times for leukemia
#
##########################################
#

# data
x <- c(56,65,17,7,16,22,3,4,2,3,8,4,3,30,4,43)

##################################################
# computation of MLEs as done on the slides

score <- function(beta, data)
{
  n <- length(data)
  n/beta +sum(log(data))-n*(sum(x^beta*log(data))/(sum(data^beta)))
}

hat.beta <- uniroot(score, data=x, interval=c(0.1,100))$root
hat.beta
hat.alpha <- (mean(x^hat.beta))^(1/hat.beta)
hat.alpha
######################################################



# (alternative) computation of MLEs using only numerical procedures
######################################

# density of Weibull distribution (as given in the slide)
dW <- function(x, alpha, beta) beta*alpha^-1*(x/alpha)^(beta-1)*exp(-(x/alpha)^beta)
dW(1, alpha=1, beta=2)

# equivalent build-in function

# shape = beta
# scale = alpha

dweibull(1, scale=1, shape=2)


# log-likelihood function
#
# Note that:
# 1.  the parameters are given as a single vector and
#     as first argument, this is a requirement of the function
#     nlminb() used later for optimization
#
# 2. we exploit the function dW() so as to avoid the
#    explicit computation (and implementation) of the
#    log-likelihood function

loglik.W <- function(par, data){#par=c(alpha, beta)
  alpha <- par[1]
  beta  <- par[2]
  l <- sum(log(dW(data, alpha, beta)))
  return(l)
}

# change the sign of the log-likelihood function
# (required because the function nlminb() is a minimizer)
#
minus.loglik.W <- function(par, data) -loglik.W(par, data)


# Numerical computation of the Maximum Likelihood Estimates
# of alpha and beta
#
opt.result <- nlminb(start = c(1,1), objective = minus.loglik.W, data=x)
opt.result$par

# store the estimate as scalars

alpha.hat <- opt.result$par[1]
beta.hat  <- opt.result$par[2]
alpha.hat
beta.hat

# Compute the observed information matrix
# with a function for numerical derivatives

library(numDeriv)

# Hessian Matrix
H <- hessian(func = loglik.W, x = c(alpha.hat, beta.hat), data=x)
H

# observed Fisher information  matrix
J <- -H
J

# asymptotic variance and covariance matrix

asy.var <- solve(J)
asy.var

# (approximate) standard error of parameters

se.alpha <- sqrt(asy.var[1,1])
se.beta  <- sqrt(asy.var[2,2])

# 95% (approximate) confidence intervals

alpha.hat+c(-1,1)*qnorm(0.975)*se.alpha
beta.hat +c(-1,1)*qnorm(0.975)*se.beta

# memoryless property?
#
# H_0: beta=1
# H_1: beta!=1

t.obs <- (beta.hat-1)/se.beta
t.obs

p.value <- 2*pnorm(-abs(t.obs))
p.value

#####################################################
#
# Exercise n. 9:  antibiotic efficacy
#
#####################################################

t <- 2:14
sample <- c(35,33,33,39,24,25,18,20,23,13,14,20,18)

# log-likelihood function
#
l <- function(delta, sample, t) -delta*sum(sample*t) - 50*sum(exp(-delta*t))

# vectorized version of the log-likelihood function
#
l.v <- Vectorize(l, vectorize.args = "delta")

# graphical representation of the log-likelihood
#
xl <- expression(delta)
yl <- expression(l(delta))
curve(l.v(x, sample=sample, t=t), -0.2, 1, xlab=xl, ylab=yl)

# numerical computation of the maximum likelihood estimate
# of delta with two alternative methods

# a) minimization of "minus" the log-likelihood

minus.l <- function(alpha, sample, t) -l.v(alpha, sample, t)

nlminb(0, minus.l, sample=sample, t=t)

# store the value as a scalar

delta.hat <- nlminb(0, minus.l, sample=sample, t=t)$par

# b) finding the root of the score function
#
score <- function(delta, sample, t) -sum(sample*t)+50*sum(t*exp(-delta*t))

uniroot(score, sample=sample, t=t, interval=c(-1, 1))

# compute the derivative of score function at delta.hat
# using two methods

# a) numerical derivative

hessian(func=l.v, x=delta.hat, sample=sample, t=t)

# b) explicit computation

scorep <- function(delta, sample, t) -50*sum(t^2*exp(-delta*t))
scorep(delta.hat, sample, t)

# observed Fisher information computed at delta.hat
#
j <- - scorep(delta.hat, sample, t)

# asymptotic standard error

se <- sqrt(1/j)
se

# 95% confindence interval for delta

delta.hat +c(-1, +1)*qnorm(0.975)*se


