###########################################
# Introduction to the likelihood function
###########################################

# Likelihood function Poisson n=1


# r-function for the pdf of the Poisson random variable
# (that is the same as the function dpois())

P <- function(x, lambda) {
  pr <- exp(-lambda)*lambda^(x)/factorial(x)
  return(pr)
}

# "direct" use of the pdf of the Poisson: compute the probability
# for a given value of the parameter lambda,
# for example lambda=4

x <- 0:15
plot(x, P(x, lambda=4), ylim=c(0, 0.25), type="h", ylab=expression(p(x, lambda==4)))

# "inverse" use of hte pdf of the Poisson: compute the
# likelihood of different values of the parameter
# given the observed value of x,
# for example x=3


P(3, lambda = 1)

# or equivalently, using the build-in function

dpois(3, lambda =1)

# likelihood function of Poisson n=1, x=3

L <- function(lambda) P(3, lambda)

L(1:4)
round(L(1:4), 2)

# use the likelihood ratio to compare different
# parameter values

L(3)/L(2.5)
L(3)/L(2)
L(3)/L(1)

# plot the likelihood function
curve(L, from=0, to=10, xlab=expression(lambda), ylab=expression(L(lambda)))


# likelihood function of Poisson - general version n>=1

L <- function(lambda, sample) {
  y <- 1
  for (x in sample) y <- y*P(x, lambda)
  return(y)
}

sample <- c(2, 1, 2, 6, 5)

L(3, sample)
L(2, sample)

L(3, sample)/L(2, sample)

curve(L(x, sample), from=0, to=8, xlab=expression(lambda), ylab=expression(L(lambda)))


# likelihood using sufficient statistics

L <- function(lambda, sample.mean, n) exp(-n*lambda)*lambda^(n*sample.mean)

curve(L(x, mean(sample), length(sample)), from=0, to=8, xlab=expression(lambda), ylab=expression(L(lambda)))

L(3, mean(sample), length(sample))
L(2, mean(sample), length(sample))
L(3, mean(sample), length(sample))/L(2, mean(sample), length(sample))

# likelihood, same mean different sample size

curve(L(x, mean(sample), 5), from=0, to=8, ylim=c(0, 40), xlab=expression(lambda), ylab=expression(L(lambda)))

curve(L(x, mean(sample), 7), from=0, to=8, add=TRUE, col="red", xlab=expression(lambda), ylab=expression(L(lambda)))

L(3, mean(sample), 5)/L(2, mean(sample), 5)
L(3, mean(sample), 7)/L(2, mean(sample), 7)



# log-likelihood

l <- function(lambda, bar.x, n) log(L(lambda, bar.x, n))
curve(l(x, mean(sample), length(sample)), 0, 8, xlab=expression(lambda), ylab=expression(l(lambda)))

# Maximum likelihood estimator
# (numerical approximation)

lambda <- seq(0, 8, length=1000)
l.values <- l(lambda, mean(sample), length(sample))
plot(lambda, l.values, type="l")

which(l.values==max(l.values))
lambda[401]


