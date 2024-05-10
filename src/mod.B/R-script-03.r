##########################
# LOGISTIC REGRESSION
##########################

#################################
# Space Shuttle Challenger
# O-rings dataset
#################################

library(faraway)
data(orings)

# 1 = at least one failure
orings[1,2]
orings[1,2] <- 1
attach(orings)

# Example to show that linear regression is
# not appropriate when the response is binary

linear.out <- lm(damage~temp)
summary(linear.out)

# scatterplot of the data with fitted line
plot(temp, damage, pch=20, xlim=c(30, 85), ylim=c(-.25, 2))
abline(linear.out)

# example of wrong predictions with negative
# probabilities or probabilities larger than
# one

# The launch temperature on the day of the crash was 31F.
# 31 Fahrenheit = - 0.56 Celsius
# 82 Fahrenheit = +27.78 Celsius

new.temp <- data.frame(temp=c(31, 82))
pred.linear <- predict(linear.out, newdata=new.temp)
pred.linear
points(c(31, 82), pred.linear, pch="X", col="red")

#########################
# the glm() function
#########################

logit.out <- glm(damage ~ temp, family = binomial)
summary(logit.out)

####################################################
# graphical representation of the fitted regression
####################################################

# scatterplot of the data

plot(temp, damage, pch=20, xlim=c(45, 85), ylim=c(-.25, 1.25))

# vector of x values
x <- seq(45, 85, length=100)

# obtain the logistic regression coefficients
beta.hat <- coefficients(logit.out)
beta0.hat <- beta.hat[1]
beta1.hat <- beta.hat[2]

# compute the "y" values

# function implementing the inverse-logit transformation
inv.logit <- function(beta0, beta1, x) {
  y <- exp(beta0+beta1*x)
  return(y/(1+y))
}

y <- inv.logit(beta0.hat, beta1.hat, x)

# add the fitted curve to the plot
lines(x, y, col="blue", lwd=1.5)

# comparison with the linear model
abline(linear.out)

#################################
#  function predict() for GLMs
#################################

new.temp <- data.frame(temp=31)

# predict the expected value (probability)
prob.hat <-predict(logit.out, newdata=new.temp, type="response")
prob.hat

# compute the logit
log(prob.hat/(1-prob.hat))

# predict the logit value
logit.hat <-predict(logit.out, newdata=new.temp, type="link")
logit.hat

# check the predicted value
beta0.hat+31*beta1.hat

############################################
# EXAMPLE WITH THE DEFAULT DATASET
############################################

library(ISLR2)
data("Default")
attach(Default)
names(Default)

# check the coding of the response
contrasts(default)


# fit logistic regression models with a single covariate

mod.out <- glm(default ~ balance, family = binomial)
summary(mod.out)

mod.out <- glm(default ~ income, family = binomial)
summary(mod.out)

mod.out <- glm(default ~ student, family = binomial)
summary(mod.out)

# Fit "Full" models including all covariates
output.F <- glm(default ~ balance+student+income, family = binomial)
summary(output.F)

# Note that there is an  "effect-reversal" for students
# due to a confounding effect of other predictors: check
# the relationship between predictors

boxplot(balance ~ student)
boxplot(income ~ student)

#####################################################
#  RECALL THE COMPARIOSON OF NESTED MODELS
#  IN THE CASE OF LINEAR MODELS
#####################################################

Advertising <- read.csv("Advertising.csv")
attach(Advertising)

# sample size
n <- length(sales)
n

mod.F <- lm(sales~TV+radio+newspaper)
summary(mod.F)

mod.R <- lm(sales~TV)
summary(mod.R)

# comparison of nested models
anova(mod.R, mod.F, test="F")

# where test="F is the default value
anova(mod.R, mod.F)


# NOTE THAT WE CAN OBTAIN THE RSS AS
deviance(mod.F)
deviance(mod.R)

##################################################
# THE DEVIANCE AND COMPARISON OF NESTED GLMs
##################################################


# Full model

output.F <- glm(damage ~ temp, family = binomial)
summary(output.F)

output.F$deviance
# or, equivalently,
deviance(output.F)


pi.hat <- predict(output.F, type="response")
DF <- -2*sum(damage*log(pi.hat)+(1-damage)*log(1-pi.hat))
DF

df.F <- output.F$df.residual
df.F


# NULL model

output.R <- glm(damage ~ +1, family = binomial)
summary(output.R)

output.R$deviance

pi.hat <- predict(output.R, type="response")
DR <- -2*sum(damage*log(pi.hat)+(1-damage)*log(1-pi.hat))
DR

df.R <- output.R$df.residual
df.R

# Deviance difference test

dev.test <- DR-DF
dev.test
df <- df.R- df.F
df

pvalue <- 1-pchisq(dev.test, df)
pvalue

# deviance difference using the anova() function

anova(output.R, output.F, test="Chisq")

# another example
#########################

output.F <- glm(default ~ balance+student+income, family = binomial)
output.R <- glm(default ~ balance+student, family = binomial)
anova(output.R, output.F, test="Chisq")


#########################################
# RESIDUAL PLOTS AND DIAGNOSTICS FOR GLMs
#########################################

logit.out.resid <- residuals(output.F, type="pearson")
plot(logit.out.resid~fitted(output.F))
abline(h=0, lty=3)
qqnorm(logit.out.resid)
qqline(logit.out.resid)


par(mfrow=c(2,2))
plot(output.F)
par(mfrow=c(1,1))


###############################################
# POISSON REGRESSION
###############################################

# The "ships" dataset"

library(MASS)
data(ships)


# define year and period as factors

ships$yearf <- factor(ships$year, levels =c(60, 65, 70, 75), labels= c("60-64", "65-69", "70-74", "75-79"))
ships$periodf <- factor(ships$period, levels= c(60, 75), labels= c("60-74", "75-79"))

# alternative way to do the same thing
#
#ships$yearf   <- as.factor(ships$year)
#levels(ships$yearf)   <- c("60-64", "65-69", "70-74", "75-79")
#
#ships$periodf <- as.factor(ships$period)
#levels(ships$periodf) <- c("60-74", "75-79")

ships$y <- ships$incidents

# remove observations with service=0
ships <- subset(ships, subset=service > 0)

par(mfrow = c(1, 3))
plot(y ~ type, data = ships)
plot(y ~ service, data = ships, pch = as.character(type))
plot(y/service ~ type, data = ships)
par(mfrow=c(1,1))

ships$rate <- ships$y/ships$service
par(mfrow = c(2, 3))
plot(y ~ type, data = ships)
plot(y ~ yearf, data = ships)
plot(y ~ periodf, data = ships)
plot(rate ~ type, data = ships)
plot(rate ~ yearf, data = ships)
plot(rate ~ periodf, data = ships)
par(mfrow=c(1,1))

# function glm() with family=poisson

MF <- glm(y ~ type + yearf + periodf + offset(log(service)),
          data = ships, family = poisson)

summary(MF)

# confidence intervals for parameters

confint(MF)


MR <- glm(y ~ type + periodf + offset(log(service)), data = ships,  family = "poisson")
anova(MR, MF, test = "Chisq")

# model diagnostics

par(mfrow = c(1, 3))
plot(predict(MF, type = "link"), rstandard(MF))
abline(h = 0)
plot(predict(MF, type = "response"), rstandard(MF))
abline(h = 0)
qqnorm(rstandard(MF))
qqline(rstandard(MF))

par(mfrow=c(1,1))

par(mfrow=c(2,2))
plot(MF)
par(mfrow=c(1,1))

