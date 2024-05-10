###############################
# LOGISTIC CLASSIFIER
##############################

library(ISLR2)
data("Default")
attach(Default)

# scatterplot of the data with the two classes

# obtain a "0/1" response variable
l <- default=="Yes"
plot(balance, income, col=l+1, pch=l*15+1)

#########################
# logistic classifier
#########################

# check that 1="YES" and 0="NO"
contrasts(default)

# fit the logistic regression model
mod.out <- glm(default ~ income + balance, family = binomial)

# obtain the estimated probabilities
logistic.prob <- predict(mod.out, type="response")

# assign probabilities>0.5 to the class "YES"
logistic.pred <- rep("No", 10000)
logistic.pred[logistic.prob>0.5] <- "Yes"

# compare true values with predicted values

default[200:215]
logistic.pred[200:215]
default[200:215]==logistic.pred[200:215]

# decision boundary

summary(mod.out)
beta <- coefficients(mod.out)

intercept <- -beta[1]/beta[2]
slope <- -beta[3]/beta[2]

plot(balance, income, col=l+1, pch=l*15+1)
abline(intercept, slope, lwd=2, col="blue")

# confusion matrix

conf.matrix <- table(logistic.pred, default)
conf.matrix

# rearrange rows and columns to match
# the confusion matrix on the slides
# and add margins
conf.matrix <- conf.matrix[2:1, 2:1]
conf.matrix <- addmargins(conf.matrix, margin = c(1, 2))
conf.matrix

# overall (training) error rate
(225+38)/10000


# trivial classifier that assigns "No" to all the
# units

trivial.pred <- rep("No", 10000)

# confusion matrix for the trivial predictor
table(trivial.pred, default)

# overall (training) error rate for the trivial classifier
333/10000

# (training) error rate for defaulting-customers
conf.matrix
225/333

# threshold=0.2

logistic.pred <- rep("No", 10000)
logistic.pred[logistic.prob>0.2] <- "Yes"

table(logistic.pred, default)[2:1, 2:1]

# overall (training) error rate
(133+271)/10000

# (training) error rate among individuals who defaulted
133/(133+200)

# (training) error rate among individuals who did not default
271/(271+9396)

####################################
# computing performance measures
####################################


# Confusion matrix with threshold = 0.5

logistic.pred <- rep("No", 10000)
logistic.pred[logistic.prob>0.5] <- "Yes"

conf.matrix <- table(logistic.pred, default)
conf.matrix

n <- sum(conf.matrix) # sample size

# extract relevant quantities from the confusion matrix

lab.pos = "Yes"
lab.neg = "No"

TP <- conf.matrix[lab.pos, lab.pos]
TP

TN <- conf.matrix[lab.neg, lab.neg]
TN

FP <- conf.matrix[lab.pos, lab.neg]
FP

FN <- conf.matrix[lab.neg, lab.pos]
FN

P     <- TP + FP
N     <- FP + TN
P.ast <- TP + FN

# compute the performance measures

# overall error rate
OER <- (FP+FN)/n
OER

# Positive Predicted Values (Precision)
PPV <- TP/P.ast
PPV

# True Positive Rate (Sensitivity, Recall)
# that refers to Pr(positive | positive)
TPR <- TP/P
TPR

# F1 score (harmonic mean of PPV and TPR)
F1  <- 2*PPV*TPR/(PPV+TPR)
F1

# True Negative Rate (Specificity)
# that refers to Pr(negative | negative)
TNR <- TN/N
TNR

# False Positive Rate
FPR <- FP/N
FPR


# Function to compute performance measures
#
# Arguments:
#
# pred.values = vector of predicted values
# true.values = vector of true values
# lab.pos     = label of the positive class
#
perf.measure <- function(true.values, pred.values,  lab.pos = 1){
  #
  # compute the confusion matrix and number of units
  conf.matrix <- table(pred.values, true.values)
  n <- sum(conf.matrix)
  #
  # force the label of positives to be a character string
  lab.pos <- as.character(lab.pos)
  #
  # obtain the label of negatives
  lab <- rownames(conf.matrix)
  lab.neg <- lab[lab != lab.pos]
  #
  # extract relevant quantities from the confusion matrix
  TP <- conf.matrix[lab.pos, lab.pos]
  TN <- conf.matrix[lab.neg, lab.neg]
  FP <- conf.matrix[lab.pos, lab.neg]
  FN <- conf.matrix[lab.neg, lab.pos]
  P     <- TP + FN
  N     <- FP + TN
  P.ast <- TP + FP
  #
  # compute the performance measures
  OER <- (FP+FN)/n
  PPV <- TP/P.ast
  TPR <- TP/P
  F1  <- 2*PPV*TPR/(PPV+TPR)
  TNR <- TN/N
  FPR <- FP/N
  return(list(overall.ER = OER, PPV=PPV, TPR=TPR, F1=F1, TNR=TNR, FPR=FPR))
}

PM <- perf.measure(default, logistic.pred,  lab.pos="Yes")
PM

# ROC curve

library(pROC)

# If labels are "0" and "1" then "0"=negative and "1"=positive
# levels = negative (0's) as first element and positive (1's) as second

roc.out <- roc(default, logistic.prob, levels=c("No", "Yes"))

# different ways of plotting the ROC curve
plot(roc.out) # check values on the x axis

# legacy.axes=TRUE   1 - specificity on the x axis
plot(roc.out, legacy.axes=TRUE)

# change the labels:
#  Sensitivity = TPR
#  Specificity = TNR

plot(roc.out, legacy.axes=TRUE, xlab="1 - True Negative Rate", ylab="True Positive Rate")

# change the labels:
# 1 - TNR = FPR
plot(roc.out, legacy.axes=TRUE, xlab="False Positive Rate", ylab="True positive rate")

# compute AUC = Area Under the Curve
plot(roc.out,  print.auc=TRUE, legacy.axes=TRUE, xlab="False Positive Rate", ylab="True Positive Rate")
auc(roc.out)

# specificity (TNR) and sensitivity (TPR) for a given threshold
coords(roc.out, 0.5)

coords(roc.out, seq(0.1, 0.9, by=0.1))

# threshold that maximizes the sum of specificity (TNR) and sensitivity (TPR)
coords(roc.out, "best")

########################################
#
# Compare the classification based on the ROC
# curve with the F1 score metric
#
########################################

# threshold="best"
best.th <- coords(roc.out, "best")$threshold
logistic.pred <- rep("No", 10000)
logistic.pred[logistic.prob>best.th] <- "Yes"

# confusion matrix: note the small number (34)
# of wrongly classified defaulting clients
# but a large number (1310) of non-defaulting
# clients are wrongly classified
table(logistic.pred, default)[2:1, 2:1]

# note that both TPR and TNR have satisfying value
# but PPV = 0.18, i.e. a lot of "good" clients (default="No")
# are classified as "bad", that is default="Yes"
# and, as a consequence F1 = 0.308
#
perf.measure(default, logistic.pred, lab.pos = "Yes")

# a good value of the F1 score can be obtained
# with threshold= 0.34

logistic.pred <- rep("No", 10000)
logistic.pred[logistic.prob>0.34] <- "Yes"


# confusion matrix: note the number (174)
# of wrongly classified defaulting clients increases
# but the number (103) of  wrongly classified non-defaulting
# clients becomes much smaller

table(logistic.pred, default)[2:1, 2:1]

# In this way all performance measures improve but TPR
# that decreases.
#
perf.measure(default, logistic.pred, lab.pos = "Yes")


###############################################
# LABORATORY: prediction of the S&P stock index
###############################################

# The Stock Market Dataset

library(ISLR2)
data(Smarket)

names(Smarket)
dim(Smarket)
summary(Smarket)
pairs(Smarket)
cor(Smarket)
round(cor(Smarket[,-9]), 3)
attach(Smarket)
plot(Volume)

# Logistic classification with all available covariates (except Today and Year)

glm.fits <- glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume, data=Smarket, family=binomial)

# alternative syntax using the "dot"
glm.fits <- glm(Direction~.-Today-Year, data=Smarket, family=binomial)

summary(glm.fits)

# obtain fitted probabilities
glm.probs <- predict(glm.fits, type="response")
glm.probs[1:10]

# check the coding of Direction (to properly interpret probabilities)
# note that 1 encodes "Up"
contrasts(Direction)

# obtain a confusion matrix

glm.pred <- rep("Down",1250)
glm.pred[glm.probs>.5] <- "Up"
table(glm.pred, Direction)

# overall (training) error rate
perf.measure(Direction, glm.pred, lab.pos="Up")$overall.ER

# We compare this overall error rate with the error rates
# of two "trivial" classification methods:
#
# a) "RANDOM GUESSING" classification is carried out "flipping a coin",
#     in this case we expect an overall error rate equal to 50%
#
# b) Constant "always Up" classification, in this case the
#    overall error rate coincides with the proportion of "Down"

table(Direction)/length(Direction)

# ROC curve

library(pROC)
roc.out <- roc(Direction, glm.probs, levels=c("Down", "Up"))
plot(roc.out, print.auc=TRUE, legacy.axes=TRUE, xlab="False positive rate", ylab="True positive rate")

#####################################################
# USING A TRAINING AND A HOLD-OUT  SET
#####################################################

# training/validation set selection

train <- (Year<2005)
Smarket.2005 <- Smarket[!train,]
dim(Smarket.2005)
Direction.2005 <- Direction[!train]

# logistic regression on training set

glm.fits <- glm(Direction~.-Today-Year, family=binomial, data=Smarket, subset=train)

# prediction on validation set
glm.probs <- predict(glm.fits, Smarket.2005, type="response")

glm.pred <- rep("Down",252)
glm.pred[glm.probs>.5] <- "Up"
table(glm.pred,Direction.2005)

# overall test error rate
perf.measure(Direction.2005, glm.pred, lab.pos = "Up")$overall.ER

# compare with trivial "always Up" classification
table(Direction.2005)/length(Direction.2005)

# ROC curve
roc.out <- roc(Direction.2005, glm.probs, levels=c("Down", "Up"))
plot(roc.out,  print.auc=TRUE, legacy.axes=TRUE, xlab="False positive rate", ylab="True positive rate")

coords(roc.out, 0.5)
perf.measure(Direction.2005, glm.pred, lab.pos = "Up")

# reduced set of predictors

glm.fits <- glm(Direction~Lag1+Lag2,data=Smarket,family=binomial,subset=train)
glm.probs <- predict(glm.fits,Smarket.2005,type="response")
glm.pred <- rep("Down",252)
glm.pred[glm.probs>.5] <- "Up"
table(glm.pred,Direction.2005)

# overall test error rate
perf.measure(Direction.2005, glm.pred, lab.pos = "Up")$overall.ER

# compare with trivial "always Up" classification
table(Direction.2005)/length(Direction.2005)

roc.out <- roc(Direction.2005, glm.probs, levels=c("Down", "Up"))
plot(roc.out, print.auc=TRUE, legacy.axes=TRUE, xlab="False positive rate", ylab="True positive rate")


# note that PPV = 0.58
# this might suggest a strategy for investing
# by only acting on the days where our prediction in "Up

perf.measure(Direction.2005, glm.pred, lab.pos = "Up")



####################################
# EXERCISE 11 chapter 4
####################################


data(Auto)

# Create a binary variable mpg01 that contains a 1 if mpg
# contains a value above its median and a 0 if mpg contains
# a value below its median

# You want to predict mpg01 from the other variables in the
# dataset.

# Create a new data.frame "new.auto" with the variable you
# need for the analysis.

mpg01 <- (Auto$mpg > median(Auto$mpg))+0 # add a 0 to turn logical into a 0/1 number
table(mpg01)


new.auto <- data.frame(mpg01, Auto[, -c(1,9)])
new.auto$origin <- as.factor(new.auto$origin)
rm(mpg01)

n <- dim(new.auto)[1]
n

attach(new.auto)


# look at some boxplots
par(mfrow=c(1, 3))
boxplot(weight~mpg01)
boxplot(year~mpg01)
boxplot(horsepower~mpg01)
par(mfrow=c(1,1))


# perform a logistic discrimination analysis to predict
# mpg1 from the other variables

glm.fits <- glm(mpg01~., data=new.auto, family=binomial)
summary(glm.fits)

glm.prob <- predict(glm.fits, type="response")
glm.pred <- (glm.prob>0.5)+0

# confusion matrix
table(mpg01, glm.pred)

# look at performance measures
perf.measure(mpg01, glm.pred)

# and more specifically the overall training error rate

perf.measure(mpg01, glm.pred)$overall.ER

# ROC curve
library(pROC)
roc.out <- roc(mpg01, glm.prob)
plot(roc.out, print.auc=TRUE, legacy.axes=TRUE, xlab="False positive rate", ylab="True positive rate")


#########################################
#  USING A TRAINING AND A VALIDATION SET
#########################################

set.seed(456)
# set.seed(123)
# set.seed(789)
validation <- sample(1:n, 130) # indexes of cars in the validation set
train <- setdiff(1:n, validation) # indexes of cars in training set

validation.auto <- new.auto[validation, ] # validation set
train.auto <- new.auto[train, ] # training set

glm.fits <- glm(mpg01~., data=train.auto, family=binomial)
summary(glm.fits)

glm.prob <- predict(glm.fits, validation.auto, type="response")
glm.pred <- (glm.prob>0.5)+0

table(validation.auto$mpg01, glm.pred)

# overall test error rate to be compared with the training error rate
perf.measure(validation.auto$mpg01, glm.pred)$overall.ER

# EXERCISE: run this code with different random seeds and compare error rates

library(pROC)
roc.out <- roc(validation.auto$mpg01, glm.prob)
plot(roc.out, print.auc=TRUE, legacy.axes=TRUE, xlab="False positive rate", ylab="True positive rate")


####################################################
# SELECTION OF PREDICTORS
# METHOD USED: stepwise backward elimination
####################################################

glm.F <- glm(mpg01~., data=new.auto, family=binomial)
summary(glm.F)

# step 1

glm.1 <- update(glm.F, .~.-acceleration)
anova(glm.1, glm.F, test="Chisq")
summary(glm.1)

# step 2

glm.2 <- update(glm.1, .~.-cylinders)
anova(glm.2, glm.F, test="Chisq")
summary(glm.2)

# step 3


glm.3 <- update(glm.2, .~.-displacement)
anova(glm.3, glm.F, test="Chisq")

summary(glm.3)

# classification with selected model

glm.fits <- glm(mpg01~.-acceleration-cylinders-displacement, data=train.auto)

glm.prob <- predict(glm.3, validation.auto, type="response")
glm.pred <- (glm.prob>0.5)+0

# confusion matrix
table(validation.auto$mpg01, glm.pred)

# overall test error rate
perf.measure(validation.auto$mpg01, glm.pred)$overall.ER


# ROC curve
roc.out <- roc(validation.auto$mpg01, glm.prob)
plot(roc.out, print.auc=TRUE, legacy.axes=TRUE, xlab="False positive rate", ylab="True positive rate")



######################################
# Linear Discriminant Analysis (LDA)
######################################

library(MASS)
lda.fit <- lda(Direction~Lag1+Lag2, data=Smarket, subset=train)
lda.fit

# plot the values of the discriminant function for the two groups
#
plot(lda.fit, type="histogram")
plot(lda.fit) # histogram is the default value

plot(lda.fit, type="density")
plot(lda.fit, type="both")

######################################
# An example that shows a more clear separation
# between classes: the Default dataset
#
data("Default")
lda.fit.default <- lda(default~balance+income, data=Default)
plot(lda.fit.default)
plot(lda.fit.default, type="density")
#
############################################

# let's continue with the Smarket example


lda.pred <- predict(lda.fit, Smarket.2005)
names(lda.pred)

# take a look at the vectors component lda.pred

lda.pred$posterior[10:20,]
lda.pred$class[10:20]

# these are obtained with threshold = 0.5
# but an arbitrary threshold could be speficied as

lda.class <- rep("Down", 252)
lda.class[lda.pred$posterior[,2]>= 0.5] <- "Up"

# check that lda.class and lda.pred$class are equal
sum(lda.class!=lda.pred$class)

# overall misclassification error rate
perf.measure(Direction.2005, lda.pred$class, lab.pos = "Up")$overall.ER


######################################
# Quadratic Discriminant Analysis (QDA)
######################################

qda.fit <- qda(Direction~Lag1+Lag2, data=Smarket, subset=train)
qda.fit
qda.class <- predict(qda.fit,Smarket.2005)$class
table(qda.class, Direction.2005)

# overall misclassification error rate
# note that this is the best, so far.
perf.measure(Direction.2005, qda.class, lab.pos = "Up")$overall.ER

##########################
# Naive Bayes
###########################


###
library(e1071)

# NaiveBayes classifier assuming independence
# and normal distribution of predictors

nb.fit <- naiveBayes(Direction ~ Lag1 + Lag2, data = Smarket, subset = train)

#
# check the nb.fit object:

# $apirori
#
# "apriori" class distribution for the dependent variable.

# $tables
#
# A list of tables, one for each predictor variable.
# For each categorical variable a table giving,
# for each attribute level, the conditional probabilities
# given the target class. For each numeric variable,
# a table giving, for each target class, mean and standard
# deviation of the (sub-)variable.

nb.fit$tables

### compare nb.fit$tables explicitly for Lag1

# Down

Lag1.train.Down <- Lag1[train][Direction[train] == "Down"]
mean.L1.Down <- mean(Lag1.train.Down)
sd.L1.Down <- sd(Lag1.train.Down)
mean.L1.Down
sd.L1.Down

# Up

Lag1.train.Up <- Lag1[train][Direction[train] == "Up"]
mean.L1.Up <- mean(Lag1.train.Up)
sd.L1.Up <- sd(Lag1.train.Up)
mean.L1.Up
sd.L1.Up

# compare the histogram of the data with the estimated normal distribution
# and with an alternative non-parametric kernel density estimate

# Down

hist(Lag1.train.Down, prob=TRUE, xlab="Training data - Lag1='Down'", ylim=c(0, 0.4))
curve(dnorm(x, mean=mean.L1.Down, sd=sd.L1.Down), add=TRUE, lwd=2, col="blue")

dens <- density(Lag1.train.Down)
lines(dens, col="red", lwd=2)

# Up

hist(Lag1.train.Up, prob=TRUE, xlab="Training data - Lag1='Up'", ylim=c(0, 0.4))
curve(dnorm(x, mean=mean.L1.Up, sd=sd.L1.Up), add=TRUE, lwd=2, col="blue")

dens <- density(Lag1.train.Up, adjust=2)
lines(dens, col="red", lwd=2)


# use type = "class" for classification with threshold = 0.5
# note that this is the default
#
nb.class <- predict(nb.fit, Smarket.2005, type="class")
nb.class <- predict(nb.fit, Smarket.2005)

table(nb.class, Direction.2005)

# note that that it is only slightly worse than
# Quadratic Discriminant Analysis
#
perf.measure(Direction.2005, nb.class, lab.pos="Up")$overall.ER


# If type = "raw", the conditional a-posterior probabilities
# for each class are returned (this allows to use an
# arbitrary threshold)

nb.preds <- predict(nb.fit, Smarket.2005, type = "raw")
nb.preds[1:5, ]


###########################
# K-Nearest Neighbors (KNN)
###########################

library(class)

# data preparation as required by the function knn()
train.X         <- cbind(Lag1,Lag2)[train,]
validation.X    <- cbind(Lag1,Lag2)[!train,]
train.Direction <- Direction[train]

# apply with k = 1
set.seed(123)
knn.pred <- knn(train.X, validation.X, train.Direction, k=1)
table(knn.pred,Direction.2005)

perf.measure(Direction.2005, knn.pred, lab.pos="Up")$overall.ER

# the best performance is obtained with k = 2
set.seed(123)
knn.pred <- knn(train.X, validation.X, train.Direction, k=3)
table(knn.pred,Direction.2005)
perf.measure(Direction.2005, knn.pred, lab.pos="Up")$overall.ER




