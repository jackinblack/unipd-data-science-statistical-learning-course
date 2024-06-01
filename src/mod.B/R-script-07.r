##################################
#
# Categorical variables
#
##################################


###############################
# Mendel's peas

y <- c(315, 108, 102, 31)
n <- sum(y)
n

p <- c(9/16, 3/16, 3/16, 1/16)

O <- y
E <- n*p

O-E

X2 <- sum((O-E)^2/E)
X2

1-pchisq(X2,3)

chisq.test(y,p=p)


##################################
# chi-square of independence

cars <- rbind(c(56, 52, 42), c(50, 83, 67), c(18, 51, 81))
rownames(cars) <- c("Large", "Medium", "Small")
colnames(cars) <- c("Large", "Medium", "Small")

cars
n <- sum(cars)

# add margins

cars.with.margins <- addmargins(cars)
cars.with.margins
# marginal counts

previous <- margin.table(cars, 1)
previous

new <- margin.table(cars, 2)
new

# expected counts

E <- previous%*%t(new)/n
E

# same margins as observed
margin.table(E, 1)
previous

margin.table(E, 2)
new

# chisq test

O <- cars

X2 <- sum((O-E)^2/E)

X2

1 - pchisq(X2, 4)

# r-function

chisq.test(cars)


#########################################
# 2 X 2 table - Vitamin C example
######################################

M <- matrix(c(335, 76, 302, 105), ncol=2)

colnames(M) <- c("Placebo", "Vitamin C")
rownames(M) <- c("Cold", "No-Cold")
M

# Placebo

n1 <- 335+76
pp.hat <- 335/n1
pp.hat


# Vitamin C

n2 <- 302+105
pc.hat <- 302/n2
pc.hat

# pooled pi

p.hat <- (335+302)/(n1+n2)
p.hat

se.hat <- sqrt(p.hat*(1-p.hat)*(1/n1+1/n2))
se.hat

t.obs <- (pp.hat-pc.hat)/se.hat
t.obs

p.value <- 1-pnorm(t.obs)
p.value

# binom.test

# with and without continuity correction
prop.test(c(335, 302), c(n1, n2), alt="g", correct=FALSE)
prop.test(c(335, 302), c(n1, n2), alt="g")

# chi-square test with and without continuity correction
chisq.test(M, correct = FALSE)
chisq.test(M)

# measures of association for 2x2 tables

pp.hat
pc.hat

# risk difference

pp.hat - pc.hat

# relative risk

pp.hat/pc.hat

# the odds ratio

(pp.hat/(1-pp.hat))

(pc.hat/(1-pc.hat))

theta <- (pp.hat/(1-pp.hat))/(pc.hat/(1-pc.hat))
theta

M[1,1]*M[2,2]/(M[1,2]*M[2,1])

# log-odds ratio

log(theta)

# Yule's Q

Q <- (theta-1)/(theta+1)
Q

# Admission at Berkeley
#
# In 1973, the University of California-Berkeley was afraid of being sued for sex discrimination,
# because their graduate school admission figures showed obvious bias against women. The
# numbers looked pretty incriminating: the graduate schools had just accepted 44% of male
# applicants but only 30% of female applicants. Thus they asked their department of statistics
# to analyze the data.
#
# Table in R object UCBAdmissions in package datasets contains the output of applications to
# graduate school at Berkeley for the six largest departments classified by admission and sex.

data("UCBAdmissions")
help(UCBAdmissions)

dim(UCBAdmissions)
dimnames(UCBAdmissions)

UCBAdmissions

tAGD <- UCBAdmissions

# "slices": examples

tAGD[,,1]
tAGD[,,"A"]

tAGD[2,,]
tAGD["Rejected",,]

tAGD[, 1,]
tAGD[, "Male",]

#######################################
# Acceptance rates in the
# marginal table of (Admit, Gender)
#######################################


# is Admission independent of Gender?
#
margin.table(tAGD, c(1,2))

tAG <- margin.table(tAGD, c(1,2))
tAG
chisq.test(tAG)

# acceptance rates

tG <- margin.table(tAGD, 2)
tG
tAG

# proportions of admitted
tAG["Admitted",]
tAG["Admitted",]/tG

# proportions of rejected
tAG["Rejected",]
tAG["Rejected",]/tG


#######################################
# Acceptance rates by department
#######################################

tAD <- margin.table(tAGD, c(1, 3))
tD <- margin.table(tAGD, 3)

tAD["Admitted",]/tD

###########################################
#
# Continuous data: partial correlation
#
############################################

# "MATHMARKS" DATA
library(gRbase)
data("mathmark")
names(mathmark)
mathmark[1:3,]

pairs(mathmark)

#####################################
# covariance and correlation matrices
#####################################

S <- cov(mathmark)
P <- cor(mathmark)

round(S, 1)
round(P, 2)

# compute the correlation matrix by scaling the covariance matrix

D <- diag(S)
D
D <- diag(D)
D

P <- solve(sqrt(D))%*%S%*%solve(sqrt(D))
round(P, 2)

# this procedure is implemented in the function cov2cor()

P <- cov2cor(S)


############################
# Partial correlations
############################

# partial correlation between analysis and statistics
# given the "rest" computed as correlation of regression
# residuals


mod1 <- lm(statistics ~. - analysis, data=mathmark)
mod2 <- lm(analysis ~. -statistics , data=mathmark)

cor(residuals(mod1), residuals(mod2))

# compute the partial correlation of every pair
# of variables given the "rest" by scaling
# the inverse covariance

round(solve(S), 6)

-cov2cor(solve(S))

###########################################
#
# Continuous data: independence graph
#
############################################


library(igraph)



# Mathmarks data
#######################

# naive model selection via thresholding

# identify the graph with partial correlations
# numerically smaller than, e.g. 0.1

data(mathmark)
S <- var(mathmark)
R <- -cov2cor(solve(S))

thr <- 0.1

G <- abs(R)>thr
diag(G) <- 0

G

# notice that rows and columns have names
rownames(G)
colnames(G)
dimnames(G)

Gi <- as(G, "igraph")
plot(Gi)


