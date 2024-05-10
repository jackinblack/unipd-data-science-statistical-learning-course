####################################################
# inference for the variance of a normal distribution
#####################################################

temp.data <- read.table("normtemp.txt", head=T)
attach(temp.data)
temp.C <- (temperature-32)*5/9

n <- length(temp.C)
n
s2 <- var(temp.C)
s2
s <- sd(temp.C)
s

ic.lower <- (n-1)*s2/qchisq(0.975, n-1)
ic.upper <- (n-1)*s2/qchisq(0.025, n-1)

# IC for the variance

round(c(lower=ic.lower, variance=s2, upper= ic.upper), 3)

# IC for the sd

round(c(lower=sqrt(ic.lower), std.dev=sqrt(s2), upper= sqrt(ic.upper)), 3)


########################################
# comparison of means: two populations
#########################################

# histograms comparison
par(mfrow=c(1,2))
hist(temp.C[gender=="M"], main="Men", prob=T, col="yellow", xlab="body temperature")
hist(temp.C[gender=="F"], main="Women", prob=T, col="yellow", xlab="body temperature")
par(mfrow=c(1,1))

d.m <- density(temp.C[gender=="M"])
d.f <- density(temp.C[gender=="F"])
plot(d.m, main="", lwd=2, col="blue", xlim=c(35, 39), ylim=c(0, 1.1))
lines(d.f, lwd=2, col="red")

# side-by-side boxplots
boxplot(temp.C~gender, col="lightgray")


# normal quantile plots

par(mfrow=c(1,2))
qqnorm(temp.C[gender=="M"], main="Males")
qqline(temp.C[gender=="M"], main="Females")

qqnorm(temp.C[gender=="F"])
qqline(temp.C[gender=="F"])
par(mfrow=c(1,1))

# Males

n1 <- length(temp.C[gender=="M"])
n1


m.1 <- mean(temp.C[gender=="M"])
m.1

s2.1 <- var(temp.C[gender=="M"])
s2.1

# Females

n2 <- length(temp.C[gender=="F"])
n2


m.2 <- mean(temp.C[gender=="F"])
m.2

s2.2 <- var(temp.C[gender=="F"])
s2.2

# pooled variance

pooled.var <- ((n1-1)*s2.1+(n2-1)*s2.2)/(n1+n2-2)

# test statistic

t.obs <- (m.1-m.2)/(sqrt(pooled.var*(1/n1 +1/n2)))
t.obs
p.value <- 2*pt(-abs(t.obs), n1+n2-2)
p.value

# function t.test()

t.test(temp.C[gender=="M"], temp.C[gender=="F"], var.equal=TRUE)
t.test(temp.C~gender, var.equal=TRUE)

# comparing variances

t.obs <- s2.1/s2.2
t.obs

# critical values

qf(0.025, n1-1, n2-1)
qf(0.975, n1-1, n2-1)

# compute the p.value
1/t.obs
p.value <- pf(t.obs, n1-1, n2-1)+ (1-pf(1/t.obs, n2-1,n1-1))
p.value


var.test(temp.C[gender=="M"], temp.C[gender=="F"])
var.test(temp.C~gender)


# t.test non-equal variances

t.test(temp.C~gender,, var.equal=FALSE)


#####################################
# comparison of means for paired data
#####################################

data(sleep)
attach(sleep)

g1 <- extra[group==1]
g2 <- extra[group==2]

d <- g2-g1
qqnorm(d)
qqline(d)

boxplot(g2-g1, col="lightgray")
abline(h=0, lty=2)

t.test(d)

# wrong application of the t-test
t.test(g2, g1, paired=FALSE)

# t-test for paired data
t.test(g2, g1, paired=TRUE)
t.test(d)


#########################################
# comparing proportions
######################################

M <- matrix(c(335, 75, 302, 105), ncol=2, byrow=TRUE)
M
margin.table(M, margin=1)

# Placebo

n1 <- 335+75
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

prop.test(c(335, 302), c(n1, n2), alt="g")

