library(MASS)
data("Cars93")
attach(Cars93)

# frequency table

table(Type)

freq.tb.Type <- table(Type)
freq.tb.Type
freq.tb.Type["Large"]

# relative frequency table

rel.freq.tb.Type <- freq.tb.Type/sum(freq.tb.Type)
rel.freq.tb.Type

round(rel.freq.tb.Type, digits=2)

library(xtable)
xtable(freq.tb.Type)

# pie chart

pie(freq.tb.Type)
help("pie")

barplot(freq.tb.Type)
barplot(rel.freq.tb.Type)

# More than one variable

tb.Origin.Type <- table(Origin, Type)
tb.Origin.Type

tb.Origin.Type["USA", "Large"]


barplot(tb.Origin.Type)
barplot(tb.Origin.Type, beside=TRUE)

# add a legend
barplot(tb.Origin.Type, beside=TRUE, legend=TRUE)

help("barplot")
xtable(tb.Origin.Type)

# histograms

Rear.seat.room.cm <- Rear.seat.room * 2.54
hist(Rear.seat.room.cm)

# making it look nicer
hist(Rear.seat.room.cm, xlab="cm", ylab="",
     main="Rear Seat Room", col="orange")

# change the number of bins
hist(Rear.seat.room.cm, breaks = 3, xlab="cm", ylab="",
     main="Rear Seat Room", col="orange")


# split the grapich panel and different number of bins

par(mfrow=c(1,2))
hist(Rear.seat.room.cm, xlab="Rear seat room (cm)",
     main="Default Number of Bins", col="orange")
hist(Rear.seat.room.cm, xlab="Rear seat room (cm)",
     main="30 Bins", breaks=30,col="orange")
par(mfrow=c(1,1))

# density vs frequency


range(Rear.seat.room.cm)

range(Rear.seat.room.cm, na.rm=TRUE)

bins <- c(48, 65, 70, 75,  92)


par(mfrow=c(1,2))
hist(Rear.seat.room.cm, freq=TRUE, xlab="Rear seat room (cm)",
     main="equi-spaced bins", breaks =10, col="orange")
hist(Rear.seat.room.cm, freq=TRUE,  xlab="Rear seat room (cm)",
     main="non-equi-spaced bins", breaks=bins, col="orange")
par(mfrow=c(1,1))


par(mfrow=c(1,2))
hist(Rear.seat.room.cm, freq=TRUE, xlab="Rear seat room (cm)",
     main="frequency", breaks =bins, col="orange")
hist(Rear.seat.room.cm, freq=FALSE,  xlab="Rear seat room (cm)",
     main="density", breaks=bins, col="orange")
par(mfrow=c(1,1))

# output of the function hist()

hist(Rear.seat.room.cm)

h <- hist(Rear.seat.room.cm)
h
plot(h)

# density plot

dens <- density(Rear.seat.room.cm, na.rm=TRUE)
plot(dens, main="Kernel Density")

# compare density plot with histogram
hist(Rear.seat.room.cm, freq=FALSE)
lines(dens)


# change the bandwidth
dens0.5 <- density(Rear.seat.room.cm, adjust=0.5, na.rm=TRUE)


plot(dens0.5, main="adjust=0.5")
lines(dens, col="blue")

# compare different bandwidths

dens0.25 <- density(Rear.seat.room.cm, adjust=0.25, na.rm=TRUE)
dens2 <- density(Rear.seat.room.cm, adjust=2, na.rm=TRUE)

plot(dens0.25)
lines(dens0.5, col="blue")
lines(dens, col="red")
lines(dens2, col="green")




library(MASS)
data("Cars93")
attach(Cars93)

Rear.seat.room.cm <- Rear.seat.room * 2.54

# mean

mean(Rear.seat.room.cm)
mean(Rear.seat.room.cm, na.rm=TRUE)


# removing NA's

# using the function is.na()
is.na(Rear.seat.room.cm)
Rear.seat.room.cm[!is.na(Rear.seat.room.cm)]

# using the function na.omit()

na.omit(Rear.seat.room.cm)

Rear.seat.room.cm <- na.omit(Rear.seat.room.cm)

# deviations from the mean

bar.x <- mean(Rear.seat.room.cm)
d <- Rear.seat.room.cm - bar.x

# check that deviations from the mean are centered about zero
hist(d)
sum(d)

# median

median(Rear.seat.room.cm)
sort(Rear.seat.room.cm)[46]

# variance and standard deviation

x <- Rear.seat.room.cm
s2 <- sum((x-mean(x))^2)/(length(x)-1)
var(x)

sd(x)
sqrt(s2)

# quantiles

quantile(Rear.seat.room.cm, 0.3)
quantile(Rear.seat.room.cm, c(0.3, 0.6))
quantile(Rear.seat.room.cm)


# five number summary

IQR(Rear.seat.room.cm)

summary(Rear.seat.room.cm)


####################################
#
# Scatterplots and boxplots
#
####################################


# boxplots

boxplot(Rear.seat.room.cm)
boxplot(Min.Price)

## side-by-side boxplots

Rear.seat.USA <- Rear.seat.room.cm[Origin=="USA"]
Rear.seat.non.USA <- Rear.seat.room.cm[Origin=="non-USA"]

boxplot(Rear.seat.USA, Rear.seat.non.USA)

# model formula syntax


boxplot(Rear.seat.room ~ Origin)

boxplot(Rear.seat.room~Type)
boxplot(Length~Type)

boxplot(Min.Price~Origin)
detach(Cars93)

####################
# "Old faithful" data
# and scatterplots

data("faithful")
attach(faithful)
help("faithful")

# EDA of waiting time

summary(waiting)
par(mfrow=c(1, 2))
hist(waiting, col="orange", freq=FALSE, main="")
boxplot(waiting, col="lightgray")
par(mfrow=c(1,1))

# EDA of eruption duration

summary(eruptions)

par(mfrow=c(1, 2))
hist(eruptions, col="orange", prob=TRUE, main="")
boxplot(eruptions, col="lightgray")
par(mfrow=c(1,1))

summary(faithful)


# scatterplots

plot(eruptions, waiting)
plot(waiting ~ eruptions)

# choice of symbol

plot(1:20)
plot(1:20, pch="A")
plot(1:20, pch=16)
plot(1:20, pch=1:20)
plot(1:20, pch=3)


# choice of colors

plot(1:20, col=1:8, pch=16)
colors()

# final scatterplot

lx <- "Eruption time (min)"
ly <- "Waiting time to next eruption (min)"
lmain <- "faithful data: Eruptions of Old Faithful"

plot(eruptions, waiting, xlab=lx, ylab=ly, main=lmain, pch="+")


# scatter.smooth with different smoothing parameters

scatter.smooth(eruptions, waiting, pch="+")
scatter.smooth(eruptions, waiting, span=2/3, pch="+") # this is the default value
scatter.smooth(eruptions, waiting, span=1, pch="+")
scatter.smooth(eruptions, waiting, span=1/2, pch="+")
scatter.smooth(eruptions, waiting, span=1/20, pch="+")


# LOESS= LOcally Estimated Scatterplot Smoothing
# different smoothing parameters for loess

int.line1 <- loess.smooth(eruptions, waiting, span=1)
int.line.66 <- loess.smooth(eruptions, waiting, span=2/3)
int.line.5 <- loess.smooth(eruptions, waiting, span=1/2)
int.line.05 <- loess.smooth(eruptions, waiting, span=1/20)

plot(eruptions, waiting, xlab=lx, ylab=ly, main=lmain, pch="+")
# add lines

lines(int.line1,  col="violet", lty=1, lwd=2)
lines(int.line.66, col="red", lty=1, lwd=2)
lines(int.line.5, col="blue", lty=1, lwd=2)
lines(int.line.05, col="black", lty=1, lwd=2)


# add legend

plot(eruptions, waiting, xlab=lx, ylab=ly, main=lmain, pch="+")
# add lines

lines(int.line1,  col=2, lty=2, lwd=2)
lines(int.line.66, col=3, lty=1, lwd=4)
lines(int.line.5, col=4, lty=3, lwd=2)
lines(int.line.05, col=5, lty=4, lwd=4)


legend(x=1.5,y=95, legend=c("smoothing=1","smoothing=2/3","smoothing=0.5",  "smoothing=0.1"), col=2:5, lty=c(2,1,3,4), lwd=c(2,4,2,4))

# alternative way for legend position
#
# "bottomright", "bottom", "bottomleft", "left",
# "topleft", "top", "topright", "right",  "center"

legend("topleft", y=NULL,
       legend=c("smoothing=1","smoothing=2/3","smoothing=0.5",  "smoothing=0.1"), col=2:5, lty=c(2,1,3,4), lwd=c(2,4,2,4))

# size of the legend
legend("topleft", y=NULL, cex=0.8,
       legend=c("smoothing=1","smoothing=2/3","smoothing=0.5",  "smoothing=0.1"), col=2:5, lty=c(2,1,3,4), lwd=c(2,4,2,4))


# Another example
library(MASS)
data("mcycle")
help(mcycle)
attach(mcycle)

plot(times, accel, pch="+")
scatter.smooth(times, accel, span=1, pch="+")
scatter.smooth(times, accel, pch="+") # span=2/3
scatter.smooth(times, accel, span=1/4, pch="+")
scatter.smooth(times, accel, span=1/10, pch="+")
scatter.smooth(times, accel, span=1/20, pch="+")


# old faithful data are time series

n <- length(waiting)
time <- 1:n

plot(time, waiting)


# first 100 time points and plot with "line"

plot(time[1:100], waiting[1:100], type="l")


waiting.next <- waiting[-1]
waiting.previous <- waiting[-n]

plot(waiting.previous, waiting.next, pch="+")

abline(h=70, lty=2, col="red")
abline(v=70, lty=2, col="red")

detach(faithful)

####################
# "Animals" data
#  and transformations

library(MASS)
data("Animals")
attach(Animals)
help("Animals")


summary(body)
hist(body, prob=TRUE)
hist(body, breaks=20, prob=TRUE, col=7)

# Logarithmic transformation

lbody <- log10(body)
summary(lbody)
hist(lbody, prob=TRUE, col=7)

# inverse transformation

10^mean(lbody)
summary(body)

# geometric mean
n <- length(body)
prod(body)^(1/n)

# scatterplot
plot(body, brain, pch="+")

lbrain <- log10(brain)
plot(lbody, lbrain, pch=16)



