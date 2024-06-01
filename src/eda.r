library(MASS)
library(boot)
library(leaps)
library(glmnet)
library(pROC)
library(corrplot)
library(numDeriv)
library(igraph)
library(gRbase)
library(xtable)
library(ISLR2)
library(car)
library(faraway)
library(e1071)
library(class)
library(knitr)
library(rmarkdown)

seoul.bike.data <- read.csv(
	file = "./data/SeoulBikeData.csv"
	,sep = ","
)
summary(seoul.bike.data)


attach(seoul.bike.data)

corrplot(seoul.bike.data)

is.factor(Functioning.Day)
Functioning.Dayf <- as.factor(Functioning.Day)
Holidayf <- as.factor(Holiday)
DateTime <- strptime( 
	paste( Date, Hour )
	,format="%Y/%m/%d %H"
)

plot(Rented.Bike.Count, Hour, pch = 20)
plot(Rented.Bike.Count, Temperature, pch = 20)
plot(Rented.Bike.Count, Functioning.Dayf, pch = 20)
plot(Rented.Bike.Count, Holidayf, pch = 20)
plot(Rented.Bike.Count, DateTime, pch = 20)
Humidity       Wind.speed      Visibility   Dew.point.temperature
Solar.Radiation     Rainfall          Snowfall         Seasons         
Holiday          Functioning.Day
var();
