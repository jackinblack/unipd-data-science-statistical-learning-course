```{r}
par(mfrow = c(1, 3));
plot(predict(model1, type = "link"), rstandard(model1));
abline(h = 0, col = "red", lwd = 2);
plot(predict(model1, type = "response"), rstandard(model1)
	,xlab = "Fitted Values"
	,ylab = "Deviance Residuals"
	,main = "Deviance Residuals vs Fitted Values");
abline(h = 0, col = "red", lwd = 2);
qqnorm(rstandard(model1));
qqline(rstandard(model1));

muhat <-fitted(model1)
par(mfrow=c(1,2), pty="s")
plot(muhat,(data$Rented.Bike.Count-muhat)^2,
     xlab=expression(hat(mu)), ylab=expression((y-hat(mu))^2 ))
plot(muhat, resid(model1,type="pearson"), 
     xlab=expression(hat(mu)), ylab="Pearson Residuals") 

1 - pchisq(deviance(model1), df = model1$df.residual)
1 - pchisq(deviance(model), df = model$df.residual)
check_overdispersion(data)
```

The plots represent deviance residuals against fitted values. As we can see a funnel shape we suspect non-constant variance in the error terms. This speaks
against the appropriateness of log(Y) transform for explaining the dataset. We can also notice that some of the points lie at a large distance from the main cluster, these points could potentially be outliers.
