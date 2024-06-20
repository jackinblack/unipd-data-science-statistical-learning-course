```

## Potential outliers
```{r}
dev_residuals <- residuals(model, type = "deviance")

# Plot deviance residuals to identify outliers
plot(dev_residuals, ylab = "Deviance Residuals", main = "Deviance Residuals for Poisson Regression")
abline(h = c(-30, 30), col = "red", lty = 2)

# Identify potential outliers
potential_outliers <- which(abs(dev_residuals) > 30)
potential_outliers
outliers <- data[potential_outliers, ]
outliers
```

## High leverage points 

```{r}
# Calculate leverage values
leverage <- hatvalues(model)

# Plot leverage values
plot(leverage, ylab = "Leverage", main = "Leverage Values for Poisson Regression")
abline(h = 2 * mean(leverage), col = "red", lty = 2)

# Identify high leverage points
high_leverage_points <- which(leverage > 2 * mean(leverage))
high_leverage_points
```

```{r}

test.model <- glm(Rented.Bike.Count ~., family = poisson(link = "log"), data=data[-union(potential_outliers, high_leverage_points),])
summary(test.model)
plot(test.model)
plot(predict(test.model, type = "link"), rstandard(test.model))
abline(h = 0)
plot(predict(test.model, type = "response"), rstandard(test.model))
abline(h = 0)
qqnorm(rstandard(test.model))
qqline(rstandard(test.model))
dev_residuals <- residuals(test.model, type = "deviance")

# Plot deviance residuals to identify outliers
plot(dev_residuals, ylab = "Deviance Residuals", main = "Deviance Residuals for Poisson Regression")
abline(h = c(-30, 30), col = "red", lty = 2)
leverage <- hatvalues(test.model)

# Plot leverage values
plot(leverage, ylab = "Leverage", main = "Leverage Values for Poisson Regression")
abline(h = 2 * mean(leverage), col = "red", lty = 2)
```

## Professor

```{r}
mod.out.resid <- residuals(model, type="deviance")
plot(mod.out.resid~fitted(model))
qqnorm(mod.out.resid)
qqline(mod.out.resid)