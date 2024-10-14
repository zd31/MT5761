# Generating random dataset
set.seed(3489) ## For reproducibility
## 200 uniformly distributed random No.
x1 <- runif(200) 
## 200 normally distributed response with linear relation with x
y1 <- rnorm(200, 5 + 3*x1, sqrt(15)) 

# Lm fit
model1 <- lm(y1 ~ x1)
summary(model1)
sqrt(15) ## S.E.

# Distribution of residuals against fitted value
plot(fitted(model1), residuals(model1), ylim = c(-15, 15))
arrows(c(5.5, 6.5, 7.5, 8.5), rep(-10, 4), 
       c(5.5, 6.5, 7.5, 8.5), rep(10, 4), 
       col = 'firebrick3', code = 3, lwd = 2)

# Draw several plots about model1
plot(model1)




set.seed(3489)
x2 <- runif(200)
y2 <- rpois(200, exp(5 + 4.5*x2))

model2 <- glm(y2 ~ x2, family = poisson)
summary(model2)

plot(fitted(model2), residuals(model2, type = 'response'), ylim = c(-200, 200))
arrows(c(1000, 4000, 7000, 10000), c(-50, -100, -150, -200), c(1000, 4000, 7000, 10000), c(50, 100, 150, 200), col = 'firebrick3', code = 3, lwd = 2)

plot(model2)


library(nlme)
NystedFarms <- read.csv("NystedFarms.csv")
model3 <- gls(sqrt(Count/Area) ~ XPos + YPos + DistCoast + Depth + as.factor(Month) + Phase, data = NystedFarms, weights = varExp(), method = 'ML')
summary(model3)

plot(fitted(model3), residuals(model3))
arrows(c(-0.4, 0.1, 0.6, 1.1), c(-1, -1, -1, -1), c(-0.4, 0.1, 0.6, 1.1), c(3, 7, 40, 10), col = 'firebrick3', code = 3, lwd = 2)

par(mfrow = c(1, 2))
plot(fitted(model3), residuals(model3))
quantiles <- quantile(fitted(model3), probs = c(seq(0, 1, length = 10)))
abline(v = quantiles, col = 'lightgrey')
cut.fit <- cut(fitted(model3), breaks = quantiles)
means <- tapply(fitted(model3), cut.fit, mean)
vars <- tapply(residuals(model3), cut.fit, var)

points(fitted(model3), residuals(model3), col = ifelse(fitted(model3) >= quantiles[1] & fitted(model3) < quantiles[2], 'firebrick3', 'lightgrey'))

plot(means[1], vars[1], ylim = c(0, 40), xlim = c(-0.35, 1.1), xlab = 'fitted value (mean)', ylab = 'variance', col = 'dodgerblue2', pch = 16)

par(mfg = c(1, 1))
plot(fitted(model3), residuals(model3), col = ifelse(fitted(model3) >= quantiles[2] & fitted(model3) < quantiles[3], 'firebrick3', 'lightgrey'))
par(mfg = c(1, 2))
points(means[2], vars[2], col = 'dodgerblue2', pch = 16)

par(mfg = c(1, 1))
plot(fitted(model3), residuals(model3), col = ifelse(fitted(model3) >= quantiles[3] & fitted(model3) < quantiles[4], 'firebrick3', 'lightgrey'))
par(mfg = c(1, 2))
points(means[3], vars[3], col = 'dodgerblue2', pch = 16)

par(mfg = c(1, 1))
plot(fitted(model3), residuals(model3), col = ifelse(fitted(model3) >= quantiles[4] & fitted(model3) < quantiles[5], 'firebrick3', 'lightgrey'))
par(mfg = c(1, 2))
points(means[4], vars[4], col = 'dodgerblue2', pch = 16)

par(mfg = c(1, 1))
plot(fitted(model3), residuals(model3))
par(mfg = c(1, 2))
points(means[5], vars[5], col = 'dodgerblue2', pch = 16)
points(means[6], vars[6], col = 'dodgerblue2', pch = 16)
points(means[7], vars[7], col = 'dodgerblue2', pch = 16)
points(means[8], vars[8], col = 'dodgerblue2', pch = 16)
points(means[9], vars[9], col = 'dodgerblue2', pch = 16)

range(fitted(model3))
tryx <- seq(-0.35, 1.1, 0.01)
tryy <- summary(model3)$sigma^2 * exp(2*coef(model3$model)*tryx)
lines(tryx, tryy, lwd = 3, col = 'dodgerblue')
