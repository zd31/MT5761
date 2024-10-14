library(readr)
NystedFarms <- read_csv("C:/Users/hannah/OneDrive - University of St Andrews/Teaching/2023 - 2024/MT5761/MT5761_2023-24/Lecture Notes/data/NystedFarms.csv")
NystedFarms$FMonth <- as.factor(NystedFarms$Month)

fullModel <- glm(Count ~ XPos + YPos + DistCoast + Depth + FMonth + Phase + XPos:Phase + YPos:Phase, data = NystedFarms, offset = log(Area), family = poisson)

summary(fullModel)

fullModel_OD <- glm(Count ~ XPos + YPos + DistCoast + Depth + FMonth + Phase + XPos:Phase + YPos:Phase, data = NystedFarms, offset = log(Area), family = quasipoisson)

summary(fullModel_OD)

library(car)
Anova(fullModel_OD, test = 'F')

reducedModel_OD <- glm(Count ~ XPos + YPos + Depth + FMonth + Phase + XPos:Phase + YPos:Phase, data = NystedFarms, offset = log(Area), family = quasipoisson)

summary(reducedModel_OD)
Anova(reducedModel_OD, test = 'F')

predict(reducedModel_OD, newdata = data.frame('XPos' = 670000, 'YPos' = 6050000, 'Depth' = 10, 'FMonth' = '3', 'Phase' = 'A', 'Area' = 1), type = 'link')
predict(reducedModel_OD, newdata = data.frame('XPos' = 670000, 'YPos' = 6050000, 'Depth' = 10, 'FMonth' = '3', 'Phase' = 'A', 'Area' = 1), type = 'response')
exp(0.581611)

plot(reducedModel_OD)
hist(NystedFarms$Count)
hist(NystedFarms$Count[NystedFarms$Count != 0])

acf(residuals(reducedModel_OD))
acf(residuals(reducedModel_OD, type = 'pearson'))

library(effects)
plot(allEffects(reducedModel_OD))
plot(allEffects(reducedModel_OD), selection = 1)

plot(Effect('Depth', reducedModel_OD), type = 'link')
plot(Effect('Depth', reducedModel_OD), type = 'response')
plot(Effect('Depth', reducedModel_OD, partial.residuals = TRUE), type = 'response')
plot(Effect('Depth', reducedModel_OD, partial.residuals = TRUE), type = 'response', ylim = c(0, exp(log(100))))

plot(Effect('FMonth', reducedModel_OD), type = 'link')
plot(Effect('FMonth', reducedModel_OD), type = 'response')
plot(Effect('FMonth', reducedModel_OD, partial.residuals = TRUE), type = 'response')

plot(Effect(c('XPos', 'Phase'), reducedModel_OD), type = 'link')
plot(Effect(c('XPos', 'Phase'), reducedModel_OD), type = 'response')
plot(Effect(c('XPos', 'Phase'), reducedModel_OD, partial.residuals = TRUE), type = 'response', ylim = c(0, exp(log(10))))

plot(Effect(c('YPos', 'Phase'), reducedModel_OD), type = 'link')
plot(Effect(c('YPos', 'Phase'), reducedModel_OD), type = 'response')
plot(Effect(c('YPos', 'Phase'), reducedModel_OD, partial.residuals = TRUE), type = 'response', ylim = c(0, exp(log(50))))