library(MASS)
library(ISLR)
library(car)

summary(lm(medv ~ lstat*age, data=Boston))
# keep age (hierarchical principal)

# ------------------------------------------------------------------------------
#               Non-linear Transformations of the Predictors
# ------------------------------------------------------------------------------

lm.fit2 <- lm(medv~ lstat + I(lstat^2), data = Boston)
summary(lm.fit2)

lm.fit <- lm(medv~ lstat, data = Boston)
anova(lm.fit, lm.fit2)

par(mfrow=c(2,2))
plot(lm.fit2)


lm.fit5 <- lm(medv~ poly(lstat, 5), data = Boston)
summary(lm.fit5)
plot(lm.fit5)

anova(lm.fit2, lm.fit5)


summary(lm(medv~log(rm),data=Boston))
plot(lm(medv~log(rm),data=Boston))
