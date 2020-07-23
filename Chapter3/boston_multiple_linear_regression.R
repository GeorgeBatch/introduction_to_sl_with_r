library(MASS)
library(ISLR)

lm.fit <- lm(medv ~ lstat + age, data=Boston)
summary(lm.fit)

lm.fit <- lm(medv ~ ., data=Boston)
summary(lm.fit)

library(car)
vif(lm.fit)

# age has really high p-value
lm.fit1=lm(medv ~ .-age,data=Boston)
summary(lm.fit1)

