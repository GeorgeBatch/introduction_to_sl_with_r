library(MASS)
library(ISLR)

?Boston
names(Boston)
# 14 cloumns 
str(Boston)
# all numeric (one binary categorical)
dim(Boston)
# 506 * 14
summary(Boston)


lm.fit <- lm(medv ~ lstat, data = Boston)
lm.fit
summary(lm.fit)

names(lm.fit)
coef(lm.fit)

confint(lm.fit, level = 0.95)

predict(lm.fit,
        data.frame(lstat=c(5,10,15)),
        interval="confidence")

predict(lm.fit,
        data.frame(lstat=c(5,10,15)),
        interval="prediction")


plot(Boston$lstat, Boston$medv)
abline(lm.fit)

abline(lm.fit,lwd=3)
abline(lm.fit,lwd=3,col="red")

plot(Boston$lstat,Boston$medv,col="red")
plot(Boston$lstat,Boston$medv,pch=20)
plot(Boston$lstat,Boston$medv,pch="+")
plot(1:20,1:20,pch=1:20)


# Diagnostic plots
par(mfrow=c(2,2))
plot(lm.fit)


plot(predict(lm.fit), residuals(lm.fit))
plot(predict(lm.fit), rstudent(lm.fit))

plot(hatvalues(lm.fit))
which.max(hatvalues(lm.fit))
# Observation 375 has the highest leverage