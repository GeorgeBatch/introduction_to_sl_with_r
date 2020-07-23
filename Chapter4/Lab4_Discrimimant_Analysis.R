################################################################################
#                     Loading and exploring the data
################################################################################

library(ISLR)
names(Smarket)
dim(Smarket)
summary(Smarket)
pairs(Smarket)
cor(Smarket[, -9])

attach(Smarket)
plot(Volume)

################################################################################
#                     Linear Discriminant Analysis
################################################################################


library(MASS)
lda.fit <- lda(Direction ~ Lag1 + Lag2,
               data=Smarket, subset=train)
lda.fit

plot(lda.fit)


lda.pred <- predict(lda.fit, Smarket.2005)
names(lda.pred)

lda.class <- lda.pred$class

table(lda.class, Direction.2005)
mean(lda.class == Direction.2005)

sum(lda.pred$posterior[,1]>=.5)
# 70
sum(lda.pred$posterior[,1]<=.5)
# 182

# probabilities of going down
lda.pred$posterior[1:20,1]

# changing the threashhold
sum(lda.pred$posterior[,1]>.9)

################################################################################
#                     Quadratic Discriminant Analysis
################################################################################

qda.fit <- qda(Direction ~ Lag1 + Lag2,
                data=Smarket, subset=train)
qda.fit

qda.class <- predict(qda.fit, Smarket.2005)$class
table(qda.class, Direction.2005)

mean(qda.class==Direction.2005)
