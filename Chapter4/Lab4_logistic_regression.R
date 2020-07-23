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
#                           Logistic regression
################################################################################

glm.fits <- glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume,
                data=Smarket, family=binomial)

# summary
summary(glm.fits)

# coefficients
summary(glm.fits)$coef

# p-values of the coefficients
summary(glm.fits)$coef[, 4]

# 1) no new data spesified - so the predictions are made to the training set
# 2) type="response" gives probabilities, not the logit values
glm.probs <- predict(glm.fits, type="response")
glm.probs[1:10]


contrasts(Direction)

glm.pred <- rep("Down",1250)
glm.pred[glm.probs >.5] <- "Up"

glm.probs[1:10]
glm.pred[1:10]


table(glm.pred,Direction)
(507 + 145) / 1250
# 0.5216 - wrong predictions (just a bit better than guessing randomly)
mean(glm.pred==Direction)
# 0.5216 - wrong predictions (just a bit better than guessing randomly)

# BUT this is train set error rate, which means that
# for different test sets it will be worse on average


################################################################################
#                     Assessing the performance
################################################################################

train <- (Year < 2005)
Smarket.2005 <-  Smarket[!train, ]
dim(Smarket.2005)
Direction.2005 <- Direction[!train]

glm.fits <- glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume,
                data=Smarket, subset = train,
                family=binomial)

glm.probs <- predict(glm.fits, newdata=Smarket.2005, type="response")

glm.pred <- rep("Down", length(glm.probs))
glm.pred[glm.probs >.5] <- "Up"
table(glm.pred,Direction.2005)

# Test set error rate
mean(glm.pred!=Direction.2005)
# [1] 0.5198413

# The results are rather disappointing: the test error rate is 52%,
# which is worse than random guessing!


################################################################################
#                   Removing predictors with low p-values
################################################################################


glm.fits <- glm(Direction~Lag1+Lag2,
                data=Smarket, subset=train,
                family=binomial)

glm.probs <- predict(glm.fits, newdata=Smarket.2005, type="response")

glm.pred <- rep("Down", 252)
glm.pred[glm.probs >.5] <-"Up"

table(glm.pred, Direction.2005)

mean(glm.pred==Direction.2005)
# We got 56% correctly classified

106/(106+76)
# However, the confusion matrix shows that on days when logistic regression
# predicts an increase in the market, it has a 58% accuracy rate.


predict(glm.fits, newdata=data.frame(Lag1=c(1.2,1.5),
                                     Lag2=c(1.1,-0.8)),
        type="response")