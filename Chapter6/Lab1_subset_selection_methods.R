################################################################################
#                         Best Subset selection
################################################################################
library(ISLR)
library(car)    # companion to applied regression
library(leaps)  # variable subset selection

options(scipen=999)


# ------------------------------------------------------------------------------
# First look at the data
# ------------------------------------------------------------------------------

names(Hitters)
# 20 names
str(Hitters)
# numeric and factor variables
dim(Hitters)
# 322  20
sum(is.na(Hitters$Salary))
# 59 NA values


Hitters <- na.omit(Hitters)
dim(Hitters)
# 263  20
sum(is.na(Hitters$Salary))
# 0

# ------------------------------------------------------------------------------
# Best subset selection (nvmax = 8 by default)
# ------------------------------------------------------------------------------

regfit.full <- regsubsets(Salary ~ ., Hitters)
summary(regfit.full)

# ------------------------------------------------------------------------------
# Best subset selection (nvmax = 19 - all variables)
# ------------------------------------------------------------------------------

regfit.very.full <- regsubsets(Salary ~ ., Hitters, nvmax = 19)
reg.summary <- summary(regfit.very.full)
names(reg.summary)

reg.summary$rsq

par(mfrow=c(2,2))

plot(reg.summary$rss, xlab="Number of Variables",
     ylab="RSS", type="l")

# Plotting R^2_adj
plot(reg.summary$adjr2, xlab="Number of Variables",
     ylab="Adjusted RSq", type="l")
which.max(reg.summary$adjr2)
# 11
points(11,reg.summary$adjr2[11], col="red",cex=2,pch=20)

# Plotting C_p
plot(reg.summary$cp, xlab="Number of Variables ",
     ylab="Cp", type="l")
which.min(reg.summary$cp )
# 10
points(10, reg.summary$cp[10], col="red", cex=2, pch=20)

# Plotting BIC
plot(reg.summary$bic, xlab="Number of Variables ",ylab="BIC",
     type="l")
which.min(reg.summary$bic )
# 6
points(6,reg.summary$bic[6],col="red",cex=2,pch=20)


# top row shows the best model
# can be used instead of summary (more condence)
par(mfrow = c(1,1))
plot(regfit.very.full, scale="r2")
plot(regfit.very.full, scale="adjr2")
plot(regfit.very.full, scale="Cp")
plot(regfit.very.full, scale="bic")

# looking up the coefficients 
coef(regfit.very.full, 6)


################################################################################
#                 Forward and Backward Stepwise Selection
################################################################################

regfit.fwd <- regsubsets(Salary ~ ., data=Hitters ,
                         nvmax=19, method ="forward")
summary(regfit.fwd)

regfit.bwd <- regsubsets(Salary ~ ., data=Hitters ,
                         nvmax=19, method ="backward")
summary(regfit.bwd)

# Different variables for 7-variable models
coef(regfit.very.full, 7)
coef(regfit.fwd, 7)
coef(regfit.bwd, 7)


################################################################################
#                   Model selection using validation set
################################################################################

# splitting data into test and train sets
set.seed(1)
train <- sample(x = c(TRUE,FALSE), nrow(Hitters), replace = TRUE)
test <- (!train )

# performing best subset selection
regfit.best <- regsubsets(Salary ~ ., data=Hitters[train, ], nvmax =19)

# ------------------------------------------------------------------------------
# Compute the validation set error for the best model of each model size.
# ------------------------------------------------------------------------------

# We first make a model matrix from the test data.
test.mat <- model.matrix(Salary ~ ., data=Hitters[test, ])

# for each of the model sizes
# take the names of the predictors in the best model of this size
# create a vector of predicted responces
# calculate the MSE for the test set
# record the MSE

val.errors=rep(NA,19)
for (i in 1:19){
  coefi <- coef(regfit.best, id = i)
  pred <- test.mat[, names(coefi)] %*% coefi
  val.errors[i] <- mean((Hitters$Salary[test]-pred)^2)
}

val.errors
which.min(val.errors)
coef(regfit.best, 10)

# now let's create a function, taking regsubsets 
predict.regsubsets <- function(object, newdata, id,...){
  form <- as.formula(object$call [[2]])
  mat <- model.matrix(form,newdata)
  coefi <- coef(object ,id=id)
  xvars <- names(coefi)
  mat[, xvars] %*% coefi
}

# Using validation, we have selected 10 - the best number of variables

# Now it's time to choose best ten-variable model using the full data set
# It is important to use the whole data set:
  # we can get differnet 10 variables selected
  # the coefficient will be more accurate using more data

regfit.best <- regsubsets(Salary ~ ., data=Hitters, nvmax=19)
coef(regfit.best, 10)


# In fact, we see that the best ten-variable model on the full data set has
# a different set of variables than the best ten-variable model on
# the training set.



################################################################################
#                   Model selection using cross-validation
################################################################################
K <- 10

set.seed(1)
subset <- rep(1:K, ceiling(nrow(Hitters)/K))[1:nrow(Hitters)]
subset <- sample(subset, size=nrow(Hitters), replace = FALSE)

cv.errors <- matrix(NA, K, 19, dimnames = list(NULL, paste(1:19)))

for (j in 1:K){
  best.fit <- regsubsets(Salary ~ ., data = Hitters[subset != j, ],
                         nvmax = 19)
  for (i in 1:19){
    pred <- predict.regsubsets(object = best.fit,
                               newdata = Hitters[subset == j, ], id = i)
    cv.errors[j, i] <- mean( (Hitters$Salary[subset == j] - pred)^2)
  }
}

mean.cv.errors <- apply(X = cv.errors, MARGIN = 2, FUN = mean)
mean.cv.errors


par(mfrow = c(1,1))
plot(mean.cv.errors, type = "b")

# cross-validation selects 11-variable model

reg.best <- regsubsets(Salary ~., data = Hitters, nvmax = 19)
coef(reg.best, 11)