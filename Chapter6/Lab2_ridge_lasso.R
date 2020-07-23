################################################################################
#                             Importing packages
################################################################################

library(ISLR)
library(car)    # companion to applied regression
library(glmnet) # ridge and lasso regressions

options(scipen=999)

################################################################################
#                         Sorting out the data
################################################################################

Hitters <- na.omit(Hitters)

x <- model.matrix(Salary ~., Hitters)[, -1]
y <- Hitters$Salary

str(x)
summary(x)

################################################################################
#                             Ridge regression
################################################################################


# ------------------------------------------------------------------------------
# Fitting Ridge regression for different values of Lambda
# ------------------------------------------------------------------------------

grid <- 10^seq(10, -2, length = 100)
ridge.mod <- glmnet(x, y, alpha = 0, lambda = grid)

# alpha=1 - lasso penalty
# alpha=0 - ridge penalty

# by default we have standardisation of variables
# (standardize=FALSE to turn off)

# ------------------------------------------------------------------------------
# What do different lambdas do?
# ------------------------------------------------------------------------------

dim(coef(ridge.mod))
# 20 predictors (19 + intersept)
# 100 values of lambda

ridge.mod$lambda[50]
# 11497.57 - value of lambda

coef(ridge.mod)[, 50]
# (Intercept)         AtBat          Hits         HmRun          Runs 
# 407.356050200   0.036957182   0.138180344   0.524629976   0.230701523 
# RBI         Walks         Years        CAtBat         CHits 
# 0.239841459   0.289618741   1.107702929   0.003131815   0.011653637 
# CHmRun         CRuns          CRBI        CWalks       LeagueN 
# 0.087545670   0.023379882   0.024138320   0.025015421   0.085028114 
# DivisionW       PutOuts       Assists        Errors    NewLeagueN 
# -6.215440973   0.016482577   0.002612988  -0.020502690   0.301433531 

sqrt(sum(coef(ridge.mod)[-1, 50]^2))
# [1] 6.360612



ridge.mod$lambda[60]
# [1] 705.4802
coef(ridge.mod)[, 60]
sqrt(sum(coef(ridge.mod)[-1, 60]^2))
# [1] 57.11001




# new value of lambda, say 50
predict(ridge.mod, s = 50, type = "coefficients")[1:20, ]


# ------------------------------------------------------------------------------
# Assessing performance of our model with different values of lambda
# ------------------------------------------------------------------------------

set.seed(1)
train <- sample(x = 1:nrow(x), size = nrow(x)/2)
test <- (-train)

y.test <- y[test]

ridge.mod <- glmnet(x[train, ], y[train], alpha = 0, lambda = grid,
                    thresh = 1e-12)
# predicting for lambda = 4, because it's new, we do it with (s = 4)
ridge.pred <- predict(ridge.mod, s = 4, newx = x[test, ])
# calculating the MSE
mean((ridge.pred - y.test)^2)
# [1] 101036.8

# null model: use only the intercept, which is mean(y[train])
mean((mean(y[train]) - y.test)^2)
# 193253.1

# this is equivalent to fitting a model letting lambda to Infinity
ridge.pred <- predict(ridge.mod, s = 1e10, newx = x[test, ])
mean((ridge.pred - y.test)^2)
# 193253.1


# If we want to fit a model exactly with new value of lambda,
# we need to set (exact = TRUE).
# Otherwise the result will be interpolated from the values in the grid.

ridge.pred <- predict(ridge.mod, x = x[train, ], y = y[train],
                      s = 0, newx = x[test, ], exact = TRUE)
mean((ridge.pred - y.test)^2)
# 114783.1


# ------------------------------------------------------------------------------
# Choosing best value of lambda using cross-validation
# ------------------------------------------------------------------------------

set.seed(1)
cv.out <- cv.glmnet(x[train, ], y[train], alpha = 0)
plot(cv.out)

best.lam <- cv.out$lambda.min
best.lam
# 212
log(best.lam)
# 5.355367 - the dotted line

ridge.pred <- predict(ridge.mod, s = best.lam, newx = x[test, ])
mean((ridge.pred - y.test)^2)
# 96015.51

out <- glmnet(x, y, alpha = 0)
predict(out, type="coefficients", s = best.lam)
# result as matrix
predict(out, type="coefficients", s = best.lam)[1:20, ]
# result as vector


################################################################################
#                             Lasso regression
################################################################################

lasso.mod <- glmnet(x[train, ], y[train], alpha = 1, lambda = grid)
plot(lasso.mod)

# choosing lambda with cross-validation
set.seed(1)
cv.out <- cv.glmnet(x[train, ], y[train], alpha = 1)
plot(cv.out)

best.lam <- cv.out$lambda.min
best.lam
log(best.lam)

lasso.pred <- predict(lasso.mod, s = best.lam, newx = x[test, ])
mean((lasso.pred - y.test)^2)
# 100743.4

# Much better than without regularisation
# Similar to the MSE given by ridge regression

out <- glmnet(x, y, lambda = grid)
lasso.coef <- predict(out, type = "coefficients", s = best.lam)[1:20, ]
lasso.coef[lasso.coef != 0]
