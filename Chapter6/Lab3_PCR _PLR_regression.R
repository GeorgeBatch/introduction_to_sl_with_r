################################################################################
#                             Importing packages
################################################################################

library(ISLR)
library(car)    # companion to applied regression
library(pls)    # Principal Component and Partial Least Squares regressions

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
#                             PCR regression
################################################################################

# ------------------------------------------------------------------------------
# First look at model performance with full data set
# ------------------------------------------------------------------------------

set.seed(2)
pcr.fit <- pcr(Salary ~., data = Hitters,
               scale = TRUE,
               validation = "CV")

summary(pcr.fit)

# root of MSE is reported, so we need to square it to get MSE

validationplot(pcr.fit, val.type = "MSEP")

# ------------------------------------------------------------------------------
# Tuning hyperparameters with cross-validation
# ------------------------------------------------------------------------------

# test and train (train/validation) sets
set.seed(1)
train <- sample(x = 1:nrow(x), size = nrow(x)/2)
test <- (-train)

pcr.fit <- pcr(Salary ~., data = Hitters, subset = train, scale = TRUE,
               validation = "CV")
validationplot(pcr.fit, val.type = "MSEP")
summary(pcr.fit)
# gives lowest cross-validated MSE, when ncomp = 7 (look at summary(pcr.fit))


# ------------------------------------------------------------------------------
# Fitting (using betst parameter) on the test set to compare with other models
# ------------------------------------------------------------------------------

pcr.pred <- predict(pcr.fit, x[test, ], ncomp = 7)
mean((pcr.pred - y.test)^2)
# 96556.22 - comparable with ridge and lasso regressions


# ------------------------------------------------------------------------------
# Fitting on the full data set
# ------------------------------------------------------------------------------

pcr.fit <- pcr(y ~ x, scale = TRUE, ncomp = 7)
summary(pcr.fit)

# 46.69% of Salary variance explained

################################################################################
#                             PLS regression
################################################################################

# now let's do the same thing without the first look

# ------------------------------------------------------------------------------
# Tuning hyperparameters with cross-validation
# ------------------------------------------------------------------------------

set.seed(1)

train <- sample(x = 1:nrow(x), size = nrow(x)/2)
test <- (-train)

pls.fit <- plsr(Salary ~., data = Hitters, subset = train, scale = TRUE,
               validation = "CV")
summary(pls.fit)
validationplot(pls.fit, val.type = "MSEP")

# Resulting in ncomp = 2 giving the best performance

# ------------------------------------------------------------------------------
# Fitting (using betst parameter) on the test set to compare with other models
# ------------------------------------------------------------------------------

pls.fit <- plsr(y ~ x, scale = TRUE, ncomp = 2)
summary(pls.fit)

# 46.40% of Salary variance explained

# Notice that the percentage of variance in Salary that the two-component PLS
# fit explains, 46.40 %, is almost as much as that explained using the
# final seven-component model PCR fit, 46.69 %. This is because PCR only attempts
# to maximize the amount of variance explained in the predictors, while PLS
# searches for directions that explain variance in both the predic- tors and the
# response.

