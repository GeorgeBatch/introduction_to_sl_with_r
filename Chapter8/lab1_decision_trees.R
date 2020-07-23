library(tree)  # classification and regeression trees
library(ISLR)  # datasets

attach(Carseats)

###############################################################################
# Classification trees
###############################################################################

High = ifelse(Sales <= 8, "No", "Yes")
Carseats = data.frame(Carseats, High)

# first model
tree.carseats = tree(High ~. -Sales, data=Carseats)
summary(tree.carseats)

# training error rate = 9%
# n = 400
# T_0 = 27 (# of terminal nodes)

plot(tree.carseats)
text(tree.carseats,  pretty = 0)

tree.carseats
# explicit tree

# creating test data
set.seed(1)
train = sample(1:nrow(Carseats), 200)
Carseats.test = Carseats[-train, ]
High.test = High[-train]

# fitting the model to train data
tree.carseats = tree(High ~. - Sales, data = Carseats, subset = train)
tree.pred = predict(tree.carseats, Carseats.test, type = "class")
table(tree.pred, High.test)

(84 + 44) / 200
# 64% correct

# can pruning the tree bring improvements?
set.seed(3)
cv.carseats = cv.tree(tree.carseats, FUN = prune.misclass)
names(cv.carseats)
cv.carseats
# dev corresponds to cv-error rate in this instance

par(mfrow = c(1, 2))
plot(cv.carseats$size, cv.carseats$dev, type = "b")
plot(cv.carseats$k, cv.carseats$dev, type = "b")

prune.carseats = prune.misclass(tree.carseats, best = 8)
par(mfrow = c(1,1))
plot(prune.carseats)
text(prune.carseats, pretty = 0)

tree.pred = predict(prune.carseats, Carseats.test, type="class")
table(tree.pred, High.test)
(83 + 57) / 200
# 70% correct now



# what if we increase best argument
prune.carseats = prune.misclass(tree.carseats, best = 15)
par(mfrow = c(1,1))
plot(prune.carseats)
text(prune.carseats, pretty = 0)

tree.pred = predict(prune.carseats, Carseats.test, type="class")
table(tree.pred, High.test)
(84 + 44) / 200
# 64% - decrease accuracy


###############################################################################
# Regression trees
###############################################################################
library(MASS)
set.seed(1)

train = sample(1:nrow(Boston), nrow(Boston) / 2)
tree.boston = tree(medv~., Boston, subset = train)  # regressin median value
summary(tree.boston)

plot(tree.boston)
text(tree.boston, pretty = 0)


cv.boston = cv.tree(tree.boston)
plot(cv.boston$size, cv.boston$dev, type = "b")
# the best one is the largest in this case, but we can still prune by varying 
# best argument
prune.boston = prune.tree(tree.boston, best = 5)
plot(prune.boston)
text(prune.boston, pretty = 0)


# evaluating on test set
yhat = predict(tree.boston, newdata = Boston[-train, ])
boston.test = Boston[-train, "medv"]
plot(yhat, boston.test)
abline(0, 1)
mse = mean((yhat-boston.test)^2)
# 35.28688 - test mse
(rmse = sqrt(mse))
