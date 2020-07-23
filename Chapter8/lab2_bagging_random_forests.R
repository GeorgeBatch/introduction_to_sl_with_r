source("lab1_decision_trees.R")

library(randomForest)
set.seed(1)

# bagging - random forest with m = p = ncol(data)-1
bag.boston = randomForest(medv~., data = Boston, subset = train,
                          mtry=ncol(Boston)-1, importance=TRUE)
bag.boston


# performance on test set
yhat.bag = predict(bag.boston, newdata = Boston[-train, ])
plot(yhat.bag, boston.test)
abline(0, 1)
(mse = mean((yhat.bag - boston.test)^2))
(rmse = sqrt(mse))


# changing ntree argument gives slight imprrovement
bag.boston = randomForest(medv~., data = Boston, subset = train,
                          mtry = ncol(Boston)-1, ntree = 25)
yhat.bag = predict(bag.boston, newdata = Boston[-train, ])
(mse = mean((yhat.bag - boston.test)^2))
(rmse = sqrt(mse))


# random forrests default m = p/3 for regression, m = sqrt(p) for classification
set.seed(1)
rf.boston = randomForest(medv~., data = Boston, subset = train,
                         mtry = 6, importance = TRUE)
yhat.rf = predict(rf.boston, newdata = Boston[-train, ])
(mse = mean((yhat.rf - boston.test)^2))
(rmse = sqrt(mse))
# improvement compared to the last bagging solution

importance(rf.boston)

varImpPlot(rf.boston)
