library(gbm)

set.seed(1)
train = sample(nrow(Boston), nrow(Boston) / 2)

# fitting on train set
boost.boston = gbm(medv~., data = Boston[train, ], distribution = "gaussian",
                   n.trees = 5000, interaction.depth = 4)
summary(boost.boston)


# plotting dependency of the responce on the predictor
# (everything else integrated out)
par(mfrow = c(1, 2))
plot(boost.boston, i="rm")
plot(boost.boston, i="lstat")


# evaluating on the test set
yhat.boost = predict(boost.boston, newdata = Boston[-train, ], n.trees = 5000)
(mse = mean((yhat.boost - boston.test)^2))
(rmse = sqrt(mse))
# similar performance to rf



# fitting on train set with different regularisation
boost.boston = gbm(medv~., data = Boston[train, ], distribution = "gaussian",
                   n.trees = 5000, interaction.depth = 4,
                   shrinkage = 0.2, verbose = FALSE)
summary(boost.boston)
# evaluating on the test set
yhat.boost = predict(boost.boston, newdata = Boston[-train, ], n.trees = 5000)
(mse = mean((yhat.boost - boston.test)^2))
(rmse = sqrt(mse))
# worse performance