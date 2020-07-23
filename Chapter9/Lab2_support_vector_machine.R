set.seed(1)
x <- matrix(rnorm(200*2), ncol = 2)
x[1:100, ] <- x[1:100, ] + 2
x[101:150, ] <- x[101:150, ] - 2
y <- c(rep(1,150), rep(2,50))

dat <- data.frame(x=x,y=as.factor(y))
plot(x, col=y+1)

# ------------------------------------------------------------------------------
# training and tuning hyperparameters of the model
# 

# 50-50 split into test and train data
train=sample(200,100)

# fitting the model on the train set with:
  # small cost=1 <=> high bias, low variance <=>
  # <=> wide margin <=> many support vectors
svmfit <- svm(y~., data=dat[train,], kernel="radial", gamma=1,
           cost =1)
plot(svmfit , dat[train ,])

# model summary
summary(svmfit)

# fitting the model on the train set with:
  # high cost=10^5 <=> low bias, high variance <=>
  # <=> narrow margin <=> very few support vectors
svmfit <- svm(y~., data=dat[train,], kernel="radial", gamma=1,
              cost =1e5)
plot(svmfit , dat[train ,])

# choosing the value of cost and gamma (kernel hyperparameter)
# via grid search - 10-fold cross-validation using tune()
set.seed (1)
tune.out <- tune(svm, y~., data=dat[train,], kernel="radial",
                 ranges=list(cost=c(0.1,1,10,100,1000),
                             gamma=c(0.5,1,2,3,4)))
tune.out$best.parameters
# cost  gamma
# 1     2
tune.out$best.performance
# best performance : 0.12


# ------------------------------------------------------------------------------
# testing
# 

table(true=dat[-train,"y"],
      pred=predict(tune.out$best.model,
                   newdata=dat[-train ,])
      )

