set.seed(1)
x <- matrix(rnorm(20*2), ncol = 2)
y <- c(rep(-1,10), rep(1,10))
x[y==1,] <- x[y==1,] + 1
plot(x, col=(3-y))

dat <- data.frame(x=x, y=as.factor(y))
library(e1071)

#-------------------------------------------------------------------------------
# cost 10
# 
svmfit <- svm(y~., data=dat, kernel="linear", cost=10,
              scale=FALSE)
plot(svmfit, dat)

# 7 support vectors
svmfit$index
# summary
summary(svmfit)


#-------------------------------------------------------------------------------
# cost 0.1
# 
svmfit <- svm(y~., data=dat, kernel="linear", cost=0.1,
              scale=FALSE)
plot(svmfit, dat)

# 16 support vectors - smoller cost <=> wider margin
svmfit$index


#-------------------------------------------------------------------------------
# tuning the cost via 10-fold cross-validation
# 
set.seed(1)
tune.out <- tune(svm, y~., data=dat, kernel="linear", 
                 ranges=list(cost=c(0.001, 0.01, 0.1, 1,5,10,100)))
summary(tune.out)
# best parameters: cost 0.1


bestmod <- tune.out$best.model
summary(bestmod)

#-------------------------------------------------------------------------------
# testing our method
# 

xtest <- matrix(rnorm(20*2), ncol=2)
ytest <- sample(c(-1,1), 20, rep=TRUE)
xtest[ytest==1,] <- xtest[ytest==1,] + 1
testdat=data.frame(x=xtest, y=as.factor(ytest))


ypred<- predict(object = bestmod, newdata = testdat)
table(predict=ypred, truth=testdat$y)
# 19 out of 20 correct predictions

# what if we use 0.01 as cost
svmfit <- svm(y~., data=dat, kernel="linear", cost=0.01,
              scale=FALSE)
ypred <- predict(object = svmfit, newdata = testdat)
table(predict=ypred, truth=testdat$y)
# worse result than for the best model


#-------------------------------------------------------------------------------
# linear-separable case
# 

x[y==1,] <- x[y==1,] + 0.5
plot(x, col=(y+5)/2, pch=19)
dat <- data.frame(x=x,y=as.factor(y))


# very high cost <=> very narrow margin <=> very few vectors on the margin
# <=> low bias, high variance <=> poor performance on the test data
svmfit<- svm(y~., data=dat, kernel="linear", cost=1e5)
summary(svmfit)
plot(svmfit , dat)

# lower cost
svmfit=svm(y~., data=dat, kernel="linear", cost=1)
summary(svmfit)
plot(svmfit, dat)