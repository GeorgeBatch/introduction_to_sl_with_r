library(ROCR)
rocplot <- function(pred, truth, ...){
  predob = prediction(pred, truth)
  perf = performance(predob , "tpr", "fpr")
  plot(perf ,...)
}

svmfit.opt <- svm(y~., data=dat[train,],
                  kernel="radial", gamma=2, cost=1,
                  decision.values=T)
fitted <- attributes(
  predict(svmfit.opt,dat[train,], decision.values=TRUE))$decision.values


par(mfrow=c(1,2))
rocplot(fitted, dat[train ,"y"], main="Training Data")

# greater value of gamma makes the decision boundary more dependent on
# individual close-by observations dectreasing the bias
# and increasing the variance
svmfit.flex <- svm(y~., data=dat[train,],
                   kernel="radial", gamma=50, cost=1,
                   decision.values=T)
fitted <- attributes(
  predict(svmfit.flex, dat[train,], decision.values=T))$decision.values
rocplot(fitted, dat[train ,"y"], add=T, col="red")


# test
fitted=attributes(
  predict(svmfit.opt,dat[-train,],decision.values=T))$decision.values
rocplot(fitted, dat[-train,"y"], main="Test Data")
fitted=attributes(
  predict(svmfit.flex, dat[-train,], decision.values=T))$decision.values
rocplot(fitted,dat[-train,"y"], add=T, col="red")