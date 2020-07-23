# 1 vs 1 approach with voting system

set.seed (1)
x <- rbind(x, matrix(rnorm(50*2), ncol=2))
y <- c(y, rep(0,50))
x[y==0,2] <- x[y==0,2]+2


dat <-data.frame(x=x, y=as.factor(y))
par(mfrow=c(1,1))
plot(x, col=(y+2), pch = 20)


svmfit=svm(y~., data=dat,
           kernel="radial", cost=10, gamma=1)
plot(svmfit , dat)
