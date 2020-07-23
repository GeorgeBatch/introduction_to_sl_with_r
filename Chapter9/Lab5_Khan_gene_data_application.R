library(ISLR)

# Task: which of the 4 types of cancer does a person have?
# Make a choice using the gene information (2803 variables)
names(Khan)
# [1] "xtrain" "xtest"  "ytrain" "ytest" 

dim(Khan$xtrain)
# 63 2308
dim(Khan$xtest)
# 20 2308

# 2308 genes, 83 observations

table(Khan$ytrain)
#   1  2  3  4 
#   8 23 12 20 

table(Khan$ytest)
# 1 2 3 4 
# 3 6 6 5 


dat <- data.frame(x=Khan$xtrain, y=as.factor(Khan$ytrain))
out <- svm(y~., data=dat, kernel="linear", cost=10)
summary(out)

table(out$fitted, dat$y)
#     1  2  3  4
# 1  8  0  0  0
# 2  0 23  0  0
# 3  0  0 12  0
# 4  0  0  0 20
# 
# No mistakes! we definitely overfitted a lot


dat.te <- data.frame(x=Khan$xtest, y=as.factor(Khan$ytest))
pred.te <- predict(out, newdata=dat.te)
table(pred.te, dat.te$y)

# pred.te 1 2 3 4
# 1       3 0 0 0
# 2       0 6 2 0
# 3       0 0 4 0
# 4       0 0 0 5