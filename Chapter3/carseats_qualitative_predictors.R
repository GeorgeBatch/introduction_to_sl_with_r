library(ISLR)
library(car)

names(Carseats)
str(Carseats)


lm.fit <- lm(Sales ~ . + Income:Advertising + Price:Age,
             data=Carseats)
summary(lm.fit)

contrasts(Carseats$ShelveLoc)
?contrasts
