# install.packages('e1071')
library(e1071)

set.seed(1)

x = matrix(rnorm(20*2), ncol=2)
y = c(rep(-1,10), rep(1,10))
x[y==1,] =x[y==1,] + 1
plot(x, col=(3-y))

dat = data.frame(x=x, y=as.factor(y))

svmfit = svm(y~., data=dat, kernel="linear", cost=.1, scale=FALSE)

plot(svmfit, dat)

svmfit$index

summary(svmfit)
