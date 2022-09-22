
library(magrittr)
library(rootSolve)

X <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
Y <- c(3, 3, 3, 6, 6, 9, 9, 9, 10, 11)

reg1 <- lm(Y ~ X)
reg1$coefficients

w0.hat <- reg1$coefficients[1]
w1.hat <- reg1$coefficients[2]

f <- function(X)  w0.hat + w1.hat %*% X
plot (f, -100, 100, col="red", xlab="x", ylab="y")

f(11)


