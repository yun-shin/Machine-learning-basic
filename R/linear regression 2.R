
library(magrittr)
library(rootSolve)

X <- c(36.5, 28.0, 42.9, 52.0, 51.5, 53.8, 25.4, 37.2, 50.9, 29.2)
Y <- c(14, 9, 15, 20, 21, 25, 9, 13, 20, 10 )

reg1 <- lm(Y ~ X)
reg1$coefficients

w0.hat <- reg1$coefficients[1]
w1.hat <- reg1$coefficients[2]

f <- function(X)  w0.hat + w1.hat %*% X
plot (f, -100, 100, col="red", xlab="x", ylab="y")

g <- function(X) f(X) - 1700

uniroot(g, c(-1, 4000))$root


