library(dplyr)

perceptron <- function(x1, x2, w1, w2, b) {
  if(w1*x1 + w2*x2 + b <= 0) return(0)
  
  else return(1)
}

NAND <- function(x1, x2) {
  return(perceptron(x1, x2, -0.5, -0.5, 0.7))
}

AND <- function(x1, x2) {
  return(perceptron(x1, x2, 0.5, 0.5, -0.7))
}

OR <- function(x1, x2) {
  return(perceptron(x1, x2, 0.5, 0.5, -0.2))
}

XOR <- function(x1, x2) {
  return(AND(NAND(x1, x2), OR(x1, x2)))
}

XOR.3.input <- function(x1, x2, x3) {
  return(XOR(XOR(x1, x2), x3))
}

x1 <- c(0, 0, 0, 0, 1, 1, 1, 1)
x2 <- c(0, 0, 1, 1, 0, 0, 1, 1)
x3 <- c(0, 1, 0, 1, 0, 1, 0,1)
y <- c(
  XOR.3.input(0, 0, 0),
  XOR.3.input(0, 0, 1),
  XOR.3.input(0, 1, 0),
  XOR.3.input(0, 1, 1),
  XOR.3.input(1, 0, 0),
  XOR.3.input(1, 0, 1),
  XOR.3.input(1, 1, 0),
  XOR.3.input(1, 1, 1)
)

df <- data.frame(x1, x2, x3, y)
