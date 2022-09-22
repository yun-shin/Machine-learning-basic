
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

XOR(0,0); XOR(0,1); XOR(1,0); XOR(1,1)


