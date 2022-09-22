
perceptron <- function(x1, x2, w1, w2, b) {
  if(w1*x1 + w2*x2 + b <= 0) return(0)
  
  else return(1)
}

NOT <- function(x) {
  ifelse(x == 1, 0, 1)
}

AND <- function(x1, x2) {
  return(perceptron(x1, x2, 0.5, 0.5, -0.7))
}

OR <- function(x1, x2) {
  return(perceptron(x1, x2, 0.5, 0.5, -0.2))
}

XNOR <- function(x1, x2) {
  return(OR(AND(x1, x2), AND(NOT(x1), NOT(x2))))
}

XNOR(0,0); XNOR(0,1); XNOR(1,0); XNOR(1,1)
