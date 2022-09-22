
perceptron <- function(x1, x2, w1, w2, b) {
  if(w1*x1 + w2*x2 + b <= 0) return(0)
  
  else return(1)
  
}

NAND <- function(x1, x2) {
  return(perceptron(x1, x2, -0.5, -0.5, 0.7))
}

NAND(0,0); NAND(0,1); NAND(1,0); NAND(1,1)
