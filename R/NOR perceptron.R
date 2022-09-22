
perceptron <- function(x1, x2, w1, w2, b) {
  if(w1*x1 + w2*x2 + b <= 0) return(0)
  
  else return(1)
  
}

NOR <- function(x1, x2) {
  return(perceptron(x1, x2, -0.5, -0.5, 0.2))
}

NOR(0,0); NOR(0,1); NOR(1,0); NOR(1,1)

