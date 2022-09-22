
x <- matrix(c(1.2, 5.9, 2.3, 0.2))
y <- 13

f.1 <- function(z) 1/(1+exp(-z))
f.2 <- function(z) return(tanh(z))
f.3 <- function(z) {
  result <- ifelse(z < 0, 0.1 * z, z)
  return(result)
}

a.0 <- x
b.1 <- matrix(c(1.2, 1.2, 1.2, 1.2))

W.1 <- matrix(c(0.6, 0.6, 0.6, 0.6,
                0.6, 0.6, 0.6, 0.6,
                0.6, 0.6, 0.6, 0.6,
                0.6, 0.6, 0.6, 0.6), byrow = T, ncol=4)
b.2 <- b.1
W.2 <- W.1
W.3 <- matrix(c(2.7, 2.7, 2.7, 2.7), byrow = T, ncol=4)
b.3 <- 1.2

a.1 <- f.1(b.1 + W.1 %*% a.0)
a.2 <- f.2(b.2 + W.2 %*% a.1)
a.3 <- f.3(b.3 + W.3 %*% a.2)
a.3