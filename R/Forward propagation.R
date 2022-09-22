f1 <- function(z) 1/(1+exp(-z))
f2 <- function(z) tanh(z)
f3 <- function(z) pmax(0.1*z, z)

x <- matrix(c(1,1.2,5.9,2.3,0.2))
wij <- 0.6
b <- 1.2
w3ij <- 2.7

W.1 <- W.2 <- matrix(rep(b,4)) %>% cbind(matrix(rep(wij,16), byrow=T, ncol=4))
W.3 <- matrix(b) %>% cbind(matrix(rep(w3ij,4),byrow = T, ncol = 4))
#a.1 <- a.2 <- rep(NA,4)

#for(i in 1:4) a.1[i] <- f1(W.1[i,] %*% x)
#a.1 <- c(1,a.1)
a.1 <- rbind(1, f1(W.1 %*% x))

#for(i in 1:4) a.2[i] <- f2(W.2[i,] %*% a.1)
#a.2 <- c(1,a.2)
a.2 <- rbind(1, f2(W.2 %*% a.1))

a.3 <- f3(W.3 %*% a.2)
(13-a.3)^2