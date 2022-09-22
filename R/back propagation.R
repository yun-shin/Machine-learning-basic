library(dplyr)
# Forward propagation computation
x <- matrix(c(7,8))
y <- 0.4

f <- function(z) 1/(1+exp(-z))

a.0 <- x
b.1 <- matrix(c(0.1, 0.5))
b.2 <- matrix(c(0.2))
W.1 <- matrix(c(0.3, 0.07,
                0.5, 0.4), byrow = T, ncol=2)
W.2 <- matrix(c(0.2, 0.6), byrow = T, ncol=2)

a.1 <- f(b.1 + (W.1 %*% a.0))
a.2 <- f(b.2 + (W.2 %*% a.1))

for(i in 1:1000) {
  a.1 <- f(b.1 + W.1 %*% a.0)
  a.2 <- f(b.2 + W.2 %*% a.1) %>% as.vector
  y.hat <- a.2
  
  # Back propagation computation
  alpha <- 0.3
  delta.2 <- a.2 - y
  
  gr.2 <- delta.2*a.2*(1-a.2) * rbind(1,a.1)
  
  b.2 <- b.2 - alpha*gr.2[1]      # b.2 update
  W.2 <- W.2 - alpha*gr.2[-1]     # W.2 update
  
  delta.1 <- delta.2 * a.2 * (1-a.2) * W.2 
  
  gr.1 <- (t(delta.1)*a.1*(1-a.1)) %*% cbind(1,t(a.0))
  
  b.1 <- b.1 - alpha*gr.1[,1]      # b.1 update
  W.1 <- W.1 - alpha*gr.1[,-1]     # W.1 update
}
a.1 <- f(b.1 + W.1 %*% a.0)
a.2 <- f(b.2 + W.2 %*% a.1) %>% as.vector
y.hat <- a.2
Y.hat

