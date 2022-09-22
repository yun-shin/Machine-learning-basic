
library(magrittr)
library(rootSolve)

X <- c(1380, 3120, 3520, 1130, 1030, 1720, 3920, 1490, 1860, 3430, 2000, 3660, 2500, 1220, 1390)
Y <- c(  76, 216,  238,   69,   50,  119,  282,   81,  132,  228,  145,  251,  170,   71, 29)

#x <- as.matrix(X)
#y <- as.matrix(Y)

# 이건 공식 적용해서 작성함, 값은 비슷함
# w1.hat <- ((X - mean(X)) * (Y - mean(Y))) %>% sum / (((X - mean(X))^2) %>% sum)
# w0.hat <- mean(Y) - w1.hat * mean(X)

reg1 <- lm(Y ~ X)
reg1$coefficients

w0.hat <- reg1$coefficients[1]
w1.hat <- reg1$coefficients[2]

res <- Y-(w0.hat+w1.hat %*% X) # 잔차
mean(res)                  # 확률오차, 즉 0에 가까울수록 좋음

f <- function(X)  w0.hat + w1.hat %*% X
plot(X, Y, xlab="Area", ylab="Appraised Value")
plot (f, 1000, 4000, col="red", xlab="Area", ylab="Appraised Value", add=T)

# 1. 집 크기가 2227 m2 일 때, 집가격을 예측하라.
f(2227)

# 2. 현재 150 억이 있으면 얼마나 큰 집을 살 수 있는가?
g <- function(X)  f(X) - 150
curve (g, 1000, 4000, col="red", xlab="Area", ylab="Appraised Value")
uniroot(g,c(-1, 4000))$root

# 3. 각 관측에서의 잔차를 구하고, 잔차들의 합을 구하라.
res <- Y-(w0.hat+w1.hat %*% X)        # 잔차
sum(res)                              # 확률오차, 즉 0에 가까울수록 좋음






