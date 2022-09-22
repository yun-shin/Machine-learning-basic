set.seed(1)
x1 <- sample(2:30, 25)
set.seed(2)
x2 <- sample(36:1460, 25)
set.seed(3)
y <- round(2.341 + 1.6159*x1 + 0.015*x2 + rnorm(25,0,3.25),2)
df1 <- data.frame(x1,x2,y)
#View(df1)

reg <- lm(y ~ ., data = df1)
reg$coefficients

# 비용함수
n <- length(y)
cost <- function(X, y, w) t((y - X%*%w)) %*% (y - X%*%w)/(2*n)

# 벡터, 행렬로 표현
X <- cbind(1, as.matrix(df1[, 1:2]))
y <- as.matrix(y)

# 학습률, 반복횟수
alpha <- 0.000001
num_iters <- 200000

# cost, w 저장
cost_history <- double(num_iters)
w_history <- list(num_iters)

# 초기화
w <- matrix(c(0, 0, 0), nrow=3)

# gradient descent
for (i in 1:num_iters) {
  grad <- t(X) %*% (X %*% w - y) / n
  w <- w - alpha * grad
  cost_history[i] <- cost(X, y, w)
  w_history[[i]] <- w
}
print(w)

# 경사하강법 시각화
for (i in c(1,3,6,10,14,seq(20, num_iters, by=10))) {
  abline(coef=w_history[[i]], col=rgb(0.8,0,0,0.3))
}

# cost 변화
plot(cost_history, type='l',
     xlab='Iterations', ylab='Cost',
     col='orange', lwd=2)
