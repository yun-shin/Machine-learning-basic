
library(dplyr)

set.seed(11)
x <- c(runif(20,0,6), runif(20,5,9)) %>% round(2)
y <- rep(c(0,1),each=20)
df <- data.frame(x,y)
df <- df[sample(1:40,40),]
row.names(df) <- 1:40


reg <- glm(y ~ x, family = "binomial")
reg$coefficients
abline(reg1,col="green",lwd=5)

# 비용함수
n <- length(y)
f <- function(w0, w1, x) 1 / (1 + exp(-(w0 + w1 * x)))

# 벡터, 행렬로 표현
X <- cbind(1,matrix(x))
y <- as.matrix(y)

# 학습률, 반복횟수
alpha <- 0.2
num_iters <- 100000

# cost, w 저장
cost_history <- double(num_iters)
w_history <- list(num_iters)

# 초기화
w <- matrix(c(0,0), nrow=2)

# gradient descent
for (i in 1:num_iters) {
  grad <-  1 / length(y) * (t(X) %*% ((1 / (1 + (exp(-X %*% w)))) - y))
  w <- w - alpha * grad
  w_history[[i]] <- w
}
print(w)

# 산점도
plot(df$x, df$y, pch=16,
     xlab = "x", ylab="y",
     cex=1.5, col=rgb(0.2,0.4,0.6,0.4))

# 로지스틱 함수
curve(f(reg$coefficients[1], reg$coefficients[2], x), add=T, 0, 8,  col="blue", lwd=2)




