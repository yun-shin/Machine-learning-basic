
# install.packages("caret")
# install.packages("ggplot2")
# install.packages("lattice")
# install.packages('caret', dependencies = TRUE)

library(dplyr)
library(ggplot2)
library(lattice)
library(caret)

mtcars <- mtcars

reg <- glm(mtcars$vs ~ mtcars$mpg + mtcars$cyl + mtcars$wt, family = "binomial")
reg$coefficients


n <- length(mtcars$vs)
f <- function(X, w) 1 / (1 + (exp(-X %*% w)))   # 학습 모델

# 벡터, 행렬로 표현
X <- cbind(1, as.matrix(mtcars$mpg), as.matrix(mtcars$cyl), as.matrix(mtcars$wt))
y <- as.matrix(mtcars$vs)

# 학습률, 반복횟수
alpha <- 0.02
num_iters <- 1000000

# cost, w 저장
cost_history <- double(num_iters)
w_history <- list(num_iters)

# 초기화
w <- matrix(c(8, 1, -1, 3), nrow=4)

# gradient descent
for (i in 1:num_iters) {
  grad <-  1 / length(y) * (t(X) %*% ((1 / (1 + (exp(-X %*% w)))) - y)) # 학습모델 미분 식
  w <- w - alpha * grad
  w_history[[i]] <- w
}

# 값 비교
print(w)
reg$coefficients

pred <- ifelse (f(X, w) %>% matrix < 0.5, 0, 1)   # 내 모델의 예상 값

real.value <- mtcars$vs %>% as.factor
pred.value <- pred %>% as.factor
confusionMatrix(pred.value, real.value)

