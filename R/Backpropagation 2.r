
library(dplyr)

# Forward propagation computation
x <- matrix(c(2, 9))                              # x 값
y <- 0.82                                         # y 값

f1 <- function(z) 1/(1+exp(-z))                   # 로지스틱 함수
f2 <- function(z) tanh(z)                         # 쌍곡선 탄젠트 함수

a.0 <- x                                          # 데이터 input
b.1 <- matrix(c(0.1, 0.5), byrow = T, ncol = 1)   # layer1 bios input
b.2 <- matrix(c(3.9))                             # layer2 bios input
W.1 <- matrix(c(3, 0.7,                    
                0.5, 2), byrow = T, ncol=2)       # layer1 w 값 초기화
W.2 <- matrix(c(1.2, -7), byrow = T, ncol=2)      # layer2 w 값 초기화

a.1 <- f(b.1 + (W.1 %*% a.0))                     # layer1 출력값
a.2 <- f(b.2 + (W.2 %*% a.1))                     # layer2 출력값 ( 최종 출력값 )

for(i in 1:31) {                                  # 31번 반복
  a.1 <- f1(b.1 + W.1 %*% a.0)                    # layer1 출력값 계산
  a.2 <- f2(b.2 + W.2 %*% a.1) %>% as.vector      # layer2 출력값 계산
  y.hat <- a.2                                    # 예측값 저장
  
  # Back propagation computation
  alpha <- 0.37                                   # 학습률 저장
  delta.2 <- a.2 - y                              # 예측값 - 실제값
  
  gr.2 <- delta.2 * (1 - a.2^2) * rbind(1,a.1)    # 오차 * tanh 미분값 * 
  
  b.2 <- b.2 - alpha*gr.2[1]      # b.2 update
  W.2 <- W.2 - alpha*gr.2[-1]     # W.2 update
  
  delta.1 <- delta.2 * (1 - a.2^2) * W.2 
  
  gr.1 <- (t(delta.1)*a.1*(1-a.1)) %*% cbind(1,t(a.0))
  
  b.1 <- b.1 - alpha*gr.1[,1]      # b.1 update
  W.1 <- W.1 - alpha*gr.1[,-1]     # W.1 update
}
a.1 <- f1(b.1 + W.1 %*% a.0)
a.2 <- as.vector(f2(b.2 + W.2 %*% a.1))
y.hat <- a.2
y.hat

