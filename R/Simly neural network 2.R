
library(dplyr)

load(file="/input_value.RData")

X <- cbind(1, input.value[,-10]) %>% as.matrix()  # 출력 변수 제외
y <- input.value$Y %>% matrix      # 출력변수 생성
w <- rep(0, ncol(X)) %>% matrix   # w값 10개 생성
lambda <- 0.3  # 람다 값 임의 지정

W <- t(w) %>% data.frame  # w 데이터프레임으로 변환 (저장용)
names(W) <- paste0("w", 0:(length(w)-1))  # w 이름 이쁘게 바꿔주기

repeat{
  for(i in 1:nrow(X)) {
    y.hat <- ifelse(X %*% w < 0, -1, 1)   # 퍼셉트론 특징, 계산 값이 일정 값 미만이면 -1
    w <- w + lambda * (y[i] - y.hat[i]) * t(X)[,i]  # 경사하강법 그대로 적용
    W <- rbind.data.frame(W, as.vector(w))  # 결과값 아카이빙
  }
  
  if(all(y == y.hat)) break   # 모든 예측값이 실제값과 동일할 때 까지
}


