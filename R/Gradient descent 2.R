
# install.packages("Deriv")
library(Deriv)

f <- expression((x1 - 4)^2 + (x3^2 * x1) + (x2 + 1)^2 + 6)   # 수식으로 미분, 수식은 미지수 다 몰라도댐
df.dx <- c(D(f, "x1"), D(f, "x2"), D(f, "x3"))     # 수식 미분

f <- deriv(~(x1 - 4)^2 + (x3^2 * x1) + (x2 + 1)^2 + 6, c("x1", "x2", "x3"), func=T)  # 진짜 미분 모든 미지수 다 알아야함
a <- 0.1

x <- c(5, 5, 5)

x1.value <- x[1]
x2.value <- x[2]
x3.value <- x[3]

# 2000번 경사하강법 실시
for (i in 1:2000) {
  gradient <-  attributes(f(x[1], x[2], x[3]))$gradient
  
  x <- x - (a * gradient)   # x값 갱신
  
  # x 값이 변화하는 과정 기록
  x1.value <- append(x1.value, x[1])
  x2.value <- append(x2.value, x[2])
  x3.value <- append(x3.value, x[3])
}

x.df <- data.frame(x1.value, x2.value, x3.value)
x.df %>% View

