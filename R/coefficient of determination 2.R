
set.seed(1)
x1 <- sample(2:30, 25)
set.seed(2)
x2 <- sample(36:1460, 25)
set.seed(3)
y <- round(2.341 + 1.6159*x1 + 0.015*x2 + rnorm(25,0,3.25),2)
df1 <- data.frame(x1,x2,y)
# View(df1)

reg1 <- lm(y ~ x1 + x2, data=df1)
reg1$coefficients
lm(y ~ x1 + x2, data=df1) %>% summary

w0.hat <- reg1$coefficients[1]
w1.hat <- reg1$coefficients[2]
w2.hat <- reg1$coefficients[3]


f <- function(x1, x2)  w0.hat + (w1.hat %*% x1) + (w2.hat %*% x2)
hat.y <- reg1$fitted.values # hat.y


SSE <- (df1$y - f(df1$x1, df1$x2))^2 %>% sum
SSR <- (f(df1$x1, df1$x2) - mean(df1$y))^2 %>% sum
SST <- (df1$y - mean(df1$y))^2 %>% sum

SSE <- (df1$y - hat.y)^2 %>% sum
SSR <- (hat.y - mean(df1$y))^2 %>% sum
SST <- (df1$y - mean(df1$y))^2 %>% sum

SSE + SSR

# R 제곱 구함
R.square <- SSR / SST
R.square1 <- 1 - SSE / SST

R.square.adj <- 1 - (25 - 1) / (25-1-2) * (SSE / SST)
# n = 데이터 갯수
# p = 변수 갯수
# R.square.adj <- 1 - (n - 1) / (n - 1 - p) * (SSE / SST)










