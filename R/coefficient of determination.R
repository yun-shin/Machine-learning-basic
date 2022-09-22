
x1 <- c(14, 16, 13, 10, 18, 17, 16, 15, 11, 10)
x2 <- c(37, 43, 38, 42, 36, 33, 40, 35, 34, 29)
y <- c(850, 970, 730, 940, 920, 830, 940, 900, 760, 710)
df1 <- data.frame(x1,x2,y)

reg1 <- lm(y ~ x1 + x2, data=df1)
reg1$coefficients
lm(y ~ x1 + x2, data=df1) %>% summary

w0.hat <- reg1$coefficients[1]
w1.hat <- reg1$coefficients[2]
w2.hat <- reg1$coefficients[3]

f <- function(x1, x2)  w0.hat + (w1.hat %*% x1) + (w2.hat %*% x2)


SSE <- (df1$y - f(df1$x1, df1$x2))^2 %>% sum
SSR <- (f(df1$x1, df1$x2) - mean(df1$y))^2 %>% sum
SST <- (df1$y - mean(df1$y))^2 %>% sum

SSE + SSR

# R 제곱 구함
R.square <- SSR / SST
R.square1 <- 1 - SSE / SST
summary(reg2)$r.squared

# R 제곱 adj 값
R.square.adj <- 1 - (10 - 1) / (10-1-2) * (SSE / SST)
