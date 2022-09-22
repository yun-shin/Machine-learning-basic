install.packages("dplyr")
library(dplyr)

set.seed(1)
x1 <- sample(2:30, 25)
set.seed(2)
x2 <- sample(36:1460, 25)
set.seed(3)
y <- round(2.341 + (1.6159 * x1) + (0.015 * x2) + rnorm(25,0,3.25),2)
df1 <- data.frame(x1,x2,y)
#View(df1)

df2 <- data.frame(1, x1, x2)
X <- as.matrix(df2)
Y <- matrix(y)


reg2$residuals # 잔차

# 1. 다중선형회귀분석을 실시하라.
reg2 <- lm(y ~ x1 + x2, data=df1)
reg2$coefficients

# 이건 공식 적용해서 구한 값.
# hat_w <- (t(X) %*% X) %>% solve %*% t(X) %*% Y


# 2. 각 관측치에서 잔차를 구하고, 잔차들의 합을 구하라.
sum(reg2$residuals) # 잔차


# res <- Y-(X %*% hat_w) 
# sum(res)






