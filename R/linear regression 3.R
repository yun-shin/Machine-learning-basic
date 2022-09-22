library(dplyr)

state <- as.data.frame(state.x77)
df1 <- data.frame (1, state[, names(state) %in%  c("Population", "Income", "Illiteracy", "Life Exp", "Frost")])


X <- as.matrix(df1)
Y <- matrix(state$Murder)


# 1. 다중선형회귀분석을 실시하라.
reg2 <- lm(Murder ~ Population + Income + Illiteracy + `Life Exp` + Frost, data=state)
reg2$coefficients

# hat_w <- (t(X) %*% X) %>% solve %*% t(X) %*% Y

# 2. 각 관측치에서 잔차를 구하고, 잔차들의 합을 구하라.
sum(reg2$residuals) # 잔차

# res <- Y-(X %*% hat_w) 
# sum(res)




