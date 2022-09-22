
#install.packages("broom")

library(broom)
library(dplyr)

state <- as.data.frame(state.x77)

reg2 <- lm(Murder ~ Population + Income + Illiteracy + `Life Exp` + Frost, data=state)
reg2 %>% summary

w0.hat <- reg2$coefficients[1]
w1.hat <- reg2$coefficients[2]
w2.hat <- reg2$coefficients[3]
w3.hat <- reg2$coefficients[4]
w4.hat <- reg2$coefficients[5]
w5.hat <- reg2$coefficients[6]

p <- 5
SSE <- (state$Murder - reg2$fitted.values)^2 %>% sum
SSR <- (reg2$fitted.values - mean(state$Murder))^2 %>% sum
SST <- (state$Murder - mean(state$Murder))^2 %>% sum

MSR <- SSR / p
MSE <- SSE / ((state %>% nrow - 1) - p)

Source <- c("Model", "Error", "Total")
DF <- c(p, (state %>% nrow - 1) - p, state %>% nrow - 1)
SS <- c(SSR, SSE, SST)
MS <- c(MSR, MSE, NA)
`F` <- c(MSR / MSE, NA, NA)
`P-value` <- c(glance(reg2)$p.value, NA, NA)   # p-value 계산 함수
  
ANOVA <- data.frame(Source, DF, SS, MS, `F`, `P-value`)
ANOVA %>% View


