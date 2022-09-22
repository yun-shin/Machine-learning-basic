
library(dplyr)
library(caret)
library(rpart)
library(rpart.plot)

set.seed(1234)
samp <- createDataPartition(iris$Species, p=0.7, list=F)
iris.train <- iris[samp, ]
row.names(iris.train) <- 1:nrow(iris.train)
iris.test <- iris[-samp,]


fit <- rpart(Species ~ ., data = iris.train, method = 'class')
plot(fit)
text(fit, use.n = TRUE)


rpart.plot(fit, extra = 101)    # 의사결정 나무 그림 유형
rpart.plot(fit, extra = 103)
rpart.plot(fit, extra = 106)

# 혼동행렬
real.value <- iris.test$Species %>% as.factor
pred.value <- predict(fit, iris.test[, -5], type = 'class')
confusionMatrix(pred.value, real.value)
