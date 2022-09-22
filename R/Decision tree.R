library(dplyr)
library(caret)

plot(NA,xlab = "X1", ylab="X2",
     xlim = c(0,5), ylim = c(0,5))

for(i in 0:5) {
  abline(v=i, col="gray90")
  abline(h=i, col="gray90")
}

points(1:5, c(1,2,2,4,5), pch=16, cex=3,
       col=c(rep("red",3),rep("green",2)))
text(1,0.8,"(1, 1)",cex=1.3)
text(2,1.8,"(2, 2)",cex=1.3)  
text(3,1.8,"(3, 2)",cex=1.3)  
text(4,3.8,"(4, 4)",cex=1.3)  
text(5,4.8,"(5, 5)",cex=1.3)  

play <- read.csv("play.csv", header = T, stringsAsFactors = F)

ent <- function(S) {
  class <- NULL
  for(i in 1:length(unique(S))) 
    class[i] <- table(S)[i]/length(S)
  return(-sum(class*log2(class)))
}

IG <- function(S, A) {
  w <- table(A)/length(A)
  ents <- tapply(S, A, ent)
  return(ent(S) - sum(w*ents))
}

IG(play$Play, play$Wind)
IG(play$Play, play$Outlook)

# 타이타닉호 생존
#install.packages("rpart")
#install.packages("rpart.plot")
library(rpart)

path <- 'https://raw.githubusercontent.com/guru99-edu/R-Programming/master/titanic_data.csv'
titanic <- read.csv(path)
head(titanic)
set.seed(3)
shuffle_index <- sample(1:nrow(titanic)) # 셔플링
titanic <- titanic[shuffle_index, ]

# home.dest, cabin, name, X, ticket 변수 제거
clean_titanic <- titanic %>%
  select(-c(home.dest, cabin, name, x, ticket)) %>%  
  # 팩터 값 변경
  mutate(pclass = factor(pclass, levels = c(1,2,3), labels = c('Upper','Middle','Lower')),
         survived = factor(survived, levels = c(0,1), labels = c('No','Yes'))) %>% 
  filter(age != "?") %>% 
  filter(fare != "?")

glimpse(clean_titanic) #str(clean_titanic)

clean_titanic$age <- clean_titanic$age %>% as.numeric
clean_titanic$fare <- clean_titanic$fare %>% as.numeric
glimpse(clean_titanic)

# 훈련, 테스트 데이터 분리
set.seed(4)
samp <- createDataPartition(clean_titanic$survived, p=0.8, list = F)
data_train <- clean_titanic[samp,]
data_test <- clean_titanic[-samp,]

table(data_train$survived)
prop.table(table(data_train$survived))

# 의사결정나무 구하기
fit <- rpart(survived ~ ., data = data_train, method = 'class')

# 의사결정나무 그리기1
plot(fit)
text(fit, use.n = TRUE)

# 의사결정나무 그리기2
#install.packages("rpart.plot")
library(rpart.plot)
rpart.plot(fit, extra = 101)
rpart.plot(fit, extra = 103)
rpart.plot(fit, extra = 106)

# 혼동행렬
real.value <- data_test$survived %>% as.factor
pred.value <- predict(fit, data_test, type = 'class')
confusionMatrix(pred.value, real.value)

# 하이퍼 파라미터 튜닝
cont <- rpart.control(minsplit = 10, minbucket = 5, maxdepth = 6, cp = 0)
tuned_fit <- rpart(survived~., data = data_train,
                   method = 'class', control = cont)
rpart.plot(tuned_fit, extra = 101)

# 혼동행렬 (튜닝 모델)
real.value <- data_test$survived %>% as.factor
pred.value <- predict(tuned_fit, data_test, type = 'class')
confusionMatrix(pred.value, real.value)
