#install.packages("caret")

library(caret)
library(caret)
library(class)
library(ggvis)
library(proxy)

standardization <- function(x) {          # 표준화
  return ((x-min(x))/(max(x)-min(x)))
}

iris <- iris
iris %>% ggvis(~Sepal.Length, ~Sepal.Width, fill = ~Species) %>% layer_points
iris %>% ggvis(~Petal.Length, ~Petal.Width, fill = ~Species) %>% layer_points()

iris_x <- as.data.frame(lapply(iris[1:4], standardization))
set.seed(1234)
samp <- createDataPartition(iris$Species, p=0.7, list=F)
iris.train_raw <- iris[samp, ]
iris.train <- iris_x[samp,]
row.names(iris.train) <- 1:nrow(iris.train)
iris.test_raw <- iris[-samp,]
iris.test <- iris_x[-samp,]

train <- iris.train_raw[, -5] 
test <- iris.test_raw[, -5]

row.names(iris.test) <- 1:nrow(iris.test)
row.names(iris.test_raw) <- 1:nrow(iris.test_raw)

Euclidean_distance <- function(x1, x2) sqrt(((x1 - x2)^2) %>% rowSums())
Manhattan_distance <- function(x1, x2) sqrt(abs((x1 - x2)) %>% rowSums())
Mahalanobis_distance <- function(x1, x2) mahalanobis(x = x1 - x2,center = colMeans(x1 - x2),cov = cov(x1 - x2))

myknn <- function(train, test, cl, k, func) {
  factor <- cl
  train_x <- train %>% as.matrix()
  test_x <- test %>% as.matrix()
  result <- NA

  if(func == "Euclidean_distance") {
    for(i in 1:nrow(test)) {
      E.d <- Euclidean_distance(train_x, test_x[i, ])
      E.d <- cbind(E.d, samp) 
      E.d <- E.d[order(E.d[,1], E.d[, 2]), ]
      
      factor <- cl[E.d[1:k, -1]]
      
      result[i] <- ifelse(table(factor) %>% which.max  == 1, "setosa", 
                                 ifelse(table(factor) %>% which.max  == 2, "versicolor", "virginica"))
    }
  }
  
  else if(func == "Manhattan_distance") {
    for(i in 1:nrow(test)) {
      E.d <- Manhattan_distance(train_x, test_x[i, ])
      E.d <- cbind(E.d, samp) 
      E.d <- E.d[order(E.d[,1], E.d[, 2]), ]
      
      factor <- cl[E.d[1:k, -1]]
      
      result[i] <- ifelse(table(factor) %>% which.max  == 1, "setosa", 
                            ifelse(table(factor) %>% which.max  == 2, "versicolor", "virginica"))
    }
  }
  
  else if(func == "Mahalanobis_distance") {
    for(i in 1:nrow(test)) {
      E.d <- Mahalanobis_distance(train_x, test_x[i, ])
      E.d <- cbind(E.d, samp) 
      E.d <- E.d[order(E.d[,1], E.d[, 2]), ]
      
      factor <- cl[E.d[1:k, -1]]
      
      result[i] <- ifelse(table(factor) %>% which.max  == 1, "setosa", 
                            ifelse(table(factor) %>% which.max  == 2, "versicolor", "virginica"))
    }
  }
  
  return(result)
}

Euclidean <- myknn(iris.train[, -5], iris.test[, -5], iris$Species, 5, "Euclidean_distance")
Euclidean <- Euclidean %>% as.factor()
confusionMatrix(Euclidean, iris.test_raw$Species) 

Manhattan <- myknn(iris.train[, -5], iris.test[, -5], iris$Species, 5, "Manhattan_distance")
Manhattan <- Manhattan %>% as.factor()
confusionMatrix(Manhattan, iris.test_raw$Species) 


Mahalanobis <- myknn(iris.train[, -5], iris.test[, -5], iris$Species, 5, "Mahalanobis_distance")
Mahalanobis <- Mahalanobis %>% as.factor()
confusionMatrix(Mahalanobis, iris.test_raw$Species) 

