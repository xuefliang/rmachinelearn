set.seed(10)
y <- seq(1:1000)
x1 <- seq(1:1000)*runif(1000,min=0,max = 2)
x2 <- seq(1:1000)*runif(1000,min = 0,max = 2)
x3 <- seq(1:1000)*runif(1000,min = 0,max = 2)
my.data <- data.frame(y,x1,x2,x3)

#线性回归
lm.fit <- lm(y~.,data = my.data)
summary(lm.fit)

set.seed(10)
size <- floor((nrow(my.data)*0.75))
intrain <- sample(nrow(my.data),size = size)
training <- my.data[intrain,]
testing <- my.data[-intrain,]

lm.fit <- lm(y~.,data = training)
pred <- predict(lm.fit,newdata = testing)
rmse <- sqrt((sum((testing$y-pred)^2))/nrow(testing))

#bagging
library(foreach)
iterations <- 1000
length.sample <- 1/4
size <- floor((nrow(training)*length.sample))
pred<-foreach(i=1:iterations,.combine=cbind) %do% {
  in.train <- sample(nrow(training), size=size)
  lm.fit<-lm(y~x1+x2+x3,data=training[intrain,])
  predict(lm.fit,newdata=testing)
}
final.pred <- rowMeans(pred)
rmse <- sqrt((sum((testing$y-final.pred)^2))/nrow(testing))
rmse

#自定义bagging函数
bagging <- function(training,testing,length_divisor=4,iterations=1000){
  predictions<-foreach(m=1:iterations,.combine=cbind) %do% {
    training_positions <- sample(nrow(training), size=floor((nrow(training)/length_divisor)))
    train_pos<-1:nrow(training) %in% training_positions
    lm_fit<-lm(y~x1+x2+x3,data=training[train_pos,])
    predict(lm_fit,newdata=testing)
  }
  rowMeans(predictions)
}