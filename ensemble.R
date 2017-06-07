set.seed(10)
y <- seq(1:1000)
x1 <- seq(1:1000)*runif(1000,min=0,max = 2)
x2 <- (seq(1:1000)*runif(1000,min = 0,max = 2))^2
x3 <- log(seq(1:1000)*runif(1000,min = 0,max = 2))
my.data <- data.frame(y,x1,x2,x3)

#线性回归
lm.fit <- lm(y~.,data = my.data)
summary(lm.fit)

credit <- read.csv('~/Dropbox/rmachinelearn/credit.csv',header = T)

#randomforest
library(randomForest)
rf <- randomForest(credit$default~.,data = credit)

library(caret)
ctrl <- trainControl(method = 'repeatedcv',number = 10,repeats = 10)
grid_rf <- expand.grid(.mtry=c(2,4,8,16))
set.seed(300)
m_rf <- train(default~.,data = credit,method='rf',metric='Kappa',trControl=ctrl,tuneGrid=grid_rf)


#boost tree
grid_c50 <- expand.grid(.model='tree',.trials=c(10,20,30,40),.winnow=F)
set.seed(300)
m_c50 <- train(default~.,data = credit,method='C5.0',metric='Kappa',trControl=ctrl,tuneGrid=grid_c50)

#线性模型
set.seed(100)
positions <- sample(nrow(my.data),size = floor(nrow(my.data)/4)*3)
training <- my.data[positions,]
testing <- my.data[-positions,]
lm_fit <- lm(y~.,data = training)
predictions <- predict(lm_fit,newdata = testing)
error <- sqrt((sum((testing$y-predictions)^2))/nrow(testing))
error

#bagging
library(foreach)
length_divisor <- 6
iterations <- 1000
predictions <- foreach(m=1:iterations,.combine = cbind) %do% {
  training_positions <- sample(nrow(training),size = floor((nrow(training)/length_divisor)))
  train_pos <- 1:nrow(training) %in% training_positions
  lm_fit <- lm(y~.,data = training[train_pos,])
  predict(lm_fit,newdata = testing)
}
predictions <- rowMeans(predictions)
error <- sqrt((sum((testing$y-predictions)^2))/nrow(testing))
error

#rf-ensemble
library(randomForest)
rf_fit <- randomForest(y~.,data = training,ntree=500)
predictions <- predict(rf_fit,newdata = testing)
error <- sqrt((sum((testing$y-predictions)^2))/nrow(testing))
error


#rf+bagging
length_divisor <- 6
iterations <- 1000
predictions <- foreach(m=1:iterations,.combine = cbind) %do% {
  training_positions <- sample(nrow(training),size = floor((nrow(training)/length_divisor)))
  train_pos <- 1:nrow(training) %in% training_positions
  lm_fit <- lm(y~.,data = training[train_pos,])
  predict(lm_fit,newdata = testing)
}
lm_predictions <- rowMeans(predictions)

rf_fit <- randomForest(y~.,data = training,ntree=500)
rf_predictions <- predict(rf_fit,newdata = testing)
predictions <- (lm_predictions+rf_predictions)/2
error <- sqrt((sum((testing$y-predictions)^2))/nrow(testing))
error

#svm
library(e1071)
svm_fit <- svm(y~.,data = training)
svm_predictions <- predict(svm_fit,newdata = testing)
error <- sqrt((sum((testing$y-svm_predictions)^2))/nrow(testing))
error

#svm+bagging
predictions <- foreach(m=1:iterations,.combine = cbind) %do% {
  training_positions <- sample(nrow(training),size = floor((nrow(training)/length_divisor)))
  train_pos <- 1:nrow(training) %in% training_positions
  svm_fit <- svm(y~.,data = training[train_pos,])
  predict(svm_fit,newdata = testing)
}
svm_predictions <- rowMeans(predictions)
error <- sqrt((sum((testing$y-svm_predictions)^2))/nrow(testing))
error

#ensemble svm+rf
predictions <- (svm_predictions+rf_predictions)/2
error <- sqrt((sum((testing$y-predictions)^2))/nrow(testing))
error

predictions <- (svm_predictions*2+rf_predictions)/3
error <- sqrt((sum((testing$y-predictions)^2))/nrow(testing))
error