library(caret)
library(neuralnet)
data(iris)
inTrain <- createDataPartition(y=iris$Species,p=0.8,list = F)
iris.train <- iris[inTrain,]
iris.test <- iris[-inTrain,]
iris.train <- cbind(iris.train,iris.train$Species=='setosa')
iris.train <- cbind(iris.train,iris.train$Species=='versicolor')
iris.train <- cbind(iris.train,iris.train$Species=='virginica')
names(iris.train)[6]='setosa'
names(iris.train)[7]='versicolor'
names(iris.train)[8]='virginica'

nn <- neuralnet(setosa+versicolor+virginica~Sepal.Length+Sepal.Width+Petal.Length+Petal.Width,data=iris.train,hidden=c(3))
plot(nn)

mypredict <- compute(nn,iris.test[-5])$net.result
maxidx <- function(arr){
  return(which(arr==max(arr)))
}
idx <- apply(mypredict,1,maxidx)
predict.species <- c('setosa','versicolor','virginica')
prediction <- predict.species[idx]
table(prediction,iris.test$Species)

concrete <- read.csv('~/Dropbox/rmachinelearn/concrete.csv',header = T)
str(concrete)
summary(concrete)
normalize <- function(x){
  return((x-min(x))/(max(x)-min(x)))
}
concrete_norm <- as.data.frame(lapply(concrete,normalize))
summary(concrete_norm$strength)
summary(concrete$strength)

inTrain <- createDataPartition(y=concrete_norm$strength,p=0.8,list = F)
concrete_train <- concrete_norm[inTrain,]
concrete_test <- concrete_norm[-inTrain,]
concrete_model <- neuralnet(strength~cement+slag+ash+water+superplastic+
                              coarseagg+fineagg+age,data = concrete_train)
plot(concrete_model)

model_result <- compute(concrete_model,concrete_test[1:8])
predicted_strength <- model_result$net.result
cor(predicted_strength,concrete_test$strength)

MAE <- function(actual,predicted){
  mean(abs(actual-predicted))
}
MAE(concrete_test$strength,predicted_strength)


concrete_model2 <- neuralnet(strength~cement+slag+ash+water+superplastic+
                              coarseagg+fineagg+age,data = concrete_train,hidden = 5)
plot(concrete_model2)

model_result <- compute(concrete_model2,concrete_test[1:8])
predicted_strength <- model_result$net.result
MAE(concrete_test$strength,predicted_strength)
