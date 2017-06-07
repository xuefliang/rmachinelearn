library(caret)
wine <- read.csv('~/Dropbox/rmachinelearn/whitewines.csv',header = T)
inTrain <- createDataPartition(wine$quality,p=0.8,list = F)
wine_train <- wine[inTrain,]
wine_test <- wine[-inTrain,]

library(rpart)
library(rpart.plot)
m.rpart <- rpart(quality~.,data = wine_train)
m.rpart
plot(m.rpart)

rpart.plot(m.rpart,digits =4,fallen.leaves = T,type = 3,extra = 101)

p.rpart <- predict(m.rpart,wine_test)
summary(p.rpart)
cor(p.rpart,wine_test$quality)

MAE <- function(actual,predicted){
  mean(abs(actual-predicted))
}

MAE(p.rpart,wine_test$quality)

MAE(mean(wine_train$quality),wine_test$quality)

library(RWeka)
m.m5p <- M5P(quality~.,data = wine_train)
summary(m.m5p)

p.m5p <- predict(m.m5p,wine_test)
cor(p.m5p,wine_test$quality)
MAE(p.m5p,wine_test$quality)
