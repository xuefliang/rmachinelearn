library(rpart)
library(caret)
library(rpart.plot)

#case 1
inTrain <-createDataPartition(iris$Species,p=0.7,list = F) 
iristrain <- iris[inTrain,]
iristest <- iris[-inTrain,]
treemodel <- rpart(Species~.,data = iristrain)

plot(treemodel)
prp(treemodel,varlen = 5)
rpart.plot(treemodel)

predicted <- predict(treemodel,iristest[1:4],type = 'class')
table(iristest$Species,predicted)

#case 2
credit <- read.csv('~/Dropbox/rmachinelearn/credit.csv',header = T)
View(credit)
View(credit)
inTrain <- createDataPartition(credit$default,p=0.75,list = F)
creditTrain <- credit[inTrain,]
creditTest <- credit[-inTrain,]
library(C50)
credit_model <- C5.0(defult~.,data=creditTrain)
credit_model <- C5.0(creditTrain$default~.,data=creditTrain)
summary(credit_model)
summary(credit_model)
credit_model2 <- C5.0(default~.,data=creditTrain,trials=10)
credit_model2
credit_model3 <- C5.0(default~.,data = creditTrain,trials=10,costs=c(0,5,1,0))
credit_model3 <- C5.0(default~.,data = creditTrain,trials=10,costs=matrix(0,5,1,0))

View(costs)
credit_model3 <- C5.0(default~.,data = creditTrain,trials=10,costs=matrix(c(0,5,1,0)))
credit_model3 <- C5.0(default~.,data = creditTrain,trials=10,costs=matrix(c(0,5,1,0),nrow = 2))
credit_pred <- predict(credit_model3,creditTest[,-"default"])
credit_pred <- predict(credit_model3,creditTest[,1:16])

library(gmodels)
credit_pred <- predict(credit_model3,creditTest[,1:16],type = 'class')
CrossTable(creditTest$default,credit_pred)
credit_model3 <- C5.0(default~.,data = creditTrain,trials=10,costs=matrix(c(0,1,5,0),nrow = 2))
credit_pred <- predict(credit_model2,creditTest[,1:16],type = 'class')

library(gmodels)
CrossTable(creditTest$default,credit_pred)
CrossTable(creditTest$default,credit_pred,prop.chisq = F)
CrossTable(creditTest$default,credit_pred,prop.chisq = F,dnn = c('actual','predict'))
costs=matrix(c(0,1,5,0),nrow = 2)

library(bnlearn)
fitControl <- trainControl(method = "repeatedcv", number = 10, repeats = 3,returnResamp = "all")
model1 <- train(Species~., data=iris,method='nb',trControl = fitControl,tuneGrid = data.frame(.fL=1,.usekernel=F,.adjust=c(0.1,0.2,0.5,0.8)))
resampleHist(model1)