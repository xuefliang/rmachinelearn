library(caret)
letters <- read.csv('~/Dropbox/rmachinelearn/letter-recognition.data')
names(letters) <- c('lettr','x-box','y-box','width','high','onpix',
                    'x-bar','y-bar','x2bar','y2bar','xybar',
                    'x2ybr','xy2br','x-ege','xegvy','y-ege','yegvx')

inTrain <- createDataPartition(letters$lettr,p=0.8,list = F)
letters_train <- letters[inTrain,]
letters_test <- letters[-inTrain,]

library(kernlab)
letters_classifier <- ksvm(lettr~.,data=letters_train,kernel='vanilladot')
letters_classifier <- ksvm(lettr~.,data=letters_train,kernel='rbfdot')
library(e1071)
letters_classifier <- svm(letters_train$lettr ~ ., data = letters_train)

letters_predicted <- predict(letters_classifier,letters_test)
table(letters_predicted,letters_test$lettr)

agreement <- letters_predicted==letters_test$lettr
table(agreement)
prop.table(table(agreement))

#case2
set.seed(123)
x <- matrix(rnorm(20*2),ncol = 2)
y <- c(rep(-1,10),rep(1,10))
x[y==1] <- x[y==1]+1

library(e1071)
dat <- data.frame(x=x,y=as.factor(y))
svmfit <- svm(y~.,data = dat,kernel='linear',cost=10,scale = T)
summary(svmfit)
svmfit$index

#tune()
tune.out <- tune(svm,y~.,data=dat,kernel='linear',ranges=list(cost=c(0.001,0.01,0.1,1,10,100)))
summary(tune.out)
bestmodel <- tune.out$best.model

xtest <- matrix(rnorm(20*2),ncol = 2)
ytest <- sample(c(-1,1),20,replace = T)
xtest[ytest==1] <- xtest[ytest==1]+1
testdat <- data.frame(x=xtest,y=as.factor(ytest))
ypred <- predict(bestmodel,testdat)
table(ypred,testdat$y)

set.seed(123)
x <- matrix(rnorm(200*2),ncol = 2)
x[1:100] <- x[1:100]+2
x[101:150] <- x[101:150]-2
y <- c(rep(1,150),rep(2,50))
dat <- data.frame(x=x,y=as.factor(y))

train <- sample(200,100)
svmfit <- svm(y~.,data=dat[train,],kernel='radial',gamma=1,cost=1)
summary(svmfit)
tune.out <- tune(svm,y~.,data=dat[train,],kernel='radial',ranges = list(cost=c(0.1,1,10,100),gamma=c(0.5,1,5,10,20)))
summary(tune.out)
table(true=dat[-train,'y'],predicted=predict(tune.out$best.model,dat[-train,]))

library(ROCR)
rocplot <- function(predictions,labels,...){
  pred<- prediction(predictions,labels)
  perf <- performance(pred,'tpr','fpr')
  plot(perf,...)
}

svm.fit <- svm(y~.,data = dat[train,],kernel='radial',gamma=0.5,cost=10,
                  probability=T)
pred <- predict(svm.fit, dat[train,], probability=T)
rocplot(attr(pred, "prob")[,2],dat[train,'y'])

test_pred <- predict(svm.fit,dat[-train,],probability=T)
rocplot(attr(test_pred,'prob')[,2],dat[-train,'y'])
