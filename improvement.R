library(caret)
credit <- read.csv('~/Dropbox/rmachinelearn/credit.csv',header = T)
#默认
set.seed(300)
m <- train(default~.,data = credit,method='C5.0')

p <- predict(m,credit)
table(p,credit$default)
head(predict(m,credit,type = 'prob'))

#控制参数调整
ctrl <- trainControl(method = 'cv',number = 10,selectionFunction = 'oneSE')
grid <- expand.grid(.model='tree',.winnow=FALSE,.trials=c(1,5,10,15,20,25,30))
m <- train(default~.,data = credit,method='C5.0',metric='Kappa',
           trControl=ctrl,tuneGrid=grid)
m$finalModel

