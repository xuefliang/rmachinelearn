#ipred
library(ipred)
set.seed(300)
credit <- read.csv('~/Dropbox/rmachinelearn/credit.csv',header = T)
mybag <- bagging(default~.,data = credit,nbagg=25)
credit_pred <- predict(mybag,credit)
table(credit_pred,credit$default)

library(caret)
set.seed(300)
ctrl <- trainControl(method = 'cv',number = 10)
train(default~.,data = credit,method='treebag',trControl=ctrl)

str(svmBag)
svmBag$fit


bagctrl <- bagControl(fit = svmBag$fit, predict = svmBag$pred,aggregate = svmBag$aggregate)

set.seed(300)
svmbag <- train(default ~ ., data = credit,"bag", trControl = ctrl, bagControl = bagctrl)
svmbag