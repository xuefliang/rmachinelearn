library(caret)
credit <- read.csv('~/Dropbox/rmachinelearn/credit.csv',header = T)

random_ids <- order(runif(1000))
train <- credit[random_ids[1:500],]
validate <- credit[random_ids[501:750],]
test <- credit[random_ids[751:100],]

inTrain <- createDataPartition(credit$default,p=0.75,list = F)
train <- credit[inTrain,]
test <- credit[-inTrain,]

library(C50)
library(irr)
folds <- createFolds(credit$default,k=10)
cv_result <- lapply(folds,function(x){
  credit_train <-credit[-x,]
  credit_test <- credit[x,]
  credit_model <- C5.0(default~.,data=credit_train)
  credit_predit <- predict(credit_model,credit_test)
  credit_actual <- credit_test$default
  kappa <- kappa2(data.frame(credit_actual,credit_predit))$value
  return(kappa)
})

str(cv_result)
mean(unlist(cv_result))

library(car)
Prestige_clean <- Prestige[!is.na(Prestige$type),]
model <- lm(Prestige$prestige~.,data = Prestige)
score <- predict(model,Prestige_clean[,-4])
actual <- Prestige_clean$prestige
rmse <- sqrt(mean((score-actual)^2))

mu <- mean(actual)
rse <- sum((score-actual)^2)/sum((mu-actual)^2)
rsqure <- 1-rse


