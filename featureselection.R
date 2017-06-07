library(caret)
library(mlbench)
data(PimaIndiansDiabetes)

#相关性
cormatrix <- cor(PimaIndiansDiabetes[,1:8])
highcor <- findCorrelation(cormatrix,cutoff = 0.5)
print(highcor)

#重要性排序
library(pROC)
control <- trainControl(method="repeatedcv", number=10, repeats=3) 
model <- train(diabetes~.,data=PimaIndiansDiabetes, method="lvq", preProc = c("center", "scale"),trControl = control)  
importance <- varImp(model, scale=FALSE)  
print(importance) 

#自动feature selection
control <- rfeControl(functions = rfFuncs,method = 'cv',number = 10)
results <- rfe(PimaIndiansDiabetes[,1:8],PimaIndiansDiabetes[,9],sizes = c(1:8),rfeControl = control)
print(results)
predictors(results)
plot(results,type=c('g','o'))
