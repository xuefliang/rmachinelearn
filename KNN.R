wdbc <- read.csv('~/Dropbox/wdbc.data',header = F)
wdbc.names <- c('Radius','Texture','Perimeter','Area','Smoothness','Compactness',
             'Concavity','Concave points','Symmetry','Fractal dimension')
wdbc.names <- c(wdbc.names,paste0(wdbc.names,'_mean',seq=''),paste0(wdbc.names,'_worst',seq=''))
names(wdbc) <- c('id','diagnosis',wdbc.names)
str(wdbc)
dim(wdbc)

wdbc$diagnosis <- factor(wdbc$diagnosis,levels = c('B','M'),labels = c('Benign','Malignant'))
round(prop.table(table(wdbc$diagnosis))*100,digits = 1)
wdbc <- wdbc[,-1]

summary(wdbc[,c("Radius_mean","Area_mean","Smoothness_mean")])

normalize <- function(x){
  return((x-min(x))/(max(x)-min(x)))
}
wdbc_n <- as.data.frame(lapply(wdbc[2:31],normalize))

wdbc_train <- wdbc_n[1:469,]
wdbc_test <- wdbc_n[470:569,]
wdbc_train_label <- wdbc[1:469,1]
wdbc_test_label <- wdbc[470,569,1]
mal_rate <- table(wdbc_train_label)
round(mal_rate[2]/sum(mal_rate),digits = 2)

set.seed(2014)
inTrain <- sample(1:nrow(wdbc_n),469,replace = F)
wdbc_train <- wdbc_n[inTrain,]
wdbc_test <- wdbc_n[-inTrain,]
wdbc_train_label <- wdbc[inTrain,1]
wdbc_test_label <- wdbc[-inTrain,1]
mal_rate <- table(wdbc_test_label)
round(mal_rate[2]/sum(mal_rate),digits = 2)

library(caret)
set.seed(1024)
inTrain <- createDataPartition(wdbc$diagnosis,p=0.8,list = F)
wdbc_train <- wdbc_n[inTrain,]
wdbc_test <- wdbc_n[-inTrain,]
wdbc_train_label <- wdbc[inTrain,1]
wdbc_test_label <- wdbc[-inTrain,1]
mal_rate <- table(wdbc_test_label)
round(mal_rate[2]/sum(mal_rate),digits = 2)

library(class)
wdbc_test_pred <- knn(wdbc_train,wdbc_test,wdbc_train_label,k=21)

library(gmodels)
CrossTable(wdbc_test_label,wdbc_test_pred,prop.chisq = F)

wdbc_z <- as.data.frame(scale(wdbc[,-1]))
inTrain <- createDataPartition(wdbc$diagnosis,p=0.8,list = F)
wdbc_train <- wdbc_z[inTrain,]
wdbc_test <- wdbc_z[-inTrain,]
wdbc_train_label <- wdbc[inTrain,1]
wdbc_test_label <- wdbc[-inTrain,1]
mal_rate <- table(wdbc_test_label)
round(mal_rate[2]/sum(mal_rate),digits = 2)
wdbc_test_pred <- knn(wdbc_train,wdbc_test,wdbc_train_label,k=21)
CrossTable(wdbc_test_label,wdbc_test_pred,prop.chisq = F)

library(caret)
wdbc_test_pred <- knn3Train(wdbc_train,wdbc_test,wdbc_train_label,k=21,prob = F)
CrossTable(wdbc_test_label,wdbc_test_pred,prop.chisq = F)

library(RWeka)
iris <- read.arff(system.file('arff','iris.arff',package='RWeka'))
classifier <- IBk(class~.,data = iris)
summary(classifier)
#k从1到20选择
classifier <- IBk(class~.,data = iris,control = Weka_control(K=20,X=TRUE))
evaluate_Weka_classifier(classifier,numFolds = 10)
