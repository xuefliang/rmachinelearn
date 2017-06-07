library(caret)
data(mdrr)
#常数自变量，或者是方差极小的自变量
zerovar <- nearZeroVar(mdrrDescr)
newdata1 <- mdrrDescr[,-zerovar]
#删除的是与其它自变量有很强相关性的变量
descrCorr <- cor(newdata1)
highCorr <- findCorrelation(descrCorr,0.9)
newdata2 <- newdata1[,-highCorr]
#自变量多重共线性
comboinfo <- findLinearCombos(newdata1)
newdata2 <- newdata2[,-comboinfo$remove]
#标准化并补足缺失值
Process <- preProcess(newdata2)
newdata3 <- predict(Process,newdata2)
#用createDataPartition将数据进行划分，分成75%的训练样本和25%检验样本
inTrain <- createDataPartition(mdrrClass,p=3/4,list = F)
trainx <- newdata3[inTrain,]
testx <- newdata3[-inTrain,]
trainy <- mdrrClass[inTrain]
testy <- mdrrClass[-inTrain]
#建模前还可以对样本数据进行图形观察，例如对前两个变量绘制箱线图
featurePlot(trainx[,1:2],trainy,plot = 'box')
