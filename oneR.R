library(OneR)
mushrooms <- read.csv('~/Dropbox/rmachinelearn/mushrooms.csv',header = T)
mushrooms$veil_type <- NULL
mushrooms_1R <- OneR(mushrooms$type~.,data = mushrooms)
summary(mushrooms_1R)

library(RWeka)
summary(mushrooms)
mushrooms_1R <- OneR(mushrooms$type~.,data = mushrooms)
summary(mushrooms_1R)

library(RWeka)
#RIPPER
library(caret)
inTrain <- createDataPartition(mushrooms$type,p=0.75,list = F)
mushroomstrain <- mushrooms[inTrain,]
mushroomstest <- mushrooms[-inTrain,]
mushrooms_JRip <- JRip(type~.,data = mushroomstrain)
summary(mushrooms_JRip)
pred <- predict(mushrooms_JRip,mushroomstest,type = 'class')
library(gmodels)
CrossTable(mushroomstest$type,pred)
