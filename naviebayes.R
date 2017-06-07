library(e1071)
head(iris)
pairs(iris[1:4],pch=21,bg=c('red','green','blue')[unclass(iris$Species)])
classifier <- naiveBayes(iris[,1:4],iris$Species)

library(bnlearn)
fitControl <- trainControl(method = "repeatedcv", number = 10, repeats = 3,returnResamp = "all")
model1 <- train(Species~., data=iris,method='nb',trControl = fitControl,tuneGrid = data.frame(.fL=1,.usekernel=F,.adjust=c(0.1,0.2,0.5,0.8)))
resampleHist(model1)

library(klaR)
classifier <- NaiveBayes(Species~.,data = iris)

table(predict(classifier,iris[,1:4]),iris$Species)

sms_raw <- read.table('~/Dropbox/rmachinelearn/SMSSpamCollection',stringsAsFactors = F,
                      header = F,sep = '\t',quote = NULL,encoding = 'UTF-8')
names(sms_raw) <- c('type','text')
str(sms_raw)

sms_raw$type <- as.factor(sms_raw$type)
table(sms_raw$type)

library(tm)
#sessionInfo()
sms_corpus <- VCorpus(VectorSource(sms_raw$text))
inspect(sms_corpus[1:3])

corpus_clean <- tm_map(sms_corpus,content_transformer(tolower))
corpus_clean <- tm_map(corpus_clean,removeNumbers)
corpus_clean <- tm_map(corpus_clean,removeWords,stopwords())
corpus_clean <- tm_map(corpus_clean,removePunctuation)
corpus_clean <- tm_map(corpus_clean,stripWhitespace)
corpus_clean <- tm_map(corpus_clean,PlainTextDocument)
inspect(corpus_clean[1:3])

sms_dtm <- DocumentTermMatrix(corpus_clean)

library(caret)
inTrain <- createDataPartition(sms_raw$type,p=0.75,list = F)
sms_raw_train <- sms_raw[inTrain,]
sms_raw_test <- sms_raw[-inTrain,]
sms_dtm_train <- sms_dtm[inTrain,]
sms_dtm_test <- sms_dtm[-inTrain,]
sms_train_labels <- sms_raw[inTrain, ]$type
sms_test_labels  <- sms_raw[-inTrain, ]$type

prop.table(table(sms_raw_train$type))
prop.table(table(sms_raw$type))

library(wordcloud)
str(corpus_clean)
wordcloud(corpus_clean,min.freq = 40,random.order = F)
spam <- subset(sms_raw, type == "spam")
ham  <- subset(sms_raw, type == "ham")

wordcloud(spam$text, max.words = 40, scale = c(3, 0.5))
wordcloud(ham$text, max.words = 40, scale = c(3, 0.5))

sms_dict <- findFreqTerms(sms_dtm_train,lowfreq = 5)

sms_corpus_train <- corpus_clean[inTrain]
sms_corpus_test <- corpus_clean[-inTrain]
sms_train <- DocumentTermMatrix(sms_corpus_train,list(sms_dict))
sms_test <- DocumentTermMatrix(sms_corpus_test,list(sms_dict))

convert_count <- function(x){
  x <- ifelse(x>0,'yes','no')
}

sms_train <- apply(sms_train,2,convert_count)
sms_test <- apply(sms_test,2,convert_count)

library(e1071)
sms_classifier <- naiveBayes(sms_train, sms_train_labels)

library(naivebayes)
sms <- as.data.frame(cbind(sms_train,sms_train_labels))
sms_classifier <- naive_bayes(sms_train,sms_train_labels,laplace = 1)
sms_classifier <- naive_bayes(sms_train_labels~.,data=sms,laplace = 1)
sms_test_pred <- predict(sms_classifier,sms_test,type = 'class')

library(gmodels)
CrossTable(sms_test_labels,sms_test_pred,prop.chisq = F,prop.t = F,dnn = c('actual','predicted'))
