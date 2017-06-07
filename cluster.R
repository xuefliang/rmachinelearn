teens <- read.csv('~/Dropbox/rmachinelearn/Machine-Learning-with-R-datasets-master/snsdata.csv')
dim(teens)
str(teens)
table(teens$gender)
table(teens$gender,useNA = 'ifany')

summary(teens$age)
teens$age <- ifelse(teens$age>=13 & teens$age<20,teens$age,NA)
summary(teens$age)
# data pre
teens$female <- ifelse(teens$gender=='F' & !is.na(teens$gender),1,0)
teens$no_gender <- ifelse(is.na(teens$gender),1,0)

table(teens$gender,useNA = 'ifany')
prop.table(table(teens$gender,useNA = 'ifany'))
table(teens$female,useNA = 'ifany')
table(teens$no_gender,useNA = 'ifany')

mean(teens$age,na.rm=T)
aggregate(data=teens,age~gradyear,mean,na.rm=T)

ave_age <- ave(teens$age,teens$gradyear,FUN = function(x) mean(x,na.rm=T))
teens$age <- ifelse(is.na(teens$age),ave_age,teens$age)
summary(teens$age)

interests <- teens[5:40]
interests_z <- as.data.frame(lapply(interests,scale))
teen_cluster <- kmeans(interests_z,5)
teen_cluster$size
teen_cluster$centers

teens$cluster <- teen_cluster$cluster
teens[1:5,c('cluster','gender','age','friends')]

aggregate(data=teens,age~cluster,mean)
aggregate(data=teens,female~cluster,mean)
aggregate(data=teens,friends~cluster,mean)
