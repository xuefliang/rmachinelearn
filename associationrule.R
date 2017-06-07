library(arules)
groceries <- read.transactions('~/Dropbox/rmachinelearn/Machine-Learning-with-R-datasets-master/groceries.csv',sep=',')
summary(groceries)

itemFrequencyPlot(groceries,support=0.1)
itemFrequencyPlot(groceries,topN=20)

inspect(groceries[1:5])
itemFrequency(groceries[,1:3])

image(groceries[1:5])
image(sample(groceries,100))

apriori(groceries)
groceriesrule <- apriori(groceries,parameter = list(support=0.006,confidence=0.25,minlen=2))
groceriesrule

summary(groceriesrule)
summary(groceriesrule[1:3])
inspect(sort(groceriesrule,by='lift')[1:5])

berryrules <- subset(groceriesrule,items %in% "berries")
inspect(berryrules)

write(groceriesrule,file = 'groceryrules.csv',sep=",",
      quote=T,row.names=F)
