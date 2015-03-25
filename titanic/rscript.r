setwd("/Users/kiote/www/kagle/titanic/")
mydata = read.csv("train.csv")
head(mydata)

# what ages was there
hist(mydata$Age, col="green", breaks=100)

# male vs female survived
prop.table(table(train$Sex, train$Survived), 1)

# male vs female survived by class
barplot(table(mydata$Survived[mydata$Sex == 'female' & mydata$Pclass==1]))
barplot(table(mydata$Survived[mydata$Sex == 'female' & mydata$Pclass==2]))
barplot(table(mydata$Survived[mydata$Sex == 'female' & mydata$Pclass==3]))

# load test data
test <- read.csv("test.csv", stringsAsFactors=FALSE)

# everybody dies
test$Survived <- rep(0, 418)

# save "all is dead" result
submit <- data.frame(PassengerId = test$PassengerId, Survived = test$Survived)
write.csv(submit, file = "theyallperish.csv", row.names = FALSE)

# all woman stayed alive!
test$Survived[test$Sex == 'female'] <- 1

# save result 
submit <- data.frame(PassengerId = test$PassengerId, Survived = test$Survived)
write.csv(submit, file = "allwomen.csv", row.names = FALSE)

# let's add Child flag
train$Child <- 0
train$Child[train$Age < 18] <- 1

# we can see nothing special here - still most men dies, most women lives
aggregate(Survived ~ Child + Sex, data=train, FUN=sum)

# let's add more cut by fare:
train$Fare2 <- '$30+'
train$Fare2[train$Fare < 30 & train$Fare >= 20] <- '$20-$30'
train$Fare2[train$Fare < 20 & train$Fare >= 10] <- '$10-$20'
train$Fare2[train$Fare < 10] <- '<$10'

# wow, somehow most womens of 3rd class, who payed $20 and more for ticket are dead
aggregate(Survived ~ Fare2 + Pclass + Sex, data=train, FUN=function(x) {sum(x)/length(x)})

# so let's use it in prediction:
test$Survived <- 0
test$Survived[test$Sex == 'female'] <- 1
test$Survived[test$Sex == 'female' & test$Pclass == 3 & test$Fare >= 20] <- 0

# save result 
submit <- data.frame(PassengerId = test$PassengerId, Survived = test$Survived)
write.csv(submit, file = "allwomenbut3class.csv", row.names = FALSE)
