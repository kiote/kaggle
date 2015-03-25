setwd("/Users/kiote/www/kagle/titanic/")
train <- read.csv("train.csv")
test <- read.csv("test.csv")

install.packages('rattle')
install.packages('rpart.plot')
install.packages('RColorBrewer')

library(rattle)
library(rpart.plot)
library(RColorBrewer)

fit <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked, data=train, method="class")

fancyRpartPlot(fit)

Prediction <- predict(fit, test, type = "class")

submit <- data.frame(PassengerId = test$PassengerId, Survived = Prediction)
write.csv(submit, file = "myfirstdtree.csv", row.names = FALSE)
