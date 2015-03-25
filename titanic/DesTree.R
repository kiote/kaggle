setwd("/Users/kiote/www/kaggle/titanic")
data = read.csv("train.csv")
check = read.csv("test.csv")

#
# first let's make numeric sex for both datasets
#
data$SexInt = 0
check$SexInt = 0

data$SexInt[data$Sex == 'male'] = 1
check$SexInt[check$Sex == 'male'] = 1

#
# let's try to fill in missing ages for train and check data set
#
library(mice)
# train
simple = data[c("Survived", "Fare", "SexInt", "Age", "Pclass")]
imputed = complete(mice(simple))

data$Age = imputed$Age

# check
simple = check[c("Fare", "SexInt", "Age", "Pclass")]
imputed = complete(mice(simple))

check$Age = imputed$Age

#
# making more factors
#

# train
data$EmbarkedInt = 0
data$EmbarkedInt[data$Embarked == 'S'] = 1
data$EmbarkedInt[data$Embarked == 'C'] = 2
data$EmbarkedInt[data$Embarked == 'Q'] = 3

data$Has.Cabin = 0
data$Has.Cabin[data$Cabin != '']=1

# check
check$EmbarkedInt = 0
check$EmbarkedInt[check$Embarked == 'S'] = 1
check$EmbarkedInt[check$Embarked == 'C'] = 2
check$EmbarkedInt[check$Embarked == 'Q'] = 3

check$Has.Cabin = 0
check$Has.Cabin[check$Cabin != '']=1

#
# split data to train and test (to check our model before submitting to kaggle)
#
library(caTools)
split = sample.split(data$Survived, SplitRatio = 0.80)

train = subset(data, split == TRUE)
test = subset(data, split == FALSE)

#
# Decision tree with cross-validation
#
library(caret)
library(e1071)

numFolds = trainControl(method="cv", number=10)
cpGrid = expand.grid(.cp = seq(0.01,0.5,0.01))

# obtain cp coefficient
train(Survived ~ Pclass + Age + SibSp + Parch + Fare + SexInt + EmbarkedInt + Has.Cabin, data = train, method="rpart", trControl = numFolds, tuneGrid = cpGrid)

tree = rpart(Survived ~ Pclass + Age + SibSp + Parch + Fare + SexInt + EmbarkedInt + Has.Cabin, data = train, method="class", cp=0.29)

predict = predict(tree, newdata=test, type="class")

table(test$Survived, predict)
accuracy = (98+46)/(98+46+12+22)

# 0.8089888

#
# save data
#
survived = predict(tree, newdata=check, type="class")
submit = data.frame(PassengerId = check$PassengerId, Survived = survived)
write.csv(submit, file = "TreeWithCrossValidation.csv", row.names = FALSE)

# kaggle score 0.76555

