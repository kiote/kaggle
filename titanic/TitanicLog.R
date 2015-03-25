setwd("/Users/kiote/www/kaggle/titanic")
data = read.csv("train.csv")
summary(data)
str(data)

# fill in missing data, making numeric features
# first let's make numeric sex
data$SexInt = 0
data$SexInt[data$Sex == 'male'] = 1

cor(data[c("Survived", "Fare", "SexInt", "Age", "Pclass")])

# let's try to fill in missing ages
library(mice)
simple = data[c("Survived", "Fare", "SexInt", "Age", "Pclass")]
imputed = complete(mice(simple))
summary(imputed)

data$Age = imputed$Age
summary(data)

data$EmbarkedInt = 0
data$EmbarkedInt[data$Embarked == 'S'] = 1
data$EmbarkedInt[data$Embarked == 'C'] = 2
data$EmbarkedInt[data$Embarked == 'Q'] = 3

summary(data)

data$Has.Cabin = 0
data$Has.Cabin[data$Cabin != '']=1

# split data to train and test
library(caTools)
set.seed(1000)
split = sample.split(data$Survived, SplitRatio = 0.65)

train = subset(data, split == TRUE)
test = subset(data, split == FALSE)

# making model
model = glm(Survived ~ Pclass + Age + SibSp + Parch + Fare + SexInt + EmbarkedInt + Has.Cabin, data=train, family = binomial)
summary(model)

# prediction vector
predictTest = predict(model, type="response", newdata=test)
table(test$Survived, predictTest > 0.4)

accuracy = (160 + 91)/(160 + 91 + 32 + 29)
# baseline = everybody die
baseline_accuracy = (166 + 26)/(166 + 84 + 26 + 36)
# wow! model is much better, than baseline!

library(ROCR)
ROCRpred = prediction(predictTest, test$Survived)
as.numeric(performance(ROCRpred, "auc")@y.values)

#
# save result 
#
result = read.csv("test.csv")

# fill in missing data, making numeric features
# first let's make numeric sex
result$SexInt = 0
result$SexInt[result$Sex == 'male'] = 1

simple = result[c("Fare", "SexInt", "Age", "Pclass")]
imputed = complete(mice(simple))
summary(imputed)

result$Age = imputed$Age

result$EmbarkedInt = 0
result$EmbarkedInt[result$Embarked == 'S'] = 1
result$EmbarkedInt[result$Embarked == 'C'] = 2
result$EmbarkedInt[result$Embarked == 'Q'] = 3

result$Has.Cabin = 0
result$Has.Cabin[result$Cabin != '']=1

predictResult = predict(model, type="response", newdata=result)
result$Survived = 0
result$Survived[predictResult > 0.4] = 1
submit = data.frame(PassengerId = result$PassengerId, Survived = result$Survived)
write.csv(submit, file = "logisticReg.csv", row.names = FALSE)

# kaggle score 0.74163