setwd("/Users/kiote/www/kaggle/edx/week4")
gerber = read.csv("gerber.csv")
table(gerber$voting)
table(gerber$voting == 1, gerber$civicduty)
table(gerber$voting == 1, gerber$hawthorne)
table(gerber$voting == 1, gerber$self)
table(gerber$voting == 1, gerber$neighbors)

lr = glm(voting ~ civicduty + hawthorne + self + neighbors, data = gerber, family = binomial)
predict = predict(lr, type="response")

tbl = table(gerber$voting, predict  >=0.5)

accuracy = (235388)/sum(as.matrix(tbl))

library(ROCR)

ROCRperd = prediction(predict, gerber$voting)
ROCRperf = performance(ROCRperd, "tpr", "fpr")
plot(ROCRperf, colorize = TRUE, print.cutoffs.at=seq(0,1,0.1), text.adj=c(-0.3, 1.8))

predictTest = predict(lr, type="response")
ROCRpredTest = prediction(predict, gerber$voting)
auc = as.numeric(performance(ROCRpredTest, "auc")@y.values)

library(rpart)
library(rpart.plot)

tree = rpart(voting ~ civicduty + hawthorne + self + neighbors, data = gerber)
prp(tree)

CARTmodel2 = rpart(voting ~ sex + civicduty + hawthorne + self + neighbors, data=gerber, cp=0.0)
prp(CARTmodel2)

control = rpart(voting ~ control, data=gerber, cp = 0.0)
control.sex = rpart(voting ~ control + sex, data=gerber, cp = 0.0)
prp(control.sex, digits = 6)

lr = glm(voting ~ control + sex, data = gerber, family = binomial)
summary(lr)

Possibilities = data.frame(sex=c(0,0,1,1),control=c(0,1,0,1))
predict(lr, newdata=Possibilities, type="response")

LogModel2 = glm(voting ~ sex + control + sex:control, data=gerber, family="binomial")
summary(LogModel2)

predict(LogModel2, newdata=Possibilities, type="response")

#
# Letters
#
letters = read.csv("letters_ABPR.csv")
letters$isB = as.factor(letters$letter == "B")

library(caTools)
set.seed(1000)
spl = sample.split(letters$isB, SplitRatio = 0.5)

train = subset(letters, spl == TRUE)
test = subset(letters, spl == FALSE)

table(train$isB)
baseline_accuracy = 1175/(1175+383)

CARTb = rpart(isB ~ . - letter, data=train, method="class")

predictTest = predict(CARTb, newdata = test, type = "class")
table(test$isB == TRUE, predictTest)
accuracy=(1118+340)/sum(as.matrix(table(test$isB == TRUE, predictTest)))

library(randomForest)
set.seed(1000)

forest = randomForest(isB ~ . -letter, data=train)
predict.forest = predict(forest, newdata = test)
table(test$isB, predict.forest)
accuracy=(1165+374)/sum(as.matrix(table(test$isB, predict.forest)))

letters$letter = as.factor( letters$letter )
set.seed(2000)
spl = sample.split(letters$letter, SplitRatio = 0.5)
train = subset(letters, spl == TRUE)
test = subset(letters, spl == FALSE)

test$baseline = "P"
table(test$letter, test$baseline)

accuracy = 401/sum(c(table(test$letter, test$baseline)))


tree = rpart(letter ~ . -isB, data=train, method="class")
predict = predict(tree, newdata = test, type="class")
table(test$letter, predict)

accuracy = (348+318+363+340)/sum(as.matrix(table(test$letter, predict)))


forest = randomForest(letter ~ .-isB, data=train)
predict.forest = predict(forest, newdata = test)
table(test$letter, predict.forest)
aaccuracy = (390+380+393+369)/nrow(test)
aaccuracy

#
# census 
#
census = read.csv("census.csv")
set.seed(2000)

spl = sample.split(census$over50k, SplitRatio = 0.6)
train = subset(census, spl == TRUE)
test = subset(census, spl == FALSE)

logmodel = glm(over50k ~ ., data = train, family = binomial)
predict = predict(logmodel, newdata = test, type = "response")
table(test$over50k, predict >= 0.5)

accuracy = (9051+1888)/nrow(test)
baseline_accuracy = (9051+622)/nrow(test)

ROCRpredTest = prediction(predict, test$over50k)
auc = as.numeric(performance(ROCRpredTest, "auc")@y.values)

tree = rpart(over50k ~ . , data=train, method="class")
prp(tree)
predict = predict(tree, newdata = test, type="class")
table(test$over50k, predict)

accuracy = (9243+1596)/nrow(test)

predictTest = predict(tree, type="class")
ROCRpredTest = prediction(predict, test$over50k)
auc = as.numeric(performance(ROCRpredTest, "auc")@y.values)

# PROBLEM 3.1 - A RANDOM FOREST MODEL
set.seed(1)
trainSmall = train[sample(nrow(train), 2000), ]

forest = randomForest(over50k ~ ., data=trainSmall)
predict.forest = predict(forest, newdata = test)
table(test$over50k, predict.forest)
accuracy = (9586+1093)/nrow(test)
accuracy

# PROBLEM 3.2 - A RANDOM FOREST MODEL
# which variable splits more often in random forest
vu = varUsed(forest, count=TRUE)
vusorted = sort(vu, decreasing = FALSE, index.return = TRUE)
dotchart(vusorted$x, names(forest$forest$xlevels[vusorted$ix]))

# PROBLEM 3.3 - A RANDOM FOREST MODEL
# which variable takes most to impurity
varImpPlot(forest)

# PROBLEM 4.1 - SELECTING CP BY CROSS-VALIDATION
set.seed(2)
cartGrid = expand.grid( .cp = seq(0.002,0.1,0.002))
library(e1071)
library(caret)
train(over50k ~ ., data = train, method="rpart", tuneGrid = cartGrid)

model = rpart(over50k~., data=train, method="class", cp=0.002)
predict.forest = predict(model, newdata = test, type="class")
table(test$over50k, predict.forest)
accuracy = (9178+1838)/nrow(test)
accuracy
prp(model)
