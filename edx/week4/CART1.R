setwd("/Users/kiote/www/kaggle/edx/week4")
stevens = read.csv("stevens.csv")

library(caTools)
set.seed(3000)
spl = sample.split(stevens$Reverse, SplitRatio = 0.7)

Train = subset(stevens, spl == TRUE)
Test = subset(stevens, spl == FALSE)

install.packages("rpart")
library(rpart)
install.packages("rpart.plot")
library(rpart.plot)

#
# Decision tree
#

StevensTree = rpart(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst, data = Train, method = "class", minbucket = 25)
prp(StevensTree)

PredictCART = predict(StevensTree, newdata=Test, type="class")
table(Test$Reverse, PredictCART)

accuracy = (41+71)/(41+71+22+36)

library(ROCR)
predictROC = predict(StevensTree, newdata = Test)
pred = prediction(predictROC[,2], Test$Reverse)
pref = performance(pred, "tpr", "fpr")
plot(pref)

as.numeric(performance(pred, "auc")@y.values)

StevensTree2 = rpart(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst, data = Train, method = "class", minbucket = 5)
prp(StevensTree2)

StevensTree3 = rpart(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst, data = Train, method = "class", minbucket = 100)
prp(StevensTree3)

#
# Random forest
#
install.packages("randomForest")
library(randomForest)

Train$Reverse = as.factor(Train$Reverse)
Test$Reverse = as.factor(Test$Reverse)

StevensForest = randomForest(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst, data = Train, nodesize = 25, ntree = 200)

PredictForest = predict(StevensForest, newdata = Test)

# confusion matrix
table(Test$Reverse, PredictForest)

accuracy = (40+74)/(40+37+19+74)

#
# Cross-validation
#
install.packages("caret")
library(caret)

install.packages("e1071")
library(e1071)

numFolds = trainControl(method="cv", number=10)

cpGrid = expand.grid(.cp = seq(0.01,0.5,0.01))

train(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst, data = Train, method="rpart", trControl = numFolds, tuneGrid = cpGrid)

StevensTreeCV = rpart(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst, data = Train, method="class", cp=0.19)
PredictCV = predict(StevensTreeCV, newdata=Test, type="class")

table(Test$Reverse, PredictCV)
prp(StevensTreeCV)
