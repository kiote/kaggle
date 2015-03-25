setwd("/Users/kiote/www/kaggle/edx/week3")
songs = read.csv("songs.csv")
table(songs$year)

table(songs$artistname == 'Michael Jackson')

subset(songs, songs$artistname == 'Michael Jackson' & songs$Top10 == 1)

summary(songs$timesignature)
str(songs$timesignature)
unique(songs$timesignature)
sort(table(songs$timesignature),decreasing=TRUE)[1:3]

sort(table(songs$tempo),decreasing = TRUE)[1:3]

SongsTrain = subset(songs, songs$year <= 2009)
SongsTest = subset(songs, songs$year == 2010)

# except some non-numeric variables
nonvars = c("year", "songtitle", "artistname", "songID", "artistID")
SongsTrain = SongsTrain[ , !(names(SongsTrain) %in% nonvars) ]

SongsTest = SongsTest[ , !(names(SongsTest) %in% nonvars) ]

SongsLog1 = glm(Top10 ~ ., data=SongsTrain, family=binomial)
summary(SongsLog1)

cor(songs$loudness, songs$energy)

SongsLog2 = glm(Top10 ~ . - loudness, data=SongsTrain, family=binomial)
summary(SongsLog2)

SongsLog3 = glm(Top10 ~ . - energy, data=SongsTrain, family=binomial)
summary(SongsLog3)

TestPrediction = predict(SongsLog3, newdata=SongsTest, type="response")
table(SongsTest$Top10, TestPrediction >= 0.45)
#     FALSE TRUE
# 0   309    5
# 1    40   19
accuracy = (309+19)/(309+19+45)
baseline_accuracy = (309+5)/(309+19+45)

# TP/(TP+FN)
sensitivity = 19/(19+40)
# TN/(TN+FP)
specifisity = 309/(309+5)


#
# parole
#
parole = read.csv("parole.csv")
sum(parole$violator)
str(parole)

parole$state = as.factor(parole$state)
parole$crime = as.factor(parole$crime)

set.seed(144)
library(caTools)
split = sample.split(parole$violator, SplitRatio = 0.7)
train = subset(parole, split == TRUE)
test = subset(parole, split == FALSE)

ParoleLog = glm(violator ~ ., data=train, family = binomial)

predictViolation = predict(ParoleLog, newdata=test, type="response")
max(predictViolation)

table(test$violator, predictViolation > 0.5)
#     FALSE TRUE
# 0   167   12
# 1    11   12
sensetivity = 12/(12+11)
specifisity = 167/(167+12)

accuracy = (167+12)/(12+11+167+12)
base_accuracy = (167+12)/(12+11+167+12)

library(ROCR)
ROCRpred = prediction(predictViolation, test$violator)
as.numeric(performance(ROCRpred, "auc")@y.values)

# 
# 3
#
loans = read.csv("loans.csv")
summary(loans$not.fully.paid == 1)

library(mice)
set.seed(144)
vars.for.imputation = setdiff(names(loans), "not.fully.paid")
imputed = complete(mice(loans[vars.for.imputation]))
loans[vars.for.imputation] = imputed

set.seed(144)
split = sample.split(loans, SplitRatio = 0.7)
train = subset(loans, split == TRUE)
test = subset(loans, split == FALSE)

mod = glm(not.fully.paid  ~ ., data=train, family = binomial)
summary(mod)

predicted.risk = predict(mod, newdata=test, type="response")
table(test$not.fully.paid, predicted.risk >= 0.5)
test$predicted.risk = predicted.risk

accuracy = (2895+16)/(2895+16+18+492)

library(ROCR)
ROCRpred = prediction(predicted.risk, test$not.fully.paid)
as.numeric(performance(ROCRpred, "auc")@y.values)

mod2 = glm(not.fully.paid  ~ int.rate, data=train, family = binomial)
summary(mod2)

predicted.risk2 = predict(mod2, newdata=test, type="response")
max(predicted.risk2)

table(test$not.fully.paid, predicted.risk2 >= 0.5)

ROCRpred = prediction(predicted.risk2, test$not.fully.paid)
as.numeric(performance(ROCRpred, "auc")@y.values)

test$profit = exp(test$int.rate*3) - 1
test$profit[test$not.fully.paid == 1] = -1
max(test$profit) * 10

highInterest = subset(test, test$int.rate > 0.15)
summary(highInterest$profit)

table(highInterest$not.fully.paid)

str(highInterest$predicted.risk)

#
# PROBLEM 6.2
#
loans = read.csv("loans_imputed.csv")

cutoff = sort(highInterest$predicted.risk, decreasing=FALSE)[100]
selectedLoans = subset(highInterest, predicted.risk <= cutoff)

profitsum = sum(selectedLoans$profit)
sum(selectedLoans$not.fully.paid)
