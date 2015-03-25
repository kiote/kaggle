setwd("/Users/kiote/www/kaggle/edx/week3")
framingham = read.csv("framingham.csv")
library(caTools)
set.seed(1000)
split = sample.split(framingham$TenYearCHD, SplitRatio = 0.65)

train = subset(framingham, split == TRUE)
test = subset(framingham, split == FALSE)

framinghamLog = glm(TenYearCHD ~ ., data = train, family=binomial)

predictTest = predict(framinghamLog, type="response", newdata=test)
# making confusion matrix
table(test$TenYearCHD, predictTest > 0.5)
# gives us
#    FALSE TRUE
# 0  1094    9
# 1   189   11
accuracy = (1094+11)/(1094+9+189+11)
# let's compare with baseline method
# in a baseline method we always predict 0 (good care)
baseline_accuracy = (1094 + 9)/(1094+9+189+11)
# we can see our model accuracy barely beats baseline accuracy

library(ROCR)
ROCRpred = prediction(predictTest, test$TenYearCHD)
as.numeric(performance(ROCRpred, "auc")@y.values)

# Polling data
polling = read.csv("PollingData.csv")
summary(polling)
# Install and load mice package
install.packages("mice")
library(mice)

# Multiple imputation
simple = polling[c("Rasmussen", "SurveyUSA", "PropR", "DiffCount")]
summary(simple)
set.seed(144)
imputed = complete(mice(simple))
summary(imputed)
polling$Rasmussen = imputed$Rasmussen
polling$SurveyUSA = imputed$SurveyUSA
summary(polling)
