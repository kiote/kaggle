setwd("/Users/kiote/www/kaggle/edx/week3")
quality = read.csv("quality.csv")
str(quality)

install.packages("caTools")
library(caTools)
set.seed(88)

split = sample.split(quality$PoorCare, SplitRatio = 0.75)
qualityTrain = subset(quality, split == TRUE)
qualityTest = subset(quality, split == FALSE)

QualityLog = glm(PoorCare ~ OfficeVisits + Narcotics, data = qualityTrain, family = binomial)
summary(QualityLog)

predictTrain = predict(QualityLog, type = "response")
tapply(predictTrain, qualityTrain$PoorCare, mean)

QualityLog2 = glm(PoorCare ~ StartedOnCombination + ProviderCount, data=qualityTrain, family = binomial)

table(qualityTrain$PoorCare, predictTrain > 0.5)

install.packages("ROCR")
library(ROCR)


ROCRperd = prediction(predictTrain, qualityTrain$PoorCare)
ROCRperf = performance(ROCRperd, "tpr", "fpr")
plot(ROCRperf, colorize = TRUE, print.cutoffs.at=seq(0,1,0.1), text.adj=c(-0.3, 1.8))

predictTest = predict(QualityLog, type="response", newdata=qualityTest)
ROCRpredTest = prediction(predictTest, qualityTest$PoorCare)
auc = as.numeric(performance(ROCRpredTest, "auc")@y.values)
