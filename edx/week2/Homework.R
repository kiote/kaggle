# 1
setwd("/Users/kiote/www/kaggle/edx/week2")
data = read.csv("climate_change.csv")

train = subset(data, Year <= 2006)
test = subset(data, Year > 2006)

Model = lm(Temp ~ MEI + CO2 + CH4 + N2O + CFC.11 + CFC.12 + TSI + Aerosols, data = train)

Model2 = lm(Temp ~ MEI + TSI + Aerosols + N2O, data = train)

StepModel = step(Model)

Prediction = predict(StepModel, newdata=test)
SSE = sum((Prediction - test$Temperature)^2)
SST = sum((mean(train$Temperature)-test$Temperature)^2)
R2 = 1 - SSE/SST

# 2
pisaTrain = read.csv("pisa2009train.csv")
pisaTest = read.csv("pisa2009test.csv")

with(pisaTrain, tapply(readingScore, male, mean))
pisaTrain = na.omit(pisaTrain)
pisaTest = na.omit(pisaTest)

pisaTrain$raceeth = relevel(pisaTrain$raceeth, "White")
pisaTest$raceeth = relevel(pisaTest$raceeth, "White")

lmScore = lm(readingScore ~ ., data = pisaTrain)
summary(lmScore)

SSE = sum(lmScore$residuals^2)
RMSE = sqrt(SSE/nrow(pisaTrain))

predTest = predict(lmScore, newdata=pisaTest)
SSE = sum((predTest - pisaTest$readingScore)^2)
RMSE = sqrt(SSE/nrow(pisaTest))

baseline = mean(pisaTrain$readingScore)
SSE = sum((predTest - pisaTest$readingScore)^2)
SST = sum((mean(pisaTrain$readingScore)-pisaTest$readingScore)^2)
R2 = 1 - SSE/SST

# 3
FluTrain = read.csv("FluTrain.csv")
with(FluTrain, (sort(tapply(ILI, Week, max))))
with(FluTrain, (sort(tapply(Queries, Week, max))))

plot(FluTrain$ILI)
plot(log(FluTrain$ILI), FluTrain$Queries)

FluTrend1 = lm(log(FluTrain$ILI) ~ Queries, data=FluTrain)

Correlation = cor(FluTrain$Queries, log(FluTrain$ILI))

FluTest = read.csv("FluTest.csv")
PredTest1 = exp(predict(FluTrend1, newdata=FluTest))
which(FluTest$Week == "2012-03-11 - 2012-03-17")

RE = (FluTest$ILI[11] - PredTest1[11])/FluTest$ILI[11]

SSE = sum((PredTest1 - FluTest$ILI)^2)
RMSE = sqrt(SSE/nrow(FluTest))

install.packages("/Users/kiote/Downloads/zoo_1.7-12.tgz", repos = NULL, type="source")
library(zoo)

ILILag2 = lag(zoo(FluTrain$ILI), -2, na.pad=TRUE)
FluTrain$ILILag2 = coredata(ILILag2)

plot(log(FluTrain$ILI), log(FluTrain$ILILag2))

FluTrend2 = lm(log(ILI) ~ Queries + log(ILILag2), data=FluTrain)

ILILag2Test = lag(zoo(FluTest$ILI), -2, na.pad=TRUE)
FluTest$ILILag2 = coredata(ILILag2Test)

FluTest$ILILag2[1] = FluTrain$ILI[416]
FluTest$ILILag2[2] = FluTrain$ILI[417]

PredTest2 = exp(predict(FluTrend2, newdata=FluTest))
SSE = sum((PredTest2 - FluTest$ILI)^2)
RMSE = sqrt(SSE/nrow(FluTest))
