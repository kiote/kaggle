setwd("/Users/kiote/www/kaggle/edx/week2")
wine = read.csv("wine.csv")

model1 = lm(Price ~ AGST, data=wine)
summary(model1)

model3 = lm(Price ~ AGST + HarvestRain + WinterRain + Age + FrancePop, data=wine)
