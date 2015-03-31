setwd("/Users/kiote/www/kaggle/edx/week4")
boston = read.csv("boston.csv")
str(boston)

plot(boston$LAT, boston$LON)
points(boston$LAT[boston$MEDV > 21.2], boston$LON[boston$MEDV>21.2], col="blue", pch=19)


# linear regression
latlonlm = lm(MEDV ~ LAT + LON, data = boston)
summary(latlonlm)

points(boston$LAT[latlonlm$fitted.values>21.2], boston$LON[latlonlm$fitted.values>21.2], col="red", pch="$")

# trees
library(rpart)
library(rpart.plot)

latlontree = rpart(MEDV ~ LAT + LON, data=boston, minbucket=50)
prp(latlontree)

plot(boston$LAT, boston$LON)
points(boston$LAT[boston$MEDV > 21.2], boston$LON[boston$MEDV>21.2], col="blue", pch=19)

fittedValues = predict(latlontree)
points(boston$LAT[fittedValues>21.2], boston$LON[fittedValues>21.2], col="red", pch="$")
abline(h=-71.07)
abline(v=42.21)

library(caTools)
set.seed(123)
split = sample.split(boston$MEDV, SplitRatio = 0.7)

train = subset(boston, split == TRUE)
test = subset(boston, split == FALSE)

linear = lm(MEDV ~ . -TOWN -TRACT, data = train)
summary(linear)

linreg.pred = predict(linear, newdata=test[ , !colnames(test) %in% c("TOWN", "TRACT")])
