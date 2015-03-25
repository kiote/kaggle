setwd("/Users/kiote/www/kaggle/edx/week2")
NBA = read.csv("NBA_train.csv")
str(NBA)
table(NBA$W, NBA$Playoffs)
NBA$PTSdiff = NBA$PTS - NBA$oppPTS
plot(NBA$PTSdiff, NBA$W)

WinsReg = lm(W ~ NBA$PTSdiff, data=NBA)
summary(WinsReg)

PointsReg = lm(PTS ~ X2PA + X3PA + FTA + AST + ORB + DRB + TOV + STL, BLK, data=NBA)
summary(PointsReg)

SSE = sum(PointsReg$residuals^2)
RMSE = sqrt(SSE/nrow(NBA))

NBA_test = read.csv("NBA_test.csv")
PointsPreditctions = predict(PointsReg, newdata=NBA_test)

SSE = sum((PointsPreditctions - NBA_test$PTS)^2)
SST = sum((mean(NBA$PTS)-NBA_test$PTS)^2)
R2 = 1 - SSE/SST
RMSE = sqrt(SSE/nrow(NBA_test))
