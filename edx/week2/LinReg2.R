setwd("/Users/kiote/www/kaggle/edx/week2")
baseball = read.csv("baseball.csv")
str(baseball)
moneyball = subset(baseball, Year < 2002)
moneyball$RD = moneyball$RS - moneyball$RA
plot(moneyball$RD, moneyball$W)
WinsReg = lm(W ~ RD, data=moneyball)
summary(WinsReg)

RunsReg = lm(RS ~ OBP + SLG + BA, data=moneyball)
summary(RunsReg)
