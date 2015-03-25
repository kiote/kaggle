setwd("/Users/kiote/www/kagle/edx/")
WHO = read.csv("WHO.csv")
str(WHO)
summary(WHO$Under15)
which.min(WHO$Under15)
plot(WHO$GNI, WHO$FertilityRate)
outliners = subset(WHO, GNI > 10000 & FertilityRate > 2.5)
outliners[c("Country", "GNI", "FertilityRate")]

boxplot(WHO$LifeExpectancy ~ WHO$Region)

tapply(WHO$ChildMortality, WHO$Region, mean)