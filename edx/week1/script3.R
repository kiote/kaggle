setwd("/Users/kiote/www/kagle/edx/")
CPS = read.csv("CPSData.csv")
sort(table(CPS$Industry), decreasing=TRUE)[1]

summary(CPS$Citizenship)

hispanic = subset(CPS, Hispanic == 1)

table(CPS$Region, is.na(CPS$Married))

sort(with(CPS, tapply(Hispanic, MetroArea, mean)))
CPS = merge(CPS, CountryMap, by.x="CountryOfBirthCode", by.y="Code", all.x=TRUE)
