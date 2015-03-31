setwd("/Users/kiote/www/kaggle/edx/week4")
Claims = read.csv('ClaimsData.csv')

# buckets
table(Claims$bucket2009)/nrow(Claims)

library(caTools)
set.seed(88)
spl = sample.split(Claims$bucket2009, SplitRatio = 0.6)

ClaimsTrain = subset(Claims, spl == TRUE)
ClaimsTest = subset(Claims, spl == FALSE)

# baseline (predict the same outcome as at 2008)
table(ClaimsTest$bucket2009, ClaimsTest$bucket2008)
baseline_accuracy = (110138+10721+2774+1539+104)/nrow(ClaimsTest)

PenaltyMatrix = matrix(c(0,1,2,3,4,2,0,1,2,3,4,2,0,1,2,6,4,2,0,1,8,6,4,2,0), byrow=TRUE, nrow=5)

as.matrix(table(ClaimsTest$bucket2009, ClaimsTest$bucket2008))*PenaltyMatrix
baseline_penalty_error = sum(as.matrix(table(ClaimsTest$bucket2009, ClaimsTest$bucket2008))*PenaltyMatrix)/nrow(ClaimsTest)

# baseline (predict most frequent outcome, which is first bucket)
ClaimsTest$bucket.baseline = 1
table(ClaimsTest$bucket2009, ClaimsTest$bucket.baseline)

accuracy = 122978/nrow(ClaimsTest)

PenaltyVector = c(0,2,4,6,8)
sum(as.vector(table(ClaimsTest$bucket2009, ClaimsTest$bucket.baseline))*PenaltyVector)/nrow(ClaimsTest)

# CART model
library(rpart)
library(rpart.plot)

ClaimsTree = rpart(bucket2009 ~ . - reimbursement2009, data = ClaimsTrain, method="class", cp=0.00005)
prp(ClaimsTree)

PredictTest = predict(ClaimsTree, newdata = ClaimsTest, type="class")
table(ClaimsTest$bucket2009, PredictTest)

accuracy = (114141+16102+118+201)/nrow(ClaimsTest)

penalty_error = sum(as.matrix(table(ClaimsTest$bucket2009, PredictTest))*PenaltyMatrix)/nrow(ClaimsTest)
# penalty error greather than for baseline model.. 
# so let's add penalty matrix to model:
ClaimsTree = rpart(bucket2009 ~ . - reimbursement2009, data = ClaimsTrain, method="class", cp=0.00005, parms = list(loss=PenaltyMatrix))
PredictTest = predict(ClaimsTree, newdata = ClaimsTest, type="class")
table(ClaimsTest$bucket2009, PredictTest)

accuracy2 = (94310+18942+4692+636+2)/nrow(ClaimsTest)
penalty_error2 = sum(as.matrix(table(ClaimsTest$bucket2009, PredictTest))*PenaltyMatrix)/nrow(ClaimsTest)
