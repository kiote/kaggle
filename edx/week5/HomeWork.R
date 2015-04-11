setwd("/Users/kiote/www/kaggle/edx/week5")
wiki = read.csv("wiki.csv", stringsAsFactors=FALSE)
wiki$Vandal = as.factor(wiki$Vandal)

table(wiki$Vandal)

library(tm)
corpusAdded = Corpus(VectorSource(wiki$Added))
corpusAdded = tm_map(corpusAdded, removeWords, stopwords("english"))
corpusAdded = tm_map(corpusAdded, stemDocument)
corpusAdded = tm_map(corpusAdded, PlainTextDocument)
dtmAdded = DocumentTermMatrix(corpusAdded)

sparseAdded = removeSparseTerms(dtmAdded, 0.997)

wordsAdded = as.data.frame(as.matrix(sparseAdded))
colnames(wordsAdded) = paste("A", colnames(wordsAdded))


corpusRemoved = Corpus(VectorSource(wiki$Removed))
corpusRemoved = tm_map(corpusRemoved, removeWords, stopwords("english"))
corpusRemoved = tm_map(corpusRemoved, stemDocument)
corpusRemoved = tm_map(corpusRemoved, PlainTextDocument)
dtmRemoved = DocumentTermMatrix(corpusRemoved)

sparseRemoved = removeSparseTerms(dtmRemoved, 0.997)

wordsRemoved = as.data.frame(as.matrix(sparseRemoved))
colnames(wordsRemoved) = paste("R", colnames(wordsRemoved))
ncol(wordsRemoved)


wikiWords = cbind(wordsAdded, wordsRemoved)
wikiWords$Vandal = wiki$Vandal

library(caTools)
set.seed(123)

split = sample.split(wikiWords$Vandal, SplitRatio = 0.7)
train = subset(wikiWords, split == TRUE)
test = subset(wikiWords, split == FALSE)

table(test$Vandal)

# baseline accuracy
618/(618+545)

library(rpart)
library(rpart.plot)

CART = rpart(Vandal ~ ., data=train, method="class")

prp(CART)

prediction = predict(CART, newdata = test, type = "class")

table(test$Vandal, prediction)

(618+12)/nrow(test)

wikiWords2 = wikiWords
wikiWords2$HTTP = ifelse(grepl("http",wiki$Added,fixed=TRUE), 1, 0)

table(wikiWords2$HTTP)
wikiTrain2 = subset(wikiWords2, split==TRUE)
wikiTest2 = subset(wikiWords2, split==FALSE)

CART = rpart(Vandal ~ ., data=wikiTrain2, method="class")

prp(CART)

prediction = predict(CART, newdata = wikiTest2, type = "class")

table(wikiTest2$Vandal, prediction)
(609+57)/nrow(wikiTest2)

wikiWords2$NumWordsAdded = rowSums(as.matrix(dtmAdded))
wikiWords2$NumWordsRemoved = rowSums(as.matrix(dtmRemoved))

wikiTrain3 = subset(wikiWords2, split==TRUE)
wikiTest3 = subset(wikiWords2, split==FALSE)
wikiCART3 = rpart(Vandal ~ ., data=wikiTrain3, method="class")
testPredictCART3 = predict(wikiCART3, newdata=wikiTest3, type="class")

table(wikiTest3$Vandal, testPredictCART3)

#
# 3.1
#
wikiWords3 = wikiWords2
wikiWords3$Minor = wiki$Minor
wikiWords3$Loggedin = wiki$Loggedin

wikiTrain3 = subset(wikiWords3, split==TRUE)
wikiTest3 = subset(wikiWords3, split==FALSE)
wikiCART3 = rpart(Vandal ~ ., data=wikiTrain3, method="class")
testPredictCART3 = predict(wikiCART3, newdata=wikiTest3, type="class")

table(wikiTest3$Vandal, testPredictCART3)

prp(wikiCART3)

#
# Clinical Trial
#
trials = read.csv("clinical_trial.csv", stringsAsFactors = FALSE, fileEncoding="latin1")

sort(nchar(trials$abstract))

table(nchar(trials$abstract) == 0)

which.min(nchar(trials$title))

corpusTitle = Corpus(VectorSource(trials$title))
corpusAbstract = Corpus(VectorSource(trials$abstract))

corpusTitle = tm_map(corpusTitle, PlainTextDocument)
corpusAbstract = tm_map(corpusAbstract, PlainTextDocument)

corpusTitle = tm_map(corpusTitle, content_transformer(tolower))
corpusAbstract = tm_map(corpusAbstract, content_transformer(tolower))

corpusTitle = tm_map(corpusTitle, content_transformer(removePunctuation))
corpusAbstract = tm_map(corpusAbstract, content_transformer(removePunctuation))

corpusTitle = tm_map(corpusTitle, content_transformer(removeWords), stopwords("english"))
corpusAbstract = tm_map(corpusAbstract, content_transformer(removeWords), stopwords("english"))

corpusTitle = tm_map(corpusTitle, content_transformer(stemDocument))
corpusAbstract = tm_map(corpusAbstract, content_transformer(stemDocument))

dtmTitle = DocumentTermMatrix(corpusTitle)
dtmAbstract = DocumentTermMatrix(corpusAbstract)

dtmTitle = removeSparseTerms(dtmTitle, 0.95)
dtmAbstract = removeSparseTerms(dtmAbstract, 0.95)

dtmTitle = as.data.frame(as.matrix(dtmTitle))
dtmAbstract = as.data.frame(as.matrix(dtmAbstract))

sort(colSums(dtmAbstract))

colnames(dtmTitle) = paste0("T", colnames(dtmTitle))
colnames(dtmAbstract) = paste0("A", colnames(dtmAbstract))

dtm = cbind(dtmTitle, dtmAbstract)
dtm$trial = trials$trial

set.seed(144)

split = sample.split(dtm$trial, SplitRatio = 0.7)
train = subset(dtm, split == TRUE)
test = subset(dtm, split == FALSE)

table(train$trial)

730/(730+572)

trialCART = rpart(trial ~ ., train, method = "class")
prp(trialCART)

predictTrain = predict(trialCART, type="class")

table(train$trial, predictTrain)

predictTest = predict(trialCART, newdata = test)
pred.prob = predictTest[,2]
table(predictTest, test$trial == 1)

library(ROCR)

predROCR = prediction(pred.prob, test$trial)

perfROCR = performance(predROCR, "fpr", "fnr")

plot(perfROCR, colorize=TRUE)

# Compute AUC

performance(predROCR, "auc")@y.values

#
# Spam
#
emails = read.csv("emails.csv", stringsAsFactors = FALSE)
table(emails$spam)

corpus = Corpus(VectorSource(emails$text))
corpus = tm_map(corpus , tolower)
corpus = tm_map(corpus , PlainTextDocument)
corpus = tm_map(corpus , removePunctuation)
corpus = tm_map(corpus , removeWords , stopwords("english"))
corpus = tm_map(corpus , stemDocument)
dtm = DocumentTermMatrix(corpus)

emailsSparse = removeSparseTerms(dtm, 0.95)

emailsSparse = as.data.frame(as.matrix(emailsSparse))
colnames(emailsSparse) = paste0("W", colnames(emailsSparse))

which.max(colSums(emailsSparse))

emailsSparse$spam = emails$spam

f = as.data.frame(sort(colSums(subset(emailsSparse, spam == 0))))

emailsSparse$spam = as.factor(emailsSparse$spam)

set.seed(123)
split = sample.split(emailsSparse$spam, SplitRatio = 0.7)
train = subset(emailsSparse, split == TRUE)
test = subset(emailsSparse, split == FALSE)

spamLog = glm(spam ~ ., data = train, family = "binomial")
spamCART = rpart(spam ~ ., data = train, method = "class")
spamRF = randomForest(spam ~., data = train)

predictSpamLog = predict(spamLog, newdata = test, type = "response")
predTrainCART = predict(spamCART, newdata = test)[,2]
predTrainRF = predict(spamRF, newdata = test, type="prob")[,2]
library(rpart)
library(rpart.plot)
prp(predTrainCART)

table(test$spam, predictSpamLog > 0.5)
table(test$spam, predTrainCART > 0.5)
table(test$spam, predTrainRF > 0.5)

library(ROCR)

predROCR = prediction(predTrainRF, test$spam)
performance(predROCR, "auc")@y.values

