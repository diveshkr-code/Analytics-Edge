emails = read.csv("energy_bids.csv", stringsAsFactors = FALSE)
str(emails)  
emails$email[2]
emails$responsive[2]
table(emails$responsive)

library(tm)
library(SnowballC)
corpus = Corpus(VectorSource(emails$email))
corpus = tm_map(corpus, tolower)
corpus[[1]]$content
corpus = tm_map(corpus, removePunctuation)
corpus = tm_map(corpus, removeWords, stopwords("english"))
corpus = tm_map(corpus, stemDocument)

#Will build a document term matrix 
dtm = DocumentTermMatrix(corpus)
dtm
dtm = removeSparseTerms(dtm, 0.97)
#Will convert into a usabel data frame 
labelledTerms = as.data.frame( as.matrix(dtm) )
labelledTerms$responsive = emails$responsive
str(labelledTerms)

#Will split the data into training and testing data
library(caTools)
set.seed(144)
sample = sample.split(labelledTerms$responsive, SplitRatio = 0.7)
Train = subset(labelledTerms, sample == TRUE)
Test = subset(labelledTerms, sample == FALSE)

#Will build a CART model
library(rpart)
library(rpart.plot)
emailCART = rpart(responsive ~ ., data = Train, method = "class")
prp(emailCART)
pred = predict(emailCART, newdata = Test)
#not using type = class hence we will get probablities
table(Test$responsive, pred[,2] >= 0.5)

#Will plot the ROCR curve
library(ROCR)
ROCRpred = prediction(pred[,2], Test$responsive)
ROCRperf = performance(ROCRpred,"tpr", "fpr")
plot(ROCRperf, colorize = TRUE)
auc = as.numeric(performance(ROCRpred, "auc")@y.values)
auc
