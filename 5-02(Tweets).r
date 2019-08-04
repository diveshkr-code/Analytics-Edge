tweets = read.csv("tweets.csv", stringsAsFactors = FALSE)
str(tweets)
tweets$Negative = as.factor(tweets$Avg <= -1)

#can't preprocess untill all the text is in form of a corpus(Collection of documents) This will be done
library(tm)
library(SnowballC)
corpus =  Corpus(VectorSource(tweets$Tweet))
corpus
corpus[["1"]]$content

#Now let's start preprocessing

corpus = tm_map(corpus, tolower)
corpus[["1"]]$content
#All text are in lowercase

corpus = tm_map(corpus, removePunctuation)
corpus[["1"]]$content
#Punctuations have been removed

stopwords("english")[1:10]
corpus = tm_map(corpus, removeWords, c("apple", stopwords("english") ))
corpus[["1"]]$content
#All stop words have been removed

corpus = tm_map(corpus, stemDocument)
corpus[["1"]]$content
#Text have been stemmed 
#Preprocessing completed

frequencies = DocumentTermMatrix(corpus)
frequencies
inspect( frequencies[1000:1005, 505:515] )
#shows the matrix from document 1000-1015 and the terms from 505-515
findFreqTerms(frequencies, lowfreq = 20)
#Shows the most popular terms in the text all will have freq > 20

sparse = removeSparseTerms(frequencies, 0.995) 
#keeps only those terms that have occured in more than 0.5% of all tweets
sparse
tweetsSparse = as.data.frame(as.matrix(sparse))
tweetsSparse
colnames(tweetsSparse) = make.names(colnames(tweetsSparse))
#Do this for every text analytics problem to ensure that all the variables are stored appropriately 

tweetsSparse$Negative = tweets$Negative
#adds a new column 
library(caTools)
set.seed(123)
sample = sample.split(tweetsSparse$Negative, SplitRatio = 0.7)
tweetTrain = subset(tweetsSparse, sample == TRUE)
tweetTest = subset(tweetsSparse, sample == FALSE)
#split the data into training and test data


#Will use CART to build a predictive model
library(rpart)
library(rpart.plot)
tweetCART = rpart(Negative ~ ., data=tweetTrain, method = "class")
#method="class" tells to build a classification tree and not a regression tree
prp(tweetCART)
#Will make predictions with the model
predictCART = predict(tweetCART, newdata = tweetTest, type="class")
table(tweetTest$Negative, predictCART)
#Does pretty well than the baseline model check

#Now will make a random forest model
library(randomForest)
set.seed(123) 
tweetRF = randomForest(Negative ~ ., data=tweetTrain, method = "class")
#nodesize and ntrees not set hence they will take defailt values
#Will predict using the forest model created
predictRF = predict(tweetRF, newdata = tweetTest, type="class")
table(tweetTest$Negative, predictRF)
