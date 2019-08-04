#read data frame and split it into training and testing set
stevens=read.csv("stevens.csv")
str(stevens)
library(caTools)          #library for splitting data 
set.seed(3000)
sample = sample.split(stevens$Reverse , SplitRatio = 0.7)
Train = subset(stevens , sample == TRUE)
Test = subset(stevens, sample==FALSE)

#Plots a CART tree with minbucket set at 25 the model is StevenTree prp plots that CART tree
library(rpart)
library(rpart.plot)
StevenTree = rpart(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst,data=Train , method="class" , minbucket=25)
prp(StevenTree)

#type="class"  same as predicting with threshold value = 0.5 if type="class" was not there then probablities would be outputed
predictCART= predict(StevenTree, newdata = Test, type="class")
table(Test$Reverse, predictCART)

#now we will plot the ROC curve 
library(ROCR)
predictROC = predict(StevenTree, newdata = Test)       #predicting without type="class" hence threshold value not specified
predictROC
pred = prediction(predictROC[,2], Test$Reverse)         
#creates a prediction object that will be used to create ROC curve the actual value(Test$Reverse) and the probablity(response) 
#is given.Also notice that the function used is prediction not predict
perf = performance(pred, "tpr", "fpr")
plot(perf)
as.numeric(performance(pred, "auc")@y.values)
#outputs the auc value

library(randomForest)
set.seed(200)
Test$Reverse = as.factor(Test$Reverse)
Train$Reverse = as.factor(Train$Reverse)
#as.factor ensures that R now's Reverse variable has limited no of possible values this has to be done for dependent variable in 
#every calssification problem.
StevensForest = randomForest(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst,data=Train , nodesize=25, ntree=200)
predictForest = predict(StevensForest, newdata = Test)
table(Test$Reverse, predictForest)
#nodesize is same as minbacket and ntree is no of tree in the forest

#optimal parameter selection for Random Forests
library(caret)
library(e1071)
numFolds = trainControl(method="cv", number = 10)       #10 folds will be constructed
cpGrid = expand.grid(.cp=seq(0.01, 0.5, 0.01))          #defines all possible values of cp this is like the minbucket for Forests 
#Performing Cross Validation method="rpart" since we are cross validating a CART model 
train(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst, data = Train, method="rpart", trControl = numFolds, tuneGrid = cpGrid)

#Creating a Random Forest with obtained value of cp instead of using minbucket
StevenTreeCV = rpart(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst, data=Train, method = "class", cp=0.17)
predictCV = predict(StevenTreeCV, newdata=Test, type="class")
table(Test$Reverse, predictCV)
#Essentailly CROSS VALIDATION helps us to select a good parameter value
