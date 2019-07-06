quality=read.csv("quality.csv")
set.seed(88)
library(caTools)
sample=sample.split(quality$PoorCare, SplitRatio = 0.75)
quality_train=subset(quality, sample==TRUE)
quality_test=subset(quality, sample==FALSE)
#read the data set and split it into training and testing data set by using caTools set.seed(value) ensures that 
# in the class have identical split.

QualityLog=glm(PoorCare ~ OfficeVisits + Narcotics , quality_train , family = binomial())
predictTrain=predict(QualityLog, type = "response")
#makes a logistic regression model and predict the variable poor care in terms of probablity (due to "response")
tapply(predictTrain,quality_train$PoorCare,mean)
table(quality_train$PoorCare , predictTrain > 0.5)
#gett the mean probablity for poor and good cases and creates the confusion matrix with 
#0.5 being the threshold value

library(ROCR)
ROCRpred=prediction(predicTrain , quality_train$PoorCare)
ROCRperf=performance(ROCRpred , "tpr" , "fpr")
plot(ROCRperf,colorize=TRUE , pront.cutoff.at=seq(0,1,0.1) , text.adj=c(-0.2,1.7))
#The ROCR curve is plotted


predictTrain = predict(QualityLog , newdata = quality_train , type = "response")
summary(predictTrain)
tapply(predictTrain , quality_train$PoorCare , mean)
table(quality_train$PoorCare , predictTrain > 0.35 )
predictTest = predict(QualityLog , newdata = quality_test , type = "response")
summary(predictTest)
tapply(predictTest , quality_test$PoorCare , mean)
table(quality_test$PoorCare , predictTest > 0.35 )
#You get the confusion matrix for both the trianing and testing data with threshold t=0.35
# t is less than 0.5 as True Positive(Poor Care detections) must be high
