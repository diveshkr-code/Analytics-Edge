framingham = read.csv("framingham.csv")
set.seed(1000)
library(caTools)
sample=sample.split(framingham$TenYearCHD,SplitRatio = 0.65 )
train = subset(framingham , sample == TRUE )
test  = subset(framingham , sample == FALSE)
#splits the data into training and testing data.

framinghamLog = glm(TenYearCHD ~ . , data=train , family = binomial())
summary(framinghamLog)
predictTest = predict(framinghamLog , type = "response" , newdata = test)
table(test$TenYearCHD , predictTest > 0.5)
#build a logistic model for CHD taking all the independent variables 
#created the confusion matrix and observe that
#not much accuracy improvement over the baseline model which will always say 0.

library(ROCR)
ROCRpred = prediction(predictTest , test$TenYearCHD )
as.numeric(performance(ROCRpred,"auc")@y.values)
#outputs the AUC value (absolute measure of accuracy of the model).