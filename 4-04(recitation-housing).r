boston = read.csv("boston.csv")
str(boston)
plot(boston$LON, boston$LAT, col="Black", pch=18)
points(boston$LON[boston$CHAS==1], boston$LAT[boston$CHAS==1], col="Blue", pch=18)
points(boston$LON[boston$TRACT==3531], boston$LAT[boston$TRACT==3531], col="Red", pch=18)
summary(boston$NOX)
points(boston$LON[boston$NOX > mean(boston$NOX)], boston$LAT[boston$NOX > mean(boston$NOX)], col="Green", pch=18)
summary(boston$MEDV)
plot(boston$LON, boston$LAT, col="Red", pch=18)
points( boston$LON[boston$MEDV >= mean(boston$MEDV)] , boston$LAT[boston$MEDV >= mean(boston$MEDV)], col="Green", pch=18)

#Trying to get a relationship with linear regression
plot(boston$LON, boston$MEDV)
plot(boston$LAT, boston$MEDV)
latlonlm = lm(MEDV ~ LON + LAT, data = boston)
summary(latlonlm)
plot(boston$LON, boston$LAT, col="Black", pch=18)
points( boston$LON[boston$MEDV >= median(boston$MEDV)] , boston$LAT[boston$MEDV >= median(boston$MEDV)], col="Red", pch=18)
latlonlm$fitted.values
points(boston$LON[latlonlm$fitted.values >= median(boston$MEDV)], boston$LAT[latlonlm$fitted.values >= median(boston$MEDV)], col="Blue", pch="$")

#Now will look at analysis from regression trees
library(rpart)
library(rpart.plot)
latlontree = rpart(MEDV ~ LON + LAT, data = boston)
prp(latlontree)
#minbucket is not defined hence tree has many splits
plot(boston$LON, boston$LAT, col="Black", pch=18)
points( boston$LON[boston$MEDV >= median(boston$MEDV)] , boston$LAT[boston$MEDV >= median(boston$MEDV)], col="Red", pch=18)
fittedvalues = predict(latlontree)
points( boston$LON[fittedvalues >= median(boston$MEDV)] , boston$LAT[fittedvalues >= median(boston$MEDV)], col="Blue", pch="$")

latlontree = rpart(MEDV ~ LON + LAT, data = boston, minbucket=50)
plot(latlontree)
text(latlontree, cex=0.7)
plot(boston$LON, boston$LAT, col="Black", pch=18)
abline(h=42.28)
abline(h=42.17)
abline(h=42.21)
abline(v=-71.07)
points( boston$LON[boston$MEDV >= mean(boston$MEDV)] , boston$LAT[boston$MEDV >= mean(boston$MEDV)], col="Red", pch=18)
fittedvalues = predict(latlontree)
points( boston$LON[fittedvalues >= median(boston$MEDV)] , boston$LAT[fittedvalues >= median(boston$MEDV)], col="Blue", pch="$")

#We will quantitatively look at how regression trees can predict data better than linear regression
library(caTools)        #First split the data
set.seed(123)
sample = sample.split(boston$MEDV, SplitRatio = 0.7)
train = subset(boston, sample==TRUE)
test = subset(boston, sample==FALSE)
linreg = lm(MEDV ~ LAT + LON + CRIM + ZN + INDUS + CHAS + NOX + RM + AGE + DIS + RAD + TAX + PTRATIO , data=train)
summary(linreg)

#Calclating sse and rmse for linear regression model on test data
linreg.pred = predict(linreg, newdata=test)
linreg.sse = sum((linreg.pred - test$MEDV)^2)
linreg.sse


#Now will be building a regression tree
tree = rpart(MEDV ~ LAT + LON + CRIM + ZN + INDUS + CHAS + NOX + RM + AGE + DIS + RAD + TAX + PTRATIO , data=train)
prp(tree)
tree.pred = predict(tree, newdata = test)
tree.sse = sum((tree.pred - test$MEDV)^2)
tree.sse


#Will try ro build regression tree with cross validation 
library(caret)
library(e1071)
numFolds = trainControl(method = "cv", number = 10 )
cpGrid = expand.grid( .cp = (0:10)*0.0005 )        
tr = train(MEDV ~ LAT + LON + CRIM + ZN + INDUS + CHAS + NOX + RM + AGE + DIS + RAD + TAX + PTRATIO , data=train, method = "rpart", trControl=numFolds, tuneGrid=cpGrid)
tr
best.tree = tr$finalModel
prp(best.tree)
best.tree.pred = predict(best.tree, newdata = test)
best.tree.sse = sum((test$MEDV-best.tree.pred)^2)
best.tree.sse
#Although cross validation makes model better it stils performs poorly than linear regression model