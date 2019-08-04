Claims = read.csv("ClaimsData.csv")
#Will be splitting data into test and train
library(caTools)
set.seed(88)
sample = sample.split(Claims$bucket2009, SplitRatio = 0.6)
ClaimsTrain= subset(Claims, sample==TRUE)
ClaimsTest= subset(Claims, sample==FALSE)

table(ClaimsTest$bucket2009, ClaimsTest$bucket2008)
#confusion matrix for smartbaseline method

#will create a penalty matrix 
PenaltyMatrix=matrix(c(0,1,2,3,4,2,0,1,2,3,4,2,0,1,2,6,4,2,0,1,8,6,4,2,0), byrow = TRUE, nrow = 5)
#byrow specifies how the matrix is to be filled default value for byrow=FALSE (will be filled column by column)
PenaltyMatrix
as.matrix( table(ClaimsTest$bucket2009, ClaimsTest$bucket2008) )*PenaltyMatrix
sum(as.matrix( table(ClaimsTest$bucket2009, ClaimsTest$bucket2008) )*PenaltyMatrix)/nrow(ClaimsTest)

#will create a CART model that has higher accuracy and lower penalty error than smart baseline model
library(rpart)
library(rpart.plot)
ClaimsTree = rpart(bucket2009 ~ age + arthritis + alzheimers + cancer + copd + depression + diabetes + heart.failure + ihd + kidney + osteoporosis + stroke + bucket2008 + reimbursement2008 , data=ClaimsTrain, method="class", minbucket=25, cp=0.00005)
prp(ClaimsTree)
predictTest=predict(ClaimsTree, newdata = ClaimsTest, type = "class")
table(ClaimsTest$bucket2009, predictTest)
sum(as.matrix( table(ClaimsTest$bucket2009, predictTest) )*PenaltyMatrix)/nrow(ClaimsTest)

#As all error penalties are assumed as 1 hence need to input penalty matrix to ensure that the penalty error is minimized
ClaimsTree = rpart(bucket2009 ~ age + arthritis + alzheimers + cancer + copd + depression + diabetes + heart.failure + ihd + kidney + osteoporosis + stroke + bucket2008 + reimbursement2008 , data=ClaimsTrain, method="class", minbucket=25, cp=0.00005, parms=list(loss=PenaltyMatrix))
prp(ClaimsTree)
predictTest = predict(ClaimsTree, newdata = ClaimsTest, type = "class")
table(ClaimsTest$bucket2009, predictTest)
sum(as.matrix( table(ClaimsTest$bucket2009, predictTest) )*PenaltyMatrix)/nrow(ClaimsTest)

#Done we are nailing it.