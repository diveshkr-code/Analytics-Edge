polling=read.csv("PollingData.csv")
library("mice")
set.seed(144)
simple=polling[c("Rasmussen","SurveyUSA","DiffCount","PropR")]
imputed = complete.cases(mice(simple))
#Creating a simplified data frame that only contains variables whose one/more observations are missing
#finally imputing the simplified data frame

polling$Rasmussen = imputed$Rasmussen
polling$SurveyUSA = imputed$SurveyUSA
polling$PropR = imputed$PropR
polling$DiffCount = imputed$DiffCount
