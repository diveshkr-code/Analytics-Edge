polling=read.csv("PollingData.csv")
library("mice")
simple=polling[c("Rasmussen","SurveyUSA","DiffCount","PropR")]
set.seed(144)
imputed = complete(mice(simple))
#Creating a simplified data frame that only contains variables whose one/more observations are missing
#finally imputing the simplified data frame.

polling$Rasmussen = imputed$Rasmussen
polling$SurveyUSA = imputed$SurveyUSA
polling$PropR = imputed$PropR
polling$DiffCount = imputed$DiffCount
#Now the polling data frame don't contains any missing data all of them have been imputed


