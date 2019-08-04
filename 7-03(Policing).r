mvt = read.csv("mvt.csv", stringsAsFactors = FALSE)
str(mvt)

#Need to convert the date varibale to format that R will recognize
mvt$Date = strptime(mvt$Date, format = "%m/%d/%y %H:%M")
#Now we can extract the weekday hour minute etc
mvt$weekday = weekdays(mvt$Date)
mvt$hour = mvt$Date$hour

#Will create a line plot
table(mvt$weekday)
WeekdayCounts = as.data.frame(table(mvt$weekday))
str(WeekdayCounts)

library(ggplot2)
ggplot(WeekdayCounts, aes( x = Var1, y = Freq)) + geom_line( aes(group = 1) )
#group = 1 tells that only one line is neccassay
#The days of the week in Var1 are in alphabetical order but we need in chronological order
WeekdayCounts$Var1 = factor(WeekdayCounts$Var1, ordered = TRUE, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))
ggplot(WeekdayCounts, aes( x = Var1, y = Freq)) + geom_line( aes(group = 1) ) + xlab("Day of the Week") + ylab("Total Motor Vehicle Theft")


#will now have hour of the day in x axis and the Freq in y axis
table(mvt$weekday, mvt$hour)
DayHourCounts = as.data.frame(table(mvt$weekday, mvt$hour))
str(DayHourCounts)
DayHourCounts$Hour = as.numeric(as.character(DayHourCounts$Var2))
#Will draw seven lines for each weekday
ggplot(DayHourCounts, aes( x = Hour, y = Freq)) + geom_line(aes(group = Var1, col = Var1))

#Heat map will be more inferencial
#First fix the order of the day
DayHourCounts$Var1 = factor(DayHourCounts$Var1, ordered = TRUE, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))
ggplot(DayHourCounts, aes( x = Hour,y = Var1)) +geom_tile(aes(fill = Freq)) + ylab("Weekday") 
#to change the gradient of the heat map
ggplot(DayHourCounts, aes( x = Hour,y = Var1)) +geom_tile(aes(fill = Freq)) + ylab("Weekday") + scale_fill_gradient(name = "Total mv theft", low = "white", high = "red") + theme(axis.title.y = element_blank())

#Now will plot the heat map in a geographical map
library("maps")
library("ggmap")
chicago = get_map(location = "chicago", zoom = 11)
#Now ggmap requires a Google API we will do this with the leaflet package

