who = read.csv("WHO.csv")
str(who)
plot(who$GNI, who$FertilityRate)
library(ggplot2)

#Remember that ggplot's plot has 3 objects data, aesthetic mapping, geometric object
#Will create a ggplot object named scatterplot that has data and aesthetic mapping
scatterplot = ggplot(who, aes( x = GNI, y = FertilityRate ))
#Now will need to tell what geometrical object to  use for plotting data values e.g. points, lines etc
fertilityGNIplot = scatterplot + geom_point(col = "blue", size = 3, shape = "+") + ggtitle("Fertility rates vs GNI")
#Points will be plotted
fertilityGNIplot

#Will save the plot to a pdf
pdf("FertilityvsGNI.pdf")
print(fertilityGNIplot)
dev.off()


scatterplot = ggplot(who, aes( x = GNI, y = FertilityRate, col = Region )) + geom_point()
scatterplot
scatterplot = ggplot(who, aes( y = Under15, x = log(FertilityRate), col = Region )) + geom_point()
model = lm(Under15 ~ log(FertilityRate), data = who)
summary(model)
scatterplot = ggplot(who, aes( x = log(FertilityRate), y = Under15 )) + geom_point() + stat_smooth(method = "lm", level = 0.99, col = "red")

library(ggplot2)
household = read.csv("households.csv")
str(household)
ggplot(household, aes(x = Year, y = MarriedWChild, col = Year)) + geom_line()
#Now this is a probelm we can't plot different line graphs in the same graph
#Solution: Use melt function from reshape2 package
library(reshape2)
melt(household, id = "Year")
ggplot(melt(household, id = "Year"), aes( x = Year, y = value, col = variable)) + geom_line(size = 1.2) + geom_point(size = 4) + ylab("Percent of House")
