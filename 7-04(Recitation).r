library(ggplot2)
intl = read.csv("intl.csv", stringsAsFactors = FALSE)
str(intl)
intl$Region = factor(intl$Region, ordered = TRUE, levels = c("Asia", "Europe", "North America", "Latin Am. & Caribbean", "Middle East", "Africa", "Oceania", "Unknown or Stateless"))
ggplot(intl, aes( x = Region, y = PercentOfIntl)) + geom_bar(stat = "identity") + geom_text(aes(label = PercentOfIntl*100))
#Stat = identity tells that we need to use the y values as the height of the bar
ggplot(intl, aes( x = Region, y = PercentOfIntl)) +
   geom_bar(stat = "identity", fill = "Dark Blue") + 
    geom_text(aes(label = PercentOfIntl*100, vjust = -0.4)) +
      ylab("% of Students") + ggtitle("International Students by Region") + 
        theme(axis.title = element_blank(), axis.text.x = element_text(angle = 45, hjust = 1))
#vjust = -0.4 pushes the text up a little 


#Will now plot such that all the country reflect individual nuo of students
library(ggmap)
intlall = read.csv("intlall.csv", stringsAsFactors = FALSE)
str(intlall) 
intlall[is.na(intlall)] = 0
world_map = map_data("world")
str(world_map)

#Will merge world_map and intlall together
world_map = merge(world_map, intlall, by.x = "region", by.y = "Citizenship")
str(world_map)
#As you can see the data has been reordered this will create problems in plot hence we will reorder it according to region(country)
#then according to order(order no)
world_map = world_map[ order(world_map$group, world_map$order),]
ggplot(world_map, aes( x = long, y = lat, group = group)) + geom_polygon( aes(fill = "white"), color = "blue") + coord_map("mercator")
#We can see China isn't there thats because it is saved as different names in 'world_map' and 'intlall' hence dropped while metging
intlall$Citizenship[intlall$Citizenship == "China (People's Republic Of)"] = "China"
#Now will do everything again so as to get China note some parts of Africa won't come as there are no students there

world_map = map_data("world")
world_map = merge(world_map, intlall, by.x = "region", by.y = "Citizenship")
str(world_map)
world_map = world_map[ order(world_map$group, world_map$order),]
ggplot(world_map, aes( x = long, y = lat, group = group)) + geom_polygon( aes(fill = Total), color = "blue") + coord_map("mercator")
#Now China is also here

#Lets now look at orthographic(3D) projection
ggplot(world_map, aes( x = long, y = lat, group = group)) + geom_polygon( aes(fill = Total), color = "blue") + coord_map("ortho", orientation = c(20,30,0))


library(ggplot2)
household = read.csv("households.csv")
str(household)
ggplot(household, aes(x = Year, y = MarriedWChild, col = Year)) + geom_line()
#Now this is a probelm we can't plot different line graphs in the same graph
library(reshape2)
melt(household, id = "Year")
ggplot(melt(household, id = "Year"), aes( x = Year, y = value, col = variable)) + geom_line()

