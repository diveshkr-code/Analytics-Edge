#data is in txt file not csv
movies = read.table("MovieLens.txt", header = FALSE, sep = "|", quote = "\"")
#sep stands for seperator
str(movies)

#Column names(independent variables) are missing hence need to add ourselves
colnames(movies) = c("ID", "Title", "ReleaseDate", "VideoReleaseDate", "IMDB", "Unknown", "Action", "Adventure", "Animation", "Childrens", "Comedy", "Crime", "Documentary", "Drama", "Fantasy", "FilmNoir", "Horror", "Musical", "Mystery", "Romance", "SciFi", "Thriller", "War", "Western")
#Won't be using the ID videorelease date Release Date and the IMDB hence we will remove them
movies$ID = NULL
movies$ReleaseDate = NULL
movies$VideoReleaseDate = NULL
movies$IMDB = NULL

#If there are some duplicate observations in data you can use unique function
movies = unique(movies)

#Will do hierarchial clustering
#2 step process 
#1.Compute the distance between all the points
#2.Cluster the points

distances = dist(movies[2:20], method = "euclidean")
clusterMovies = hclust(distances, method = "ward.D")
#method="ward" compute distance betwn clusters by centeroid method and also takes variance in clusters into account
#will now plot the dendogram
plot(clusterMovies)
#Will now label each of the data points according to which cluster it belongs to
clusterGroups = cutree(clusterMovies, k = 10)
tapply(movies$Action, clusterGroups, mean)
tapply(movies$Romance, clusterGroups, mean)

#We are done clustering now will build the recommmendation system
clusterGroups[257]
cluster2 = subset(movies, clusterGroups==2)
cluster2$Title[1:10]
