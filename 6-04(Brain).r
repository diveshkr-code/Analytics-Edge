#First read the data frame with header = FALSE so that 1st row values are not lost
#Then convert it to matrix form
#Then Morph the resultant matrix to Vector
flower = read.csv("flower.csv", header = FALSE)
flower
flowerMatrix = as.matrix(flower)
str(flowerMatrix)
flowerVector = as.vector(flowerMatrix)
#Now the flowet dataset has been conveted to a single linear vector 

#Form the distance Matrix for HIERARCIAL CLUSTERING
distances = dist(flowerVector, method = "euclidean")
#This will create the Dendrogram then we will plot the dendeogram
clusterIntensity = hclust(distances, method = "ward.D")
plot(clusterIntensity)
#Will draw rectangles around each of the cluster selected
rect.hclust(clusterIntensity, k = 3)

flowerCluster = cutree(clusterIntensity, k = 3)
flowerCluster                                #This is a vector of the same size that assigns value(denoting class) to each pixel
tapply(flowerVector, flowerCluster, mean)

image(flowerMatrix, axes = FALSE)                 #image is Plotted intenstiy as matrix is input
image(flowerMatrix, axes = FALSE, col = grey(seq(0,1,length = 256)))    #The greyscale image
dim(flowerCluster) = c(50,50)                     
#The dimension has to be given as "flowetCluster" is a vector not Matrix and plotting can only happen if input is in form of matrix
image(flowerCluster, axes = FALSE)                #This is the clustered image

######################################################################################
#Now MRI image of a healthy brain must be segmented

healthy = read.csv("healthy.csv", header = FALSE)
healthyMatrix = as.matrix(healthy)
healthyVector = as.vector(healthyMatrix)
#The healthy dataset has been read and xonverted to a vector.

image(healthyMatrix, axes = FALSE, col = grey(seq(0,1,length = 256)))
str(healthyVector)
n = 365636
n*(n-1)/2
# "distanceHealthy = dist(healthyVector, method = "euclidean")" this line will give an ERROR
#Since the computed value(total number of elements in the matrix) is so large we can't use hierarcial clustering
#We need to use k means clustering since it won't have a data overflow untill we have a large no of clusters


k = 5                     #Number of cluster
set.seed(1)                 #As the first step randomyly assigns value hence all will get diff results
KMC = kmeans(healthyVector, centers = k, iter.max = 1000)
#centers denote the number of predecided clusters 
#Remember that k neans continues untill no more improvement can be made well it can go on forever hence need to stop it with iter.max

KMC
healthyClulsters = KMC$cluster
dim(healthyClulsters) = c(566,646)
image(healthyClulsters, col = grey(seq(0,1,length(256))))
image(healthyClulsters, axes = FALSE, col = rainbow(k))


#####################
#Doing analysis of tumor image
tumor = read.csv("tumor.csv", header = FALSE)
tumorMatrix = as.matrix(tumor)
tumorVector = as.vector(tumorMatrix)
image(tumorMatrix, axes = FALSE)
#Won't apply Clustering again instead use healthy image as training set and tumor image as testing set
library(flexclust)

#kcca: k centeroid clustering algorithm now we will use the healthy image as training data and train the model the we will test it 
#upon the test data(tumor) 
KMC.kcca = as.kcca(KMC, healthyVector)
#An object(model) of type kcca had been created

tumorCluster = predict(KMC.kcca, newdata = tumorVector)
#predicting the cluster distribution using that object(model)
dim(tumorCluster) = c(nrow(tumorMatrix), ncol(tumorMatrix))
image(tumorCluster, axes = FALSE, col = rainbow(k))

