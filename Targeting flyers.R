#Name: MansiJoshi
#Student id: 300317856
#CSIS3360 Assignment 1

#1. Import the data into RStudio and drop the first column.
#   Standardize the data using scale() function as follows: datat1 <- scale(data)

data1 <- read.csv("EastWestAirlinesCluster.csv",header = TRUE,sep = ",")
data1 <- data1[,-c(1)] #remove first column
data2 <- scale(data1) #scale the dataframe
head(data2)

#2. Using scaled data, determine the optimal number of clusters 
#   using both Scree plot and NbClust() approaches.
pkgs <- c("factoextra",  "NbClust")
install.packages(pkgs)
install.packages("factoextra")
remove.packages("NbClust")
install.packages("NbClust")
install.packages('tidyverse') #install.packages(pkgs) and load
library(tidyverse)  # data manipulation
library(cluster)    # clustering algorithms
library(factoextra) # clustering visualization
library(NbClust)    # clustering visualization

set.seed(5)
fviz_nbclust(data2, kmeans, method = "wss") #determine optimal clusters - Scree plot

set.seed(5)
res.nbclust <- NbClust(data2, distance = "euclidean",
                       min.nc = 2, max.nc = 10, 
                       method = "kmeans", index ="all") #determine optimal clusters - NbClust()
fviz_nbclust(res.nbclust) #visualize the results (histogram) to show optimal no of clusters (2 in this case)

#3. Use k-means clustering with the number of clusters that you found above in 2 above

set.seed(5)               #set starting seed use set-seed() function)
kmResult <- kmeans(data2, 2)
str(kmResult)             #visualize contents of kmResult
fviz_cluster(kmResult, data2, ellipse.type = "convex")  # Visualize kmeans clustering
library(cluster)

clusplot(data2, kmResult$cluster, main="2D Respresentation of Cluster Solution",
         color=TRUE, shade=TRUE, labels=2,lines=0)

#4. Validate k-means results using Silhouette coefficient
silOut <- silhouette(kmResult$cluster,dist(data2, method="euclidean")) #compute silhouette measure

plot(silOut, main="Silhoutte Plot: K-means Results",    #plot silOut values
     col = c("red", "green", "blue"))


#5. Generate a table to show the distribution (i.e., number) of 
#   observations/records across
table(kmResult$cluster)   #display cluster distribution


#6. Generate a summary table of cluster centers (i.e., cluster means) 
#   for the variables used
data.frame(kmResult$centers) 

#7. Repeat the analysis using hierarchical clustering with Euclidean 
#   distance and Ward's method
dismax <-dist(data2, method="euclidean") #generate the dissimilarity matrix (distance matrix)
hclustOut <-hclust(dismax, method="ward.D") #run the hierarchical clustering algorithm


#8. Plot a dendogram for hierarchical clustering results
plot(hclustOut, cex=0.6, hang = -1, main="Dendogram from hclust Algorithm")
fviz_dend(hclustOut, k=2, cex=0.5, main="Dendogram from hclust Analysis") #colors


#9. Cut the tree (i.e., dendogram) using the optimal number 
#   of clusters you determined in step 2 and generate a table to 
#   show the distribution of observations (i.e., number of observations) 
#   across the clusters

groups <-cutree(hclustOut, k=2) #cut
table(groups) #observations

#10. Generate a summary table of cluster centers (i.e., cluster means) 
#    for the variables used in analysis.
str(hclustOut)

clust.centroid = function(i,dat,clusters){
  ind = (clusters==i)
  colMeans(dat[ind,])
}

cluster_cent <- data.frame(sapply(unique(groups), clust.centroid ,data2,groups))
hclustResult <- print (cluster_cent)


# Reference: 
#https://www.rdocumentation.org/packages/factoextra/versions/1.0.7/topics/fviz_cluster
