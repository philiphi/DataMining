# 07/10/2013 
# From Data Mining with Rattle & R 
# Chapter 9 Clustering

# kmeans()is provided directly through R by the standard stats package.

# Prior to clustering data, you may want to 
#   remove or estimate missing data and 
#   rescale variables for comparability.


# Goal of clustering => identify groups of observations that are close together but as a
# group are quite separate from other groups

# k-means algorithm
# identifies a collection of k clusters using a heuristic search
# starting with a selection of k randomly chosen clusters

# Observations in the dataset are associated with their closest "mean" and thus are partitioned into k clusters

# Example: 
# Step1  get small subset of weather data
  library(rattle)
  set.seed(42)
  obs1 <- sample(1:nrow(weather), 5)
  vars <- c("MinTemp", "MaxTemp",
              "Rainfall", "Evaporation")
  cluster1 <- weather[obs1, vars]

# Step2
  # We now obtain the means of each of the variables
  # The vector of means then represents one of the clusters within our set of k clusters
#   mean(cluster1) # throws NA error...hmmm
  sapply(cluster1, mean)
  
# Step3
  # A different sample would yield different means
  # If we were to simply partition the weather dataset into
  # ten sets (a common value for k), we would get ten sets of means for each of the four variables.

  
#k-means algorithm  uses a search heuristic
#   Begins with a random collection of k clusters
#   Each cluster is represented by a vector of the mean values
#   Measure the distance between an observation and each of the k vectors of mean values    
#   Each observation is then associated with its closest cluster
#   Recalculate  mean values based on the observations that are now associated with each cluster
#   This will provide us with a new collection of k vectors of means
#   Again measure the distance between an observation and each of the k vectors of mean values 
#   Re-associate each observation with its closest cluster [obserbvations will often switch clusters]
#   Iterative process repeats until no more observations move from one cluster to another

  
#   One common distance measure is known as the Minkowski distance
  d(a,b) = q root of (abs(a1-b1)^q) + (abs(a2-b2)^q).....(abs(an-bn)^q)
# The value of q  determines an actual distance formula
  
  # **Note when q=1 this = Manhattan distance  
  d(a,b) = (abs(a1-b1)) + (abs(a2-b2)).....(abs(an-bn))
  
    x <- round(weather$MinTemp[1:2])
    y <- round(weather$MaxTemp[1:2])
    plot(x, y, ylim=c(23, 29), pch=4, lwd=5,
         xlab="MinTemp", ylab="MaxTemp", bty="n")
    round(x)
    [1] 8 14
    > round(y)
    [1] 24 27
    
    # For our weather dataset, we can add a grid() to the plot and limit   our walk to the lines on the grid, as in Figure 9.2.   
    grid()  
  
  # **Note when q=2 this = Euclidean distance  (straight-line distance between the two point)
  d(a,b) = sqrt (abs(a1-b1)^2) + (abs(a2-b2)^2).....(abs(an-bn)^2)
  
  
#To start rattlle()
rattle()
  
# Model Quality => Within cluster sum of squares:
#   sum of the squares of the differences between the observations within each of the ten clusters

# Tuning
#   iteratively build more clusters and measure the quality of each resulting model as a guide to how many
#   clusters to build
#   This is chosen by enabling the Iterate Clusters option.
#   When active, a model with two clusters, then a model with three clusters,
#   and so on up to a model with ten (or as many as specified) clusters will
#   be built. A plot is generated and displayed to report the improvement
#   in the quality measure (the sum of the within cluster sum of squares).
head(weather)  


# Since kmeans requires the analyst to specify the number of clusters to extract. 
# A plot of the within groups sum of squares by number of clusters extracted can 
# help determine the appropriate number of clusters. 
# The analyst looks for a bend in the plot similar to a scree test in factor analysis. 
  
# Determine number of clusters
wss <- (nrow(weather)-1)*sum(apply(weather,2,var))
for (i in 2:23) wss[i] <- sum(kmeans(weather, 
                                     centers=i)$withinss)
plot(2:23, wss, type="b", xlab="Number of Clusters", ylab="Within groups sum of squares")  
str(weather)
  
cols_to_use <- c("MinTemp", "MaxTemp","Rainfall", "Evaporation")

MyKmModel <- kmeans(x=na.omit(weather[, cols_to_use]), centers=10)  
summary(MyKmModel)
# Report on the cluster characteristics. 
# Cluster sizes:

paste(MyKmModel$size, collapse=' ')

# Data means:
colMeans(na.omit(crs$dataset[crs$sample, crs$numeric]))

# Cluster centers:
MyKmModel$centers
  
plot(MyKmModel)  
  
  
  
#   Model Based
#   Model based approaches assume a variety of data models and apply maximum likelihood estimation 
#   and Bayes criteria to identify the most likely model and number of clusters. 
#   Specifically, the Mclust( ) function in the mclust package selects the optimal model according to 
#   BIC for EM initialized by hierarchical clustering for parameterized Gaussian mixture models
#   One chooses the model and number of clusters with the largest BIC. 

  
# Model Based Clustering
library(mclust)

cols_to_use <- c("MinTemp", "MaxTemp","Rainfall", "Evaporation")  
mydata <- weather[,cols_to_use]  
  
fit <- Mclust(mydata)
plot(fit) # plot results 
summary(fit) # display the best model  
  
  
  
#========================================================
#   Plotting Cluster Solutions 
#========================================================
  
# K-Means Clustering with 5 clusters
fit <- kmeans(mydata, 5)
summary(fit)

# Cluster Plot against 1st 2 principal components

# vary parameters for most readable graph
library(cluster) 
clusplot(mydata, fit$cluster, color=TRUE, shade=TRUE, 
         labels=2, lines=0)

# Centroid Plot against 1st 2 discriminant functions
library(fpc)
plotcluster(mydata, fit$cluster)  