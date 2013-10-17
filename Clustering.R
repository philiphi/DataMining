# 07/10/2013 
# From Data Mining with Rattle & R 
# Chapter 9 Clustering

# kmeans()is provided directly through R by the standard stats package.

# **NB
# All fields MUST be numeric
# Make sure fields are NOT stored as factors instead of numeric. 
# Use as.numeric(new)  where necessary

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
wss <- (nrow(weather)-1)*sum(apply(weather,2,vars))
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


#============================================================================
# Create clusters with subset of fields and append back to original dataset
#============================================================================
cols_to_use <- c("rowid","MinTemp", "MaxTemp","Rainfall", "Evaporation")  
weather2 <- weather

#need a unique rowid
weather2 <- cbind(weather2,seq(1:nrow(weather2)))
names(weather2)  <-  c("Date" ,"Location" , "MinTemp" , "MaxTemp" , "Rainfall" ,"Evaporation" , "Sunshine", "WindGustDir" , "WindGustSpeed" , "WindDir9am" , "WindDir3pm" , "WindSpeed9am" , "WindSpeed3pm" , "Humidity9am" ,"Humidity3pm" , "Pressure9am", "Pressure3pm" , "Cloud9am" , "Cloud3pm" , "Temp9am" , "Temp3pm" , "RainToday" , "RISK_MM" , "RainTomorrow" , "rowid")
# head(weather2)  

#Fields to use for clustering  
mydata <- weather2[,cols_to_use] 
# head(mydata)  

fit <- kmeans(mydata, 5)
aggregate(mydata, by = list(fit$cluster), FUN = mean)
cleanData <- data.frame(mydata, fit$cluster)

cleanData <- subset(cleanData, select = c(rowid, fit.cluster))
cleanData <- merge(x = weather2, y = cleanData, by = "rowid", all.x = TRUE)
cleanData <- subset(cleanData, select = -c(rowid))
# head(cleanData)   
return(cleanData)




#============================================================================
# Sites
#============================================================================  
library("RODBC")
Conn <- odbcConnect(dsn="Extgensql01",uid="Bentley\\Phil.Hickey",pwd="Waterford?")
Query <- "Select top 10000 SiteID,
IsNull(Cast(SalesChannelID as Int),0) as SalesChannelID,
IsNull(RevenueClassificationID,0) as RevenueClassificationID,
IsNull(Cast(LearnerStatusCode as Int),0) as LearnerStatusCode	,
IsNull(ArrValue,0) as ArrValue
from [dwh-db].DWH.PROD. Sites"

#Alternatively you can read teh sql froma  file
# Query <- readChar("data/MyQuery.sql",nchars=99999)"
myData <- sqlQuery(Conn,Query,errors=TRUE)
# head(myData)
# str(myData)
# dim(myData)  

#Remove NA
myData <- na.omit(myData)  


#Subset of fields to use for clustering    
cols_to_use <- names(myData[,c(1:5)])  
# cols_to_use <- names(myData[,c(1,8:13)])  
myData_sub <- myData[,cols_to_use]   
# head(myData_sub)

fit_Sites <- kmeans(myData_sub, 5)

#Calculate Cluster Means for each field  
aggregate(myData_sub, by = list(fit_Sites$cluster), FUN = mean)
cleanData <- data.frame(myData_sub, fit_Sites$cluster)
# head(cleanData,10)  

cleanData <- subset(cleanData, select = c(SiteID, fit_Sites.cluster))
cleanData <- merge(x = myData, y = cleanData, by = "SiteID", all.x = TRUE)
head(cleanData)   



#============================================================================
# Contacts active in IMS 
#============================================================================    
library("RODBC")
Conn <- odbcConnect(dsn="ExtprddmDM",uid="Bentley\\Phil.Hickey",pwd="Waterford?")
Query <- "Select ContactID,OppCount,LeadCount,SRCount,Age,DaysSinceLogin,SiteArr from [ContactsActiveIMS]"

rawContacts <- sqlQuery(Conn,Query,errors=TRUE)
# head(rawContacts)
# str(rawContacts)
# dim(rawContacts)  

#Remove NA
rawContacts <- na.omit(rawContacts)  

#Subset of fields to use for clustering    
cols_to_use <- names(rawContacts[,c(4,7)])   
myData_sub <- rawContacts[,cols_to_use]   
head(myData_sub)

fit_ConIMS <- kmeans(rawContacts, 3)

#Calculate Cluster Means for each field  
aggregate(myData_sub[], by = list(fit_ConIMS$cluster), FUN = mean)
cleanContacts <- data.frame(myData_sub, fit_ConIMS$cluster)
# head(cleanContacts,10)  

# cleanContacts <- subset(cleanContacts, select = c(SiteID, fit_Sites.cluster))
# cleanContacts <- merge(x = myData, y = cleanContacts, by = "SiteID", all.x = TRUE)
# head(cleanContacts)   




# --------------------------------------------------
# Plots
# --------------------------------------------------
head(cleanContacts)

summary(fit_ConIMS)

# Cluster sizes:
paste(fit_ConIMS$size, collapse=' ')

# Data means:
colMeans(na.omit(cleanContacts[,2:7]))

# Cluster centers:
fit_ConIMS$centers

#Who is the average contact
summary(cleanContacts[])

# Within cluster sum of squares:
fit_ConIMS$withinss

table(cleanContacts$OppCount)
table(cleanContacts$LeadCount)
table(cleanContacts$SRCount)
table(cleanContacts$Age)
table(cleanContacts$DaysSinceLogin)
table(cleanContacts$SiteArr)

table(cleanContacts$fit_ConIMS.cluster)


# Plot each field and colour by its cluster
plot(cleanContacts[,'OppCount'],col=cleanContacts$fit_ConIMS.cluster,main="",xlab="Obs#",ylab="OppCount" )

#Restrict where OppCount <30
plot(cleanContacts[cleanContacts$OppCount < 30,'OppCount'],col=cleanContacts$fit_ConIMS.cluster,main="",xlab="Obs#",ylab="OppCount" )
plot(cleanContacts[cleanContacts$LeadCount < 30,'LeadCount'],col=cleanContacts$fit_ConIMS.cluster,main="",xlab="Obs#",ylab="LeadCount" )
plot(cleanContacts[cleanContacts$SRCount < 30,'SRCount'],col=cleanContacts$fit_ConIMS.cluster,main="",xlab="Obs#",ylab="SRCount" )

#Restrict plot to n=1000
sample1 <- sample(cleanContacts[cleanContacts$SRCount < 30,'SRCount'],1000)
plot(sample1,col=cleanContacts$fit_ConIMS.cluster,main="",xlab="Obs#",ylab="SRCount" )



# Cluster Plot against 1st 2  components [vary parameters for most readable graph]
library(cluster) 
clusplot(cleanContacts, cleanContacts$fit_ConIMS.cluster, color=TRUE, shade=TRUE, labels=2, lines=0)

# Centroid Plot against 1st 2 discriminant functions
library(fpc)
plotcluster(cleanContacts, cleanContacts$fit_ConIMS.cluster)  


# --------------
#EDA
# --------------
head(rawContacts)

lm1 <- lm(OppCount ~ SRCount + LeadCount,data=rawContacts)
summary(lm1)
anova(lm1)

cor(rawContacts[, 2:7], use = "complete.obs")
plot(rawContacts$OppCount)
hist(rawContacts$OppCount,breaks=100)
table(rawContacts$OppCount)

# --Records where Oppcount>0
hist(rawContacts[rawContacts$OppCount >0,2],breaks = 20)


# ----------------------------------------------------------------------
### Nice plot with some sample data
# ----------------------------------------------------------------------
install.packages("vegan")
library(vegan)
data(dune)

# kmeans
kclus <- kmeans(dune,centers= 4, iter.max=1000, nstart=10000)

# distance matrix
dune_dist <- dist(dune)

# Multidimensional scaling
cmd <- cmdscale(dune_dist)


# plot MDS, with colors by groups from kmeans
groups <- levels(factor(kclus$cluster))
ordiplot(cmd, type = "n")
cols <- c("steelblue", "darkred", "darkgreen", "pink")
for(i in seq_along(groups)){
  points(cmd[factor(kclus$cluster) == groups[i], ], col = cols[i], pch = 16)
}

# add spider and hull
ordispider(cmd, factor(kclus$cluster), label = TRUE)
ordihull(cmd, factor(kclus$cluster), lty = "dotted")
