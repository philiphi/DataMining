# 06/07/2013
# Torgo book: Data Mining with R
# Chapter2: Predicting Algae Blooms
# package accompanying the book that includes data frames with the datasets ready for u

#The 7 fields a1-a7 are frequencies of various harmful algae
install.packages("DMwR")
library(DMwR)

head(algae)
str(algae)
summary(algae)

# By observing the difference between medians and means, as well as the
# inter-quartile range (3rd quartile minus the 1st quartile),we can get an idea
# of the skewness of the distribution and also its spread

# If we order the values of a variable, the 1st quartile is the value below which there are 25% of the data points

hist(algae$mxPH)
#With probabilities for each interval instead of frequency
hist(algae$mxPH, prob = T)

#Is it Normal?
#obtained using normal Q-Q plots. The function qq.plot(), in the car packagelibrary(car)

#par() function set several parameters of the R graphics system

install.packages("car")
library("car")

par(mfrow=c(1,2))
hist(algae$mxPH, prob=T, xlab='', main='Histogram of maximum pH value',ylim=0:1)

#plots a smooth version of the histogram
lines(density(algae$mxPH,na.rm=T))

#RUG plots the real values of the variable near the X-axis, thus allowing easy spotting of outliers.
rug(jitter(algae$mxPH))

#plots the variable values against the theoretical quantiles of a normal distribution (solid black line).
# & an envelope with the 95% condence interval of the normal distribution (dashed lines)
qq.plot(algae$mxPH,main='Normal QQ plot of maximum pH')
par(mfrow=c(1,1))


#Look at variable oPO4
boxplot(algae$oPO4, ylab = "Orthophosphate (oPO4)")
rug(jitter(algae$oPO4), side = 2)
abline(h = mean(algae$oPO4, na.rm = T), lty = 2)

# 1st and 3rd quartiles = The  boxs vertical limits
# MEDIAN  =horizontal line inside box
# Let r be the inter-quartile range.

# The small horizontal dash above box = largest observation that is less than or equal to the 3rd quartile + 1:5 r 
# The small horizontal dash below  box = smallest observation that is greater than or equal to the 1st quartile minus + 1:5 r 
# The circles below or above these small dashes represent observations that are extremely low (high) compared to all others, and are usually considered
# outliers

# ---------OUTLIERS-----------------

plot(algae$NH4, xlab = "")
abline(h = mean(algae$NH4, na.rm = T), lty = 1)
abline(h = mean(algae$NH4, na.rm = T) + sd(algae$NH4, na.rm = T),lty = 2)
abline(h = median(algae$NH4, na.rm = T), lty = 3)
identify(algae$NH4)


#Note **** To refresh a built in dataset at any time used data() eg
# library(DMwR)
# data(algae)


#UNKNOWN VALUES - options for dealing with
# Remove the cases with unknowns.
# Fill in the unknown values by exploring the correlations between variables.
# Fill in the unknown values by exploring the similarity between cases.
# Use tools that are able to handle these values.


# Remove the cases with unknowns.
#Look at observations with at least one unknown value =>  function complete.cases() 
algae[!complete.cases(algae),]

nrow(algae[!complete.cases(algae),])

#Remove all
algae <- na.omit(algae)

#Remove two records
algae <- algae[-c(62,99),]

# The number of unknown values in each row of the algae dataset:


# apply() belongs to a set of very powerful functions of R
# These functions are sometimes known as meta-functions and allow applying
# other functions to objects under certain conditions

# The function we have provided is in this case a temporary function. 
# It is temporary because it only exists within the call of the apply()
# here the temporary function calculates the number of NAs on its argument x

#***Note: true value in R is equivalent to the number 1,
# and the false to the value 0, which means that when you sum a vector of
# Boolean values, you obtain how many trues exist in the vector

apply(algae, 1, function(x) sum(is.na(x)))

#Function in the book package returns row# where count of Na > x = 0.2 default
manyNAs(algae, 0.3)

#Remove them
algae <- algae[-manyNAs(algae), ]


# Filling in the Unknowns with the Most Frequent Values
# to use some statistic of centrality
#mean good choice if roughtly normal dist
#median  good choice if skewed dist

#algae[48,] does not have a value in the variable mxPH. As the distribution of this variable is nearly normal
algae[48,]

#check for normality => Yes so mean good
hist(algae$mxPH)

#Replace msssing value with mean
algae[48, "mxPH"] <- mean(algae$mxPH, na.rm = T)

#variable Chla is unknown on 12 records
#check for normality => NO - skewed
hist(algae$Chla)
# use the median to fll in all the unknowns in this column,
algae[is.na(algae$Chla), "Chla"] <- median(algae$Chla, na.rm = T)

# function centralImputation()  uses the median for numeric columns & mode for nominal variables. You may use it as follows:
data(algae)
algae <- algae[-manyNAs(algae), ]
algae <- centralImputation(algae)



# Filling in the Unknown Values by Exploring Correlations

# obtain the variables correlation
#The use="complete.obs" setting tells R to disregard observations with NA values in this calculation 
cor(algae[, 4:18], use = "complete.obs")

# cor() function is not very legible but we can put it  through the function symnum()
symnum(cor(algae[,4:18],use="complete.obs"))

#PO4 and oPO4 are strongly correlated (above 0.9)
#we need to find the form of the linear correlation between these variables
data(algae)
algae <- algae[-manyNAs(algae), ]
lm(PO4 ~ oPO4, data = algae)

# There's a single observation with an unknown value on the variable PO4 (sample 28)
# thus we could simply use the discovered relation to do the following:
algae[28, "PO4"]
algae[28, "PO4"] <- 42.897 + 1.293 * algae[28, "oPO4"]

#Create a function to do from > 1 record
data(algae)
algae <- algae[-manyNAs(algae), ]
fillPO4 <- function(oP) {
  if (is.na(oP))
    return(NA)
  else return(42.897 + 1.293 * oP)
  }


#Call function
#Reset algae[28, "PO4"] <- NA
algae[is.na(algae$PO4), "PO4"] <- sapply(algae[is.na(algae$PO4),"oPO4"], fillPO4)

#Note histogram is from {lattice}
#**Nice 
# histogram of the values of mxPH for the different values of season
histogram(~mxPH | season, data = algae)

# By default, when we factor a set of nominal variable values, the levels
# parameter assumes the alphabetical ordering of these values. 
# In this case we want a different ordering (the temporal order of the seasons), so we need to
# specify it to the factor function. Try executing this instruction and afterward
# obtain again the histogram to see the difference.
algae$season <- factor(algae$season, levels = c("spring","summer", "autumn", "winter"))
histogram(~mxPH | season, data = algae)




#Filling in the Unknown Values by Exploring Similarities between Cases 
#try to use the similarities between the rows (observations) to fill in the unknown values 
# assumes that if two water samples are similar, and one of them has an unknown value in some variable, there is a
# high probability that this value is similar to the value of the other sample. 
# 
# we need to define the notion ofsimilarity. 
# usually defined using a metric over the multivariate space of the variables used to describe the observations.
# common choice is the Euclidean distance
# = square root of the sum of the squared diferences between the values of any two case

# We will consider two ways of using their values. 
  # 1. Calculate the median(mode if nominal variables) of the values of the ten nearest neighbors to fll in the gaps. 
  # 2. Use a weighted average of the values of the neighbors
      #use a Gaussian kernel function to obtain the weights from the distances

# function knnImputation() available in the book package.
#reset 
data(algae)

#For method1
algae <- knnImputation(algae, k = 10, meth = "median")
#For method2
algae <- knnImputation(algae, k = 10)


# 2.6 Obtaining Prediction Model

# The implementation of linear regression available in R is not able to use
# datasets with unknown values, the implementation of regression trees handles
# these values naturally.


#2.6.1 Multiple Linear Regression 






















# .....stopped page 63








