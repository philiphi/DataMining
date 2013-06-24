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
data(algae)
algae <- algae[-manyNAs(algae), ]
clean.algae <- knnImputation(algae, k = 10)

lm.a1 <- lm(a1 ~ ., data = clean.algae[, 1:12])
summary(lm.a1)

#***how R handled the three nominal variables
# R will create a set of auxiliary variables.
# For each factor variable with k levels, R will create k-1 auxiliary variables
# These variables have the values 0 or 1. 
# 1 means that the associated value of the factor is "present"
# and the other auxiliary variables will have the value 0
# If ALL k-1 variables are 0, then it means that the factor variable has the remaining kth  value
#NOte we see seasonspring,seasonsummer,seasonsummer (4-1) for the k=4 seasons 

# -------------------------
#Output of summary()
# -------------------------  
# Residuals: = the errors of the fit
# These residuals should have a mean zero 
# and should have a normal distribution 
# (and obviously be as small as possible!)

# Coefficients:
#For each coecient (variable) of the multiple regression equation, 
# R will show its value and also its standard error (an estimate of the variability of these coecients) 

# we can test the hypothesis that each of them is null, that is, H0 :  Bi = 0
# t-test usually used 
# R calculates a t value, which is defined as the ratio between the coefficient value and its standard error
# tvalue = Estimare/Std Error

# column (Pr(>|t|)) = level at which the hypothesis that the coefficient is null is rejected.
# Thus a value of 0.0001 has the meaning that we are 99.99% confident that the coeff is NOT null
# 
#R^2 coefficients (multiple and adjusted). 
# These indicate the degree of FIT of the model to the data, that is, 
# the proportion of variance in the data that is explained by the model
# Values near 1 are better (almost 100% explained variance) while the smaller the values, the larger the lack of t

# Finally, we can also test the null hypothesis that there is no dependence
# of the target variable on any of the explanatory variables
#The F -statistic can be used for this purpose by comparing it to a critical value.
# Thus a p-level of 0.0001 means that we are 99.99% confiddent that the null hypothesis is not true
#Null The means H0 :B1 =  B2 = ... = Bm = 0  ie target var doesn't depend on the predictors

#***Usually, if the model fails this test (e.g., with a p value that is considered too high
# for example, higher than  0.1), it makes no sense to look at the t-tests on the individual coefficients.

# plot(lm.a1) gives a series of successive plots that help in understanding the performance of the model. 
plot(lm.a1)

#Plot1: Fitted target variable value against the respective residual (error) of the mode
# Larger errors are usually marked by adding the corresponding row number to the dot in the graph

#Plot2: normal Q-Q plot of teh errors (Ideally should be approx normally distributed)


# The proportion of variance explained by this model is not very impressive
# (around 32.0%). Still, we can reject the hypothesis that the target variable
# does not depend on the predictors (the p value of the F test is very small)


# Looking at the signiccance of some of the coefficients, we may question the
# inclusion of some of them in the model. There are several methods for simplifying regression models. In this section we will explore a method usually
# known as backward elimination

#anova()
# We will start our study of simplifying the linear model using the anova()
# function. When applied to a single linear model, this function will give us a
# sequential analysis of variance of the MODEL FIT
# That is, the reductions in the residual sum of squares (the total error of the model) 
# as each term of the formula is added in turn

anova(lm.a1)

# These results indicate that the variable season is the variable that least
# contributes to the reduction of the fitting error of the model. 
# Let us remove it from the model:

#The update() function can be used to perform small changes to an existing model 
lm2.a1 <- update(lm.a1, . ~ . - season)

summary(lm2.a1)

#***Nice
# We can carry out a more formal comparison between the two models by using
# again the anova() function, but this time with both models as arguments:

anova(lm.a1,lm2.a1)
# performs an analysis of variance of the two models using an F -test to assess the signicance of the dierences
# The sum of the squared errors has decreased -448
# ...but the differences are not signifcant (a value of 0.6971 tells us that 
# with only around 30% confidence we can say they are different

# In order to check if we can remove more coefficients, we would again use the anova() function
# This process would continue until we have no candidate coecients for removal.
# R has a function step() that performs all process for us

final.lm <- step(lm.a1)
# The function step() uses the Akaike Information Criterion to perform model search
# uses backward elimination by default

summary(final.lm)
#The proportion of variance explained(Adj R^2) by this model is still not very interesting.
#This kind of proportion is usually considered a sign that the linearity
#assumptions of this model are inadequate for the domain




# ------------------------------------------------------------
# 2.6.2 Regression Trees
# ------------------------------------------------------------
# these models handle datasets with missing values
#A regression tree is a hierarchy of logical tests on some of the explanatory variables. 

library(rpart)
data(algae)
algae <- algae[-manyNAs(algae), ]
rt.a1 <- rpart(a1 ~ ., data = algae[, 1:12])
rt.a1

# A tree is read from the root node that is marked by R with the number 1.

#The output R provides:
# 198 samples 
# Average value for the frequency of algal "a1" = 16.99
# deviance(The sum of squared dierences from the average) from this average is 90401.29

# Each node of a tree has two branches
# These are related to the outcome of a test on one of the predictor variables
# Leaf (terminal)nodes are marked with asterisks *
# At these leaves we have the predictions of the tree
# The average target variable value found at the leaf we have reached is the prediction of the tree

#obtain a graphical representation of the tree 
# by successively applying the functions plot() and text() to the tree.
#book package the function prettyTree() 

prettyTree(rt.a1)

# summary() function can also be applied to tree objects. This will
# produce a lot of information concerning the tests on the tree

# Trees are usually obtained in two steps. 
# Initially, a large tree is grown
# then this tree is pruned by deleting bottom nodes through a process of statistical estimation. 
# This process has the goal of avoiding overfitting

# an overly large tree will fit the training data almost perfectly, 
# but will be capturing spurious relationships of the given dataset

# The function rpart() that we have used to obtain our tree only grows the
# tree, stopping when certain criteria are met. 
# Namely, the tree stops growing whenever
#   (1) the decrease in the deviance goes below a certain threshold
#   (2) the number of samples in the node is less than another threshold
#   (3) the tree depth exceeds another valuethresholds are controlled
# these parameters cp, minsplit, and maxdepth
# and have default values are 0.01, 20, and 30

#Pruning
# rpart package implements a pruning method called cost complexity pruning 
# use the cp parameter at eaach node
# and tries to estimate the value of cp that ensures the best compromise between predictive accuracy and tree size.

# Given a tree obtained with the rpart() function, R
# can produce a set of sub-trees of this tree and estimate their predictive performance. 
# This information can be obtained using the function printcp():
printcp(rt.a1)

#**The tree produced by the rpart() function is the last tree of this list # (tree 9). 
# This tree has a cp value of 0.01
# It includes 9 tests and has a relative error (compared to the root node) of 0.354

# R estimates, using an internal process of ten-fold crossvalidation, 
# that this tree will have an average relative error of 0.69531 +- 0.11917

# we can observe that we would theoretically be better off with the tree number 8, 
# which has a lower estimated relative error 

# An Alternative selection rule:
# choose the best tree according to the 1-SE rule
# Look at "xerror" column 
# the smallest tree with error less than 0.69531 +  0.11917 =. Tree#2

# If we prefer this tree to the one # suggested by R, we can obtain it using the 
# respective cp value:
rt2.a1 <- prune(rt.a1, cp = 0.08)
rt2.a1

# R also allows a kind of interactive pruning of a tree through the function
# snip.rpart(). 
#generate a pruned tree in two ways
#1.indicating the number of the nodesvat which you want to prune the tree

first.tree <- rpart(a1 ~ ., data = algae[, 1:12])
snip.rpart(first.tree, c(4, 7))

#2.use snip.rpart() in a graphical way
# First, youplot the tree, and then you call the function without the second argument. If
# you click the mouse at some node, R prints on its console



# 2.7 Model Evaluation and Selection

# obtain the model predictions using predict()
lm.predictions.a1 <- predict(final.lm, data=clean.algae)
rt.predictions.a1 <- predict(rt.a1, data=algae)

#calculate mean absolute error MAE
mae.a1.lm <- mean(abs(lm.predictions.a1 - algae[, "a1"]))
mae.a1.rt <- mean(abs(rt.predictions.a1 - algae[, "a1"]))

#mean squared error (MSE)
mse.a1.lm <- mean((lm.predictions.a1 - algae[, "a1"])^2)
mse.a1.rt <- mean((rt.predictions.a1 - algae[, "a1"])^2)

# Even if we use the MAE statistic, we can ask ourselves
# the question whether the scores obtained by the models are good or bad.

# An alternative statistic that provides a reasonable answer to this question
# is the normalized mean squared error (NMSE)

# NMSE calculates a ratio between the performance of our models and that of a baseline predictor
# ,usually the mean value of the target variable

nmse.a1.lm <- mean((lm.predictions.a1-algae[,'a1'])^2)/
     mean((mean(algae[,'a1'])-algae[,'a1'])^2)

nmse.a1.rt <- mean((rt.predictions.a1-algae[,'a1'])^2)/
     mean((mean(algae[,'a1'])-algae[,'a1'])^2)

# The NMSE is a unit-less error measure with values usually ranging from 0 to 1
# If model performing better than baseline NMSE should be clearly less than 1
# The smaller the NMSE, the better

#Nice ***** book function regr.eval()
regr.eval(algae[, "a1"], rt.predictions.a1, train.y = algae[,"a1"])


# visual inspection of the predictions of the models
# scatter plot of the errors

plot(lm.predictions.a1, algae[, "a1"], main = "Linear Model",xlab = "Predictions", ylab = "True Values")
abline(0, 1, lty = 2)

plot(rt.predictions.a1, algae[, "a1"], main = "Regression Tree",xlab = "Predictions", ylab = "True Values")
abline(0, 1, lty = 2)

# We can check which is the sample number where a particularly bad prediction is made
# with the function identify()
algae[identify(lm.predictions.a1,algae[,'a1']),]


# The key issue here is to obtain a reliable estimate of a model performance
# on data for which we do not know the true target value
# Calculating the performance metrics using the training data leads to OVERFITTING

# Alternative => k-fold cross-validation 
# fequently used method for obtaining reliable estimates for small datasets

# The k-fold cross-validation  method:
# Obtain k equally sized and random subsets of the training data. 
# For each k subset, build a modelusing the remaining k-1 sets
# Evaluate this model on the k subset & record performance 
# 
# REPEAT this process for ALL remaining subsets.
# In the end, we have k performance measures, all obtained by testing a model
# on data not used for its construction, and that is the key issue. 
# The k -fold cross-validation estimate is the average of these k measures. 
# A common choicefor k is 10. 
# Sometimes we even repeat the overall k-fold CV process several times to get even more reliable estimates.


# In general, we can say that when facing a predictive task, we have to make
# the following decisions:
#   • Select the alternative models to consider (the models can actually be alternative settings of the same algorithm) for the predictive task(s) we want to address.
#   • Select the evaluation metrics that will be used to compare the models.
#   • Choose the experimental methodology for obtaining reliable estimates of these metrics.


#NICE*** BOOK experimentalComparison() function is generic in the sense that it
# can be used for any model(s) and any dataset(s). 

# experimentalComparison() TAKE 3 PARAMETERS
# 1.the data sets to use for the comparison,
# 2.the alternative models
# 3.the experimental process parameters

# note the function is generic so,can be used for any model(s) and any dataset(s).


#example & illustration of using experimentalComparison()

# User supplies a set of functions implementing the models to be compared
# Each of these functions should implement a full train+test+evaluate cycle for the given training and test datasets
# The functions will be called from the experimental routines on each iteration of the estimation process
# The funtions should return a vector with the values of whatever evaluation metrics the user wants to estimate by
# cross-validation

# Let us construct such functions for our two target models:
cv.rpart <- function(form,train,test,...) {
    m <- rpartXse(form,train,...)
    p <- predict(m,test)
    mse <- mean((p-resp(form,test))^2)
    c(nmse=mse/mean((mean(resp(form,train))-resp(form,test))^2))
    }
cv.lm <- function(form,train,test,...) {
  m <- lm(form,train,...)
  p <- predict(m,test)
  p <- ifelse(p < 0,0,p)
  mse <- mean((p-resp(form,test))^2)
  c(nmse=mse/mean((mean(resp(form,train))-resp(form,test))^2))
  }

#**Notes - The functions include a special parameter ...
# allows specification of variable number of parameters
# captures all arguments passed in the function call after the first three that are specified by name
# the resp() function(book package) is used to obtain the target variable values of a data set given a formula.

# SO.....
# Having defined the functions that will carry out the learning and testing
# phase of our models, we can carry out the cross-validation comparison as follows:

#note ** The variants() function generates a set of alternative models resulting from all possible combinations of
#the parameters values

res <- experimentalComparison(
c(dataset(a1 ~ .,clean.algae[,1:12],'a1')),
c(variants('cv.lm'),
variants('cv.rpart',se=c(0,0.5,1))),
cvSettings(3,10,1234))

#Results: we want lowest NMSE
summary(res)
plot(res)
# As it can be seen, one of the variants of the regression tree achieves the best average NMSE score.

#The experimentalComparison() function assigns a label to each model variant 
#To find the specific parameter settings corresponding to any label
getVariant("cv.rpart.v1", res)


#RANDOMFOREST
# Formed by a large set of tree-based models (regression or classification trees)
# Predictions for regression tasks are obtained by averaging the predictions of the trees in the ensemble

#Create new function using RandomForst()
#install.packages("randomForest")
library(randomForest)
cv.rf <- function(form,train,test,...) {
  m <- randomForest(form,train,...)
  p <- predict(m,test)
  mse <- mean((p-resp(form,test))^2)
  c(nmse=mse/mean((mean(resp(form,train))-resp(form,test))^2))
  }

#Use experimentalComparison() to call
res.all <- experimentalComparison(
  c(dataset(a1 ~ .,clean.algae[,1:12],'a1')),
  c(variants('cv.lm'),
      variants('cv.rpart',se=c(0,0.5,1)),
      variants('cv.rf',ntree=c(200,500,700))
      ),
  cvSettings(5,10,1234))

#Note 3 of the  RandomForest modesl perform joint best
summary(res.all)
plot(res.all)








# .....stopped page 75








