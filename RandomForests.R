# 07/03/2013 
# From Data Mining with Rattle & R 
# Chapter 12 Random Forests

#*** Note: Book uses env() to create containers for everything => See Page 50
# The container is an R environment and is initialised using new.env() (new environment)
# Data is placed into the container using the $ notation
#     > en$obs <- 4:8
#     > en$obs
#     [1] 4 5 6 7 8

# evalq() alows us to operate on variables within an environment without $ notation

# > evalq(
#   {
#     nobs <- length(obs)
#     nvars <- length(vars)
#   }, en)
# 
# > en$nobs
# [1] 5
# > en$nvars
# [1] 3


installed.packages()
# install.packages("rattle")
library(rattle)
library(randomForest)
rattle()


data_weather <- na.omit(weather)
nobs <- nrow(data_weather)
form <- formula(RainTomorrow ~ .)

#first element in the formula  => [2] would return . as ~ is recognized as a formula operator
target <- all.vars(form)[1]

#The index of these 3 variables witha minus sign in front of each
vars <- -grep('^(Date|Location|RISK_)', names(data_weather))

#Takes 70% of the the number nobs
set.seed(42)
train <- sample(nobs, 0.7*nobs)

#Building a model with all the part sdefined as variables!
MyRFmodel <- randomForest(formula=form,
                      #train =  random sample , vars = all predictors
                      data=data_weather[train, vars],
                      ntree=500, mtry=4,
                      importance=TRUE,
                      localImp=TRUE,
                      na.action=na.roughfix,
                      replace=FALSE)

str(MyRFmodel)

#The predicted component contains the values predicted for each
# observation in the training dataset based on the out-of-bag samples
head(MyRFmodel$predicted, 10)

# The importance component records the information related to measures of variable importance
head(MyRFmodel$importance)

#The importance of each variable predicting outcome for each observation in the training dataset
head(MyRFmodel$localImp)[,1:4]

# The error rate data is stored as the err.rate component
MyRFmodel$localImp[,1:4]

# Here we see that the OOB estimate decreases quickly and then starts to atten out
round(head(MyRFmodel$err.rate, 15), 4)
    # We can fnd the minimum quite simply, together with a list of the indexes where each minimum occurs
      min.err <- min(data.frame(MyRFmodel$err.rate)["OOB"])
      min.err.idx <- which(data.frame(MyRFmodel$err.rate)["OOB"]
                     == min.err)
    # We can then list the actual models where the minimum occurs
      MyRFmodel$err.rate[min.err.idx,]

      #*****NB****
      # We might thus decide that 25 (the first instance of the minimum OOB estimate) 
      # is a good number of trees to have in the forest.
      min.err
      min.err.idx

# "VOTES" Another interesting component is votes, which records the number
# of trees that vote No and Yes within the ensemble for a particular observation
    head(MyRFmodel$votes)
    # numbers are reported as proportions so sum to 1 for each onservation - check by summing the Yes|No  columns
    head(apply(MyRFmodel$votes, 1, sum))


# TUNING parametets for Rattle
Number of Trees ntree=
Number of Variables ntry=      default is srqt(n)
Sample Size sampsize=
    if training set  5,000 observations =Y and 500=N 
    we can specify the sample size as 400,400 to ensure balanced sample

Variable Importance importance=
  
  
=============================================================================  
  
# Appy to wine dataset

=============================================================================  
  
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
# cols_to_use <- names(rawContacts[,c(4,7)])   
# myData_sub <- rawContacts[,cols_to_use]   
# head(myData_sub)
  


#Exclude any variables here target
# vars <- -grep('^(free.sulfur.dioxide)', names(data_wine))
vars <- 1:12

#Take 70:30 split
set.seed(42)
data_all <- seq(1:nrow(rawContacts))
data_train <- sample(nrow(rawContacts), 0.7*nrow(rawContacts))
data_test <- data_all[-data_train]

myDTform <- formula(SiteArr ~ OppCount + DaysSinceLogin )

RFwine <- randomForest(formula=myDTform,
#train =  random sample , vars = all predictors
       data=rawContacts[data_train,],
ntree=500, mtry=4,
importance=TRUE,
localImp=TRUE,
na.action=na.roughfix,
replace=FALSE)

#Results
str(RFwine)
head(RFwine$predicted, 10)

# The importance component records the information related to measures of variable importance
head(RFwine$importance)

#The importance of each variable predicting outcome for each observation in the training dataset
head(RFwine$localImp)[,1:4]

# The error rate data is stored as the err.rate component
RFwine$localImp[,1:4]

# Here we see that the OOB estimate decreases quickly and then starts to atten out
round(head(RFwine$err.rate, 15), 4)

# We can fnd the minimum quite simply, together with a list of the indexes where each minimum occurs
min.err <- min(data.frame(RFwine$err.rate)["OOB"])
min.err.idx <- which(data.frame(RFwine$err.rate)["OOB"]



=============================================================================     

# Appy to [ContactsActiveIMS]

=============================================================================  

set.seed(42)
head(rawContacts)

library(randomForest)

#Take 70:30 split
set.seed(42)
data_all <- seq(1:nrow(rawContacts))
data_train <- sample(nrow(rawContacts), 0.7*nrow(rawContacts))
data_test <- data_all[-data_train]

myDTform <- formula(SiteArr ~ OppCount + DaysSinceLogin )

myModRF1 <- randomForest(formula=myDTform,
                  #train =  random sample , vars = all predictors
                  data=rawContacts[data_train,],
                  ntree=500, mtry=4,
                  importance=TRUE,
                  localImp=TRUE,
                  na.action=na.roughfix,
                  replace=FALSE)

#Results
str(myModRF1)
head(myModRF1$predicted, 10)

# The importance component records the information related to measures of variable importance
head(myModRF1$importance)

#The importance of each variable predicting outcome for each observation in the training dataset
head(myModRF1$localImp)[,1:4]

# The error rate data is stored as the err.rate component
RFwine$localImp[,1:4]


