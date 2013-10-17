# 07/14/2013 
# From Data Mining with Rattle & R 
# Chapter 11 Decision Trees

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

# DSN ExtprddmDM
installed.packages()
# install.packages("rattle")
library(rattle)
rattle()

set.seed(42)
head(rawContacts)

#Take 70:30 split
set.seed(42)
data_all <- seq(1:nrow(rawContacts))
data_train <- sample(nrow(rawContacts), 0.7*nrow(rawContacts))
data_test <- data_all[-data_train]

myDTform <- formula(SiteArr ~ OppCount + DaysSinceLogin )

myDTmod <- rpart(formula=myDTform, data=rawContacts[data_train,])
summary(myDTmod)










