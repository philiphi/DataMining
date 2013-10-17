# Torgo book: Data Mining with R
# Chapter4: Detecting Fraudulent Transactions
# book Web site it is available as an Rdata file

load("Data Mining with R/sales.Rdata")

library(DMwR)
head(sales)

summary(sales)

# number of uniques
nlevels(sales$ID)
nlevels(sales$Prod)

#How many records have NA for Quant & Val 
length(which(is.na(sales$Quant)& is.na(sales$Val) ))
  # take advantage of the way logical values are coded in R (T=1 and F=0)to obtain the same number more eciently:
  sum(is.na(sales$Quant) & is.na(sales$Val))

#% records where Insp=fraud
table(sales$Insp)/nrow(sales) * 100


totS <- table(sales$ID)
totP <- table(sales$Prod)
barplot(totS, main = "Transactions per salespeople", names.arg = "", xlab = "Salespeople", ylab = "Amount")
barplot(totP, main = "Transactions per product", names.arg = "", xlab = "Products", ylab = "Amount")

#given the different quantity of products that are sold on each transaction 
#it makes more sense carry out this analysis over the unit price instead


# Add new column for unit price
sales$Uprice <- sales$Val/sales$Quant
head(sales)
  # check the distribution of the unit price => observe a rather marked variability
  summary(sales$Uprice)

  totUP <- table(sales$Uprice)
  barplot(totUP, main = "Unit Price", names.arg = "", xlab = "Salespeople", ylab = "Amount")
  
# Use the median unit price to represent the typical priceat which a product is sold

attach(sales)
#median unit price of each product 
upp <- aggregate(Uprice,list(Prod),median,na.rm=T)
sales$Uprice <- sales$Val/sales$Quant

#******Really cool*******
# The five most expensive & five cheapest products side-by-side
# We vary the parameter "decreasing" of the function order() between True & False
# Use the sapply() function to apply a vector of TRUE,FALSE to our generic "function"
topP <- sapply(c(T,F),function(o) upp[order(upp[,2],decreasing=o)[1:5],1])
colnames(topP) <- c('Expensive','Cheap')
topP

# View completely different price distribution of the top products using a box plot of their unit prices:
# The %in% operator tests if a value belongs to a set

tops <- sales[Prod %in% topP[1, ], c("Prod", "Uprice")]
tops$Prod <- factor(tops$Prod)
boxplot(Uprice ~ Prod, data = tops, ylab = "Uprice", log = "y")

#******Really cool*******
# which salespeople are the ones who bring more (less) money to the company
vs <- aggregate(Val,list(ID),sum,na.rm=T)
scoresSs <- sapply(c(T,F),function(o) vs[order(vs$x,decreasing=o)[1:5],1])
colnames(scoresSs) <- c('Most','Least')
scoresSs

# The Top 100 Sales people account for 38% of total sales value
sum(vs[order(vs$x, decreasing = T)[1:100], 2])/sum(Val, na.rm = T) * 100

# The Bottom 1000 Sales people account for < 1% of total sales value
sum(vs[order(vs$x, decreasing = F)[1:1000], 2])/sum(Val, na.rm = T) * 100



#===========================================================================
#OUTLIER IDENTIFICATION 
#===========================================================================

#Boxplot rule:  Interquartile Range IQR = Q3-Q1
#Observation is an outlier if it
# falls above Q3 + 1.5*IQR
# or falls below Q1 - 1.5*IQR


#TEst for Outliers: Use boxplot.stats()
# the out component of this list contains the observations considered outliers
MyOutliers <- tapply(Uprice,list(Prod=Prod),function(x) length(boxplot.stats(x)$out))
sum(MyOutliers)

MyOutliers[order(MyOutliers, decreasing = T)[1:10]]




# ==================================================================

# ==================================================================












