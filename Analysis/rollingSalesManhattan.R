# Author: Benjamin Reddy
# Taken from pages 49-50 of O'Neil and Schutt

#require(gdata)
#require(plyr) #Added by Monnie McGee
#install the gdata and plyr packages and load in to R.
library(plyr)
library(gdata)
setwd("/Users/Shravan/R/projects/DS_6306_Unit6_Pract2/")



# So, save the file as a csv and use read.csv instead
#bk <- read.csv("Analysis/Data/rollingsales_manhattan.xls",skip=4,header=TRUE)
bk <- read.xls("Analysis/Data/rollingsales_manhattan.xls",skip=4,header=TRUE)

## Check the data
head(bk)
summary(bk)
str(bk) # Very handy function!
head(bk$SALE.PRICE, 50)


## clean/format the data with regular expressions
## More on these later. For now, know that the
## pattern "[^[:digit:]]" refers to members of the variable name that
## start with digits. We use the gsub command to replace them with a blank space.
# We create a new variable that is a "clean' version of sale.price.
# And sale.price.n is numeric, not a factor.
bk$SALE.PRICE.N <- as.numeric(gsub("[^[:digit:]]","", bk$SALE.PRICE))
head(bk$SALE.PRICE.N, 50)
count(is.na(bk$SALE.PRICE.N))

names(bk) <- tolower(names(bk)) # make all variable names lower case
names(bk)
## Get rid of leading digits
bk$gross.sqft <- as.numeric(gsub("[^[:digit:]]","", bk$gross.square.feet))
head(bk$gross.sqft, 50)

bk$land.sqft <- as.numeric(gsub("[^[:digit:]]","", bk$land.square.feet))
head(bk$land.sqft, 50)

bk$year.built <- as.numeric(as.character(bk$year.built))
head(bk$year.built, 50)

## do a bit of exploration to make sure there's not anything
## weird going on with sale prices
attach(bk)
hist(sale.price.n) 
detach(bk)

## keep only the actual sales

bk.sale <- bk[bk$sale.price.n!=0,]

op <- par(mfrow = c(1,1))
plot(bk.sale$gross.sqft,bk.sale$sale.price.n)
title("Before log transform")
plot(log10(bk.sale$gross.sqft),log10(bk.sale$sale.price.n))
title("After log transform")
rm(op)

## for now, let's look at 1-, 2-, and 3-family homes
bk.homes <- bk.sale[which(grepl("FAMILY",bk.sale$building.class.category)),]
dim(bk.homes)
plot(log10(bk.homes$gross.sqft),log10(bk.homes$sale.price.n))
summary(bk.homes[which(bk.homes$sale.price.n<100000),])


## remove outliers that seem like they weren't actual sales
bk.homes$outliers <- (log10(bk.homes$sale.price.n) <=5) + 0
bk.homes <- bk.homes[which(bk.homes$outliers==0),]
plot(log10(bk.homes$gross.sqft),log10(bk.homes$sale.price.n))
