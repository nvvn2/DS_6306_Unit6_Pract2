# Author: Shravan Kuchkula 

##install the gdata and plyr packages and load in to R.
library(plyr)
library(gdata)
setwd("/Users/Shravan/R/projects/DS_6306_Unit6_Pract2/")

# So, save the file as a csv and use read.csv instead
bk <- read.xls("Analysis/Data/rollingsales_manhattan.xls",skip=4,header=TRUE)

# And sale.price.n is numeric, not a factor.
bk$SALE.PRICE.N <- as.numeric(gsub("[^[:digit:]]","", bk$SALE.PRICE))
count(is.na(bk$SALE.PRICE.N))

names(bk) <- tolower(names(bk)) # make all variable names lower case
names(bk)

## Get rid of leading digits
bk$gross.sqft <- as.numeric(gsub("[^[:digit:]]","", bk$gross.square.feet))
bk$land.sqft <- as.numeric(gsub("[^[:digit:]]","", bk$land.square.feet))
bk$year.built <- as.numeric(as.character(bk$year.built))

#Take a backup
bk_backup <- bk

# PART 1: Remove observations with gross.sqft and sale.price.n equal to NA
# Remove all observations which don't have gross.sqft
bk <- bk[!is.na(bk$gross.sqft),]

# Now remove all observations which don't have sales
#count(is.na(bk$sale.price.n))
bk <- bk[!is.na(bk$sale.price.n),]
#head(bk$sale.price.n, 100)
#head(bk$gross.sqft, 100)

# Now check if sale.price.n is equal to 0
count(bk$sale.price.n != 0)
bk <- bk[(bk$sale.price.n != 0),]

bk$sale.price.n

# Draw a hist with breaks 10
hist(log10(bk$sale.price.n), breaks = 100)
hist(log10(bk$sale.price.n), breaks = 100, col="green", border="blue")

mean(bk$sale.price.n)
boxplot(bk$sale.price.n)
count(bk$sale.price.n > 1000000)
count(bk$sale.price.n > mean(bk$sale.price.n))

plot(log10(bk$sale.price.n), type="b")
plot(log10(bk$sale.price.n), type="h")
plot(log10(bk$sale.price.n), type="S")

# Draw a scatter plot
library(MASS)
plot(log10(bk$sale.price.n), log10(bk$gross.sqft), main="Scatterplot")
sunflowerplot(log10(bk$sale.price.n), log10(bk$gross.sqft), main="Scatterplot")

boxplot(bk$neighborhood ~ bk$sale.price.n, data=bk)
boxplot(log10(bk$sale.price.n) ~ bk$neighborhood, data=bk)

str(bk)
