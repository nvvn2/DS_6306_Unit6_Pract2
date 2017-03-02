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
count(bk$gross.sqft != 0)
bk <- bk[(bk$gross.sqft != 0),]


# Draw a hist with breaks 10
hist(log10(bk$sale.price.n), breaks = 100)
hist(log10(bk$sale.price.n), breaks = 100, col="green", border="blue")


#Using truehist
library(MASS)
truehist(log10(bk$sale.price.n))
lines(density(log10(bk$sale.price.n)))

#Using qqPlot from car package
library(car)
qqPlot(log10(bk$sale.price.n))



# Draw a scatter plot

plot(log10(bk$gross.sqft), log10(bk$sale.price.n), main="Scatterplot")
linear_model <- lm(log10(bk$sale.price.n) ~ log10(bk$gross.sqft), data=bk)
abline(linear_model, lty =2)

log10(bk$gross.sqft)

# Remove outliers

## for now, let's look at 1-, 2-, and 3-family homes
bk.homes <- bk[which(grepl("FAMILY",bk$building.class.category)),]
dim(bk.homes)
plot(log10(bk.homes$gross.sqft),log10(bk.homes$sale.price.n))
text(x = 3.3, y = 2, labels = "Outliers --->", col = "red", cex = 1.2, font=4, srt = -45)


## remove outliers that seem like they weren't actual sales
bk.homes$outliers <- (log10(bk.homes$sale.price.n) <=5) + 0
bk.homes$outliers
bk.homes <- bk.homes[which(bk.homes$outliers==0),]
plot(log10(bk.homes$gross.sqft),log10(bk.homes$sale.price.n))

# East Village Neighbhorhood
bk.neighbor <- bk[which(grepl("UPPER EAST SIDE", bk$neighborhood)),]
bk.neighbor$neighborhood
plot(log10(bk.neighbor$gross.sqft), log10(bk.neighbor$sale.price.n))
bk.neighbor$outliers <- (log10(bk.neighbor$sale.price.n) <=5) + 0
bk.neighbor <- bk.neighbor[which(bk.neighbor$outliers==0),]
plot(log10(bk.neighbor$gross.sqft), log10(bk.neighbor$sale.price.n))


abline(a=0, b=1)

abline(a = 5.5, b=3, lty=2, lwd=2, col="red")
?abline()



mean(bk$sale.price.n)
boxplot(bk$sale.price.n)
count(bk$sale.price.n > 1000000)
count(bk$sale.price.n > mean(bk$sale.price.n))

plot(log10(bk$sale.price.n), type="b")
plot(log10(bk$sale.price.n), type="h")
plot(log10(bk$sale.price.n), type="S")


boxplot(bk$neighborhood ~ bk$sale.price.n, data=bk)

boxplot(log10(bk$sale.price.n) ~ bk$neighborhood, data=bk)
op <- par(mar=c(11,4,4,2))
boxplot(log10(bk$sale.price.n) ~ bk$neighborhood, data=bk, las=2, cex.names=0.1)
rm(op)

str(bk)
