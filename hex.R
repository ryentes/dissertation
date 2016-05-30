### Load Hexaco data and prepare it for Parameter estimation
library(mirt)
library(plyr)
library(foreach)
library(doParallel)

# Set Working Directory

# Load utilities
source("utils.R")

# Hexaco dataset  downloaded from http://personality-testing.info/_rawdata/
# on 4/18/2016. Imported into SPSS then resaved as an actual .csv
d <- read.csv("data/hexaco.csv", header=T, stringsAsFactors=F)

# Find missing country codes and repalce them with NA values
x <- which(d[,243]== " ")
d[x,243] <- NA
rm(x)

# Remove obeservations where respondents reported inadequate 
# understanding of instructions or attention. Min time to completion
# Was x and max time to completion was y, so instead of cutting by 
# standard deviation, somewhat arbirtrary time bounds were imposed.
test1 <- d[,"V1"] >= 5
test2 <- d[,"V2"] >= 5
test3 <- d[,"elapse"] < 6000 # Around 2 hours
test4 <- d[,"elapse"] > 1037.456 # less than 17.5 minutes
d <- d[which(test1 & test2 & test3 & test4),]
rownames(d) <- NULL

# Remove extra characters from HSinc1 columname
colnames(d)[1] <- "HSinc1"

# Select out constructs to be included and create a vector of keys for 
# reverse coded items
HSincerity <- d[,1:10]
HFairness <- d[,11:20]
EAnxiety <- d[,51:60]
EDependence <- d[,61:70]
XLiveliness <- d[,111:120]
AForgiveness <- d[,121:130]
APatience <- d[,151:160]
CPerfectionism <- d[,181:190]
OInquisitiveness <- d[,211:220]
OUnconventionality <- d[,231:240]

# Create a vector of names
n <- c("HSincerity", "HFairness", "EAnxiety", "EDependence", "XLiveliness",
       "AForgiveness", "APatience", "CPerfectionism", "OInquisitiveness", 
       "OUnconventionality")

# Define keys for negatively coded variables
# Note: Negative is defined in reference to the construct name,
# Not it's social desirability. dependence is absent because it
# consists of only positive items.
key <- list()
key[["HSinc"]] <- c(2:10)
key[["HFair"]] <- c(6:10)
key[["EAnxi"]] <- c(6:10)
key[["XLive"]] <- c(9,10)
key[["AForg"]] <- c(5:10)
key[["APati"]] <- c(6:10)
key[["CPerf"]] <- c(9,10)
key[["OInqu"]] <- c(7:10)
key[["OUnco"]] <- c(6:10)
  
# Reverse code and compute factor scores for each  facet.
for (i in 1:length(n)){
  name <- substr(n[i],1,5)
  if(!is.null(key[[name]])) {  
    tmp <- revcode(eval(as.name(n[i])),key[[name]],7)
  } else tmp <- eval(as.name(n[i]))
  tmp <- rowMeans(tmp)
  assign(name, tmp) 
}

# Combine factor scores into a matrix
factors <- cbind(HSinc,HFair,EAnxi,EDepe,XLive,AForg,APati,CPerf,OInqu,OUnco)

fcorr <- cor(factors)

# write out correlation matrix
write.table(fcorr, file="resources/fcorr.dat")

# Check response option frequencies 
x <- t(apply(d[,1:240],2,table))

# Write out response option frequencies
write.table(x, file="resources/responsefreq.dat")

# Instantiate Looping Variables
ipar <- NULL
rows <- NULL

# Estimate item parameters by construct using the grm. eval(as.name()) replaces 
# itself with the "name" for the current value of x. Then extract the item
# parameters from the model object as a dataframe 
registerDoParallel(8)
ipar <- foreach(c=1:length(n), .combine=rbind) %dopar% {
  y <- mirt(eval(as.name(n[c])), 1)
  as.data.frame(coef(y, simplify=T)$items) 
}


# rbind.fill doesn't support row names, so rejoin those
rownames(ipar) <- rows

# Write out item parameters
write.table(ipar, file="resources/ipar.dat")

# Clean up the work space (warning this will wipe your R environment)
rm(list=ls())