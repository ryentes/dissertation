### Check simulated dataset to see if the appropriate item parameters
### Can be recovered.
# Load Dependencies
library(mirt)
library(plyr)

# Set Working Directory
setwd("e:/dropbox/dissertation/03 - code")

# Load utilities
source("utils.R")

d <- read.table("cyw/bigSim.dat", header=T)

# Select out constructs to be included and create a vector of keys for 
# reverse coded items
sincerity <- d[,1:10]
fairness <- d[,11:20]
anxiety <- d[,21:30]
dependence <- d[,31:40]
liveliness <- d[,41:50]
forgiveness <- d[,51:60]
patience <- d[,61:70]
perfectionism <- d[,71:80]
inquisitiveness <- d[,81:90]
unconventionality <- d[,91:100]

# Create a vector of names
n <- c("sincerity", "fairness", "anxiety", "dependence", "liveliness",
       "forgiveness", "patience", "perfectionism", "inquisitiveness", 
       "unconventionality")

# Define keys for negatively coded variables
# Note: Negative is defined in reference to the construct name,
# Not it's social desirability. dependence is absent because it
# consists of only positive items.
key <- list()
key[["sinc"]] <- c(2:10)
key[["fair"]] <- c(6:10)
key[["anxi"]] <- c(6:10)
key[["live"]] <- c(9,10)
key[["forg"]] <- c(5:10)
key[["pati"]] <- c(6:10)
key[["perf"]] <- c(9,10)
key[["inqu"]] <- c(7:10)
key[["unco"]] <- c(6:10)

# Reverse code and compute factor scores for each  facet.
for (i in 1:length(n)){
  name <- substr(n[i],1,4)
  if(!is.null(key[[name]])) {  
    tmp <- revcode(eval(as.name(n[i])),key[[name]],7)
  } else tmp <- eval(as.name(n[i]))
  tmp <- rowMeans(tmp)
  assign(name, tmp) 
}

# Combine factor scores into a matrix
factors <- cbind(sinc,fair,anxi,depe,live,forg,pati,perf,inqu,unco)

# Correlations
fcorr <- cor(factors)

# Load correlation matrix used to simulate facet-level data
ocorr <- as.matrix(read.table("resources/fcorr.dat", header=T, row.names=1))

# Find the difference between recovered facet scores and those originally
# specified
ftest <- abs(abs(fcorr) - abs(ocorr))

# Check max difference
max(ftest)

# Print to console
ftest

# Write to file
write.table(ftest, "cyw/ftest.dat")

## Check item parameters
# Instantiate Looping Variables
ipar <- NULL
rows <- NULL

# Estimate item parameters by construct using the grm. eval(as.name()) replaces 
# itself with the "name" for the current value of x. Then extract the item
# parameters from the model object as a dataframe 
for(c in 1:length(n)) {
  y <- mirt(eval(as.name(n[c])), 1)
  p <- as.data.frame(coef(y, simplify=T)$items) 
  ipar <- rbind.fill(ipar,p)
  rows <- c(rows, rownames(p))
}

# rbind.fill doesn't support row names, so rejoin those
rownames(ipar) <- rows

# Load item parameters used to simulate item level data
opar <- read.table("resources/ipar.dat", header=T, row.names=1)

# Compute the difference between recovered item parameters and those
# used to simulate the data.
iptest <- abs(abs(ipar) - abs(opar))

# Check max differnece
max(iptest, na.rm=T)

# print to console
iptest

# Write to file
write.table(iptest, "cyw/iptest.dat")