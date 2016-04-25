### Load Hexaco data and prepare it for Parameter estimation
setwd("e:/dropbox/dissertation/03 - code")

# Hexaco dataset  downloaded from http://personality-testing.info/_rawdata/
# on 4/18/2016. Imported into SPSS then resaved as an actual .csv
d <- read.csv("data/hexaco.csv", header=T, stringsAsFactors=F)

# Find missing country codes and repalce them with NA values
x <- which(d[,243]== " ")
d[x,243] <- NA
rm(x)

# Remove obeservations where respondents reported inadequate 
# understanding of instructions or attention
test1 <- d[,"V1"] >= 5
test2 <- d[,"V2"] >= 5
d <- d[test1 & test2,]

# Select out constructs to be included

# Estimate item parameters by construct using the grm

# Compute the correlation matrix for the constructs