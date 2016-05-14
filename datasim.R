# load dependencies
library(mirt)
source('utils.R')

# import item parameters
ipar <- read.table("resources/ipar.dat", header=T, row.names=1,
                   stringsAsFactors=F)
# import correlation matrix 
fcorr <- read.table("resources/fcorr.dat", header=T, row.names=1)

# User specified variables
nObs <- 1000
nDataSets <- 5000
seed = 1234

## other parameters
a <- modelSlopes(ipar,colnames(fcorr), 5)
d <- as.matrix(ipar[,2:ncol(ipar)])

#while() {
  f = simFactorScores(fcorr,nObs, seed)
  ## Generate item level data
  fakeData <- simdata(a, d, itemtype="graded", Theta=f, mins=1)
  colnames(fakeData) <- rownames(ipar)
  write.table(fakeData, "cyw/simd1.dat", row.names=F)
  seed <- seed + 1
#}





## insert careless respondents

## export dataset to file

## i++