setwd('~/dissertation')

# load dependencies
library(mirt)
library(doParallel)
library(foreach)
source('utils.R')

# import item parameters
ipar <- read.table("resources/ipar.dat", header=T, row.names=1,
                   stringsAsFactors=F)
# import correlation matrix 
fcorr <- read.table("resources/fcorr.dat", header=T, row.names=1)

# User specified variables
nObs <- 1000
nDatasets <- 5000
labels <- c('longstring', 'skewed')
lsDis <- c(.07,.02)
skewDis <- c(.11,.04)
seed = 1234

## other parameters
a <- modelSlopes(ipar,colnames(fcorr), 5)
d <- as.matrix(ipar[,2:ncol(ipar)])
i <- 1

ptime <- system.time({
  foreach(i=1:nDatasets) %dopar% {
    f = simFactorScores(fcorr,nObs, seed)
    ## Generate item level data
    fakeData <- simdata(a, d, itemtype="graded", Theta=f, mins=1)
    colnames(fakeData) <- rownames(ipar)
    # Insert careless respndents
    fakeData <- cbind(fakeData,sampleCareless(nObs, labels, lsDis, skewDis))
    careless <- fakeData[,'careless']==1
    fakeData[careless,] <- t(apply(fakeData[careless,],1,simCareless))

    # write the dataset to a file
    fn <- paste0("simulated/sim", i, ".dat")
    write.table(fakeData, fn, row.names=F)
  
    print(i)
    #increment the looping variables
    seed <- seed + 1
    i <- i+1
  }
})





