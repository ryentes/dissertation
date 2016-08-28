setwd("~/dissertation")
library(careless)
library(tools)
library(foreach)
library(doParallel)
source('performance.R')

dsnames <- list.files('simulated')

registerDoParallel()
test <- foreach(c=1:10, .combine='rbind') %dopar% {
  # Read the dataset
  d <- read.table(paste0('simulated/', dsnames[c]), header=T)

  # Split off the sample statistics
  crStats <- d[,101:102]
  d <- d[,1:100]

  ls <- longstring(d)
  d2 <- outlier(d)
  eo <- evenodd(d, rep(10,10))

  # Prepare common variables for functions
  crStats <- cbind(crStats, d2, ls, eo)
  crIndices <- cbind(ls,d2,eo)
  models <- levels(unique(crStats[,'crModel']))
  nModels <- length(models)
  knownCR <- crStats[,'careless']
  n <- nrow(crStats)

  #### Sample Statistics ###########
  pct.cr <- mean(crStats[,'careless'])
  pct.crByModel <- get.crByModel(crStats)
  ls.perf1 <- get.lsPerf(crStats, 1)
  ls.perf1.5 <- get.lsPerf(crStats, 1.5)
  ls.perf2 <- get.lsPerf(crStats, 2)
  d2.perf999 <- get.d2Perf(d, .999, method='tf')
  d2.perf95 <- get.d2Perf(d, .95, method='tf')
  d2.perf1 <- get.d2Perf(d, 1)
  d2.perf1.5 <- get.d2Perf(d, 1.5)
  d2.perf2   <- get.d2Perf(d, 2)
  eo.perf1 <- get.eoPerf(crStats, 1)
  eo.perf1.5 <- get.eoPerf(crStats, 1.5)
  eo.perf2 <- get.eoPerf(crStats, 1.5)
  
  cbind(c,
        pct.cr,
        pct.crByModel,
        ls.perf1,
        ls.perf1.5,
        ls.perf2,
        d2.perf999,
        d2.perf95,
        d2.perf1,
        d2.perf1.5,
        d2.perf2,
        eo.perf1,
        eo.perf1.5,
        eo.perf1.5)
}






# Run analyses
#   Cut tests
#   Combo rules
#     1. LS
#     2. Mah. D
#     3. E/O
#     4. LPA
#     5. LS -> Then compute Mah. D, E/O
#       a. either
#       b. both
#     6. Random Forest

# Print Table
