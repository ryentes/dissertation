setwd("~/dissertation")
library(careless)

# Score 
d <- read.table("simulated/sim1.dat", header=T)

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
n <- nrow(crStats)

#### Sample Statistics ###########
pct.cr <- mean(crStats[,'careless'])
pct.crByModel <- crByModel(crStats)
ls.perf1 <- lsPerf(crStats, 1)
ls.perf1.5 <- lsPerf(crStats, 1.5)
ls.perf2 <- lsPerf(crStats, 2)






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
