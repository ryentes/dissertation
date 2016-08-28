###### Overall Sample functions
crByModel <- function(x) {
  # Instantiate result vector
  r <- vector(mode='numeric', length=nModels)
  # For each of the crModels compute the proportion of the sample that
  # followed that model
  for(i in 1:nModels) {
    varName <- paste('pct.', models[i])
    assign(varName, sum(x[which(x[,'crModel']==models[i]),'careless'])/n)
    r[i] <- eval(as.name(varName))
  }
  return(r)
}

##### Generic Functions
perfByModel <- function(x, flag) {
  r <- vector(mode='numeric', length=nModels)
  for(i in 1:nModels) {
    this.model <- which(x[,'crModel']==models[i])
    r[i] <-  sum(flag[this.model] == 1)/ length(this.model)
  }
  return(r)
}

##### Longstring Specific functions
# Compute the performance of the longstring metric
lsPerf <- function(x, cut) {
  lsFlag <- ls.cut(x[,'ls'])
  ls.ntp <- sum(lsFlag == 1 & knownCR == 1)
  ls.tp <-  ls.ntp / sum(lsBinary)
  ls.fp <- sum(lsFlag == 1 & knownCR == 0) / sum(lsBinary)
  ls.perfByModel <- perfByModel(x, lsFlag)
  return(c(ls.ntp/(pct.cr*nrow(x)),
           ls.tp,
           ls.fp,
           ls.perfByModel))
}

# This is really just renaming a specific call of scale, but it makes the
# main lsPerf function a bit more readable
ls.cut <- function(x, cut=2) {
  scale(x, center=T, scale=T) > 2
}

##### Mah. D specific functions
d2Perf

##### Even/Odd specific functions
