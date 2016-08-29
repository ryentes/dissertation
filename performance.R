###### Overall Sample functions
get.crByModel <- function(x) {
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
get.perfByModel <- function(x, flag) {
  r <- vector(mode='numeric', length=nModels)
  for(i in 1:nModels) {
    this.model <- which(x[,'crModel']==models[i])
    if(length(this.model) > 0) {
      r[i] <-  sum(flag[this.model] == 1)/ length(this.model)
    } else r[i] = NA
  }
  r <- t(r)
  colnames(r) <- models
  return(r)
}

# This is really just renaming a specific call of scale, but it makes the
# main perf functions a bit more readable
cri.cut <- function(x, cut=2, direction='right') {
  normed <- scale(x, center=T, scale=T)
  if(direction=='left')
    return(normed < cut)
  else return(normed > cut)
}

make.colnames <- function(pre, oldNames) {
  return(paste0(pre,toTitleCase(oldNames)))
}

##### Longstring Specific functions
## Longstring cuts to the right, because people with a long string of identical
## responses are thought to be responding carelessly
get.lsPerf <- function(x, cut) {
  lsFlag <- cri.cut(x[,'ls'], cut)
  ls.np <- sum(lsFlag)
  ls.nn <- sum(!lsFlag)
  ls.pctCorrect <- mean(lsFlag==knownCR)
  ls.ntp <- sum(lsFlag  & knownCR)
  ls.ntn <- sum(!lsFlag & !knownCR)
  if(ls.np > 0) ls.tp <-  ls.ntp / ls.np
  else ls.tp <- 0
  ls.fp <- 1-ls.tp
  if(ls.nn > 0) ls.tn <- ls.ntn / ls.nn
  else ls.tn <- 0
  ls.fn <- 1-ls.tn
  ls.perfByModel <- get.perfByModel(x, lsFlag)
  colnames(ls.perfByModel) <- make.colnames('ls.pct', colnames(ls.perfByModel))
  return(cbind(ls.np,
               ls.nn,
               ls.pctCorrect,
               ls.ntp,
               ls.ntn,
               ls.tp,
               ls.fp,
               ls.tn,
               ls.fn,
               ls.perfByModel))
}

##### Mah. D specific functions
## Mahlanobis Distance cuts to the right, because people with a high Mah. D are
## expected to be primarily careless
get.d2Perf <- function(x, cut, method='sd') {
  if(method=='tf') {
    d2Flag <- outlier(x, confidence=cut)
  }
  else d2Flag <- as.vector(cri.cut(outlier(x, raw=TRUE), cut))
  d2.np <- sum(d2Flag, na.rm=T)
  d2.nn <- sum(!d2Flag, na.rm=T)
  d2.pctCorrect <- mean(d2Flag==knownCR)
  d2.ntp <- sum(d2Flag & knownCR)
  d2.ntn <- sum(!d2Flag & !knownCR)
  if(d2.np > 0) d2.tp <- d2.ntp / d2.np
  else d2.tp <- 0
  d2.fp <- 1-d2.tp
  if(d2.nn > 0) d2.tn <- d2.ntn / d2.nn 
  else dt.tn <- 0
  d2.fn <- 1-d2.tn
  d2.perfByModel <- get.perfByModel(crStats, d2Flag)
  colnames(d2.perfByModel) <- make.colnames('d2.pct', colnames(d2.perfByModel))
  return(cbind(d2.np,
               d2.nn,
               d2.pctCorrect,
               d2.ntp,
               d2.ntn,
               d2.tp, 
               d2.fp,
               d2.tn,
               d2.fn,
               d2.perfByModel))
}
  

##### Even/Odd specific functions
## E/O Cuts to the left, because people with a low correlation between even/odd
## subscales are the ones we would think of as careless
get.eoPerf <- function(x, cut) {
  eoFlag <- cri.cut(x[,'eo'], cut, direction='left')
  eo.np <- sum(eoFlag, na.rm=T)
  eo.nn <- sum(!eoFlag, na.rm=T)
  eo.pctCorrect <- mean(eoFlag==knownCR)
  eo.ntp <- sum(eoFlag & knownCR)
  eo.ntn <- sum(!eoFlag & !knownCR)
  if(eo.np > 0) eo.tp <- eo.ntp / eo.np
  else eo.tp <- 0
  eo.fp <- 1-eo.tp
  if(eo.nn > 0) eo.tn <- eo.ntn / eo.nn
  else eo.tn <- 0
  eo.fn <- 1-eo.tn
  eo.perfByModel <- get.perfByModel(crStats, eoFlag)
  colnames(eo.perfByModel) <- make.colnames('eo.pct', colnames(eo.perfByModel))
  return(cbind(eo.np,
               eo.nn,
               eo.pctCorrect,
               eo.ntp,
               eo.ntn,
               eo.tp,
               eo.fp,
               eo.tn,
               eo.fn,
               eo.perfByModel))
}