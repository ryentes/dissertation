## Simulate careless responder following the resources model
simCareless <- function(x, c) {
  # Random number representing length of the refractory period before
  # Responding carefully again
  refract <- rnorm(1, )
  if(c='longstring') {
    
  }
  else if(c='skewed'){
    
  }
}

## Simulate careless responder following the longstring model

## Simulate careless responder following the skewed distribution model

## Reverse coding
revcode <- function(x, neg, max) {
  for(i in 1:length(neg)) {
    x[,neg[i]] <- reverse(x[,neg[i]], max)
  }
  return(x)
}

# Takes a vector or scalar and reverse codes it. 
reverse <- function(var, max) {
  abs(var-(max+1))
}

# Model Slope Parameters
modelSlopes <- function(ipar, names, nchar) {
  nCol <- length(names)
  nRow <- nrow(ipar)
  a <- ipar[,1]
  x <- matrix(rep(0,nRow*nCol),nrow=nRow,ncol=nCol)
  for(i in 1:nRow) {
    n = substr(rownames(ipar)[i],1,nchar)
    x[i,which(names == n)]=a[i]
  }
  return(x)
}

#Simulate Factor scores
simFactorScores <- function(x,n,seed=NULL) {
  if (!is.null(seed)) set.seed(seed)
  nItems=dim(x)[1]
  w <- t(chol(x))
  r <- matrix(rnorm(nItems*n,0,1), nrow=nItems, ncol=n)
  return(t(w %*% r))
}