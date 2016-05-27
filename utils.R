## Simulate careless responder following the resources model
simCareless <- function(x,floor=1,ceiling=7) {
  nitems <- length(x)-2
  insert <- round(rnorm(1, mean=50,sd=10)) # get from adam's dataset?
  if(insert < 1) insert <- 1 #do i want a lower bound?
  refract <- round(rnorm(1, mean=10,sd=5))
  if(x['crModel']=='longstring') {
    repvalue <- sample(floor:ceiling,1)
    x[insert:nitems] <- repvalue
  }
  if(x['crModel']=='skewed') {
    span <- insert:nitems
    repvalue <- genSkewed(length(span))
    x[span] <- repvalue
  }
  return(x)
}

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

# Draws a sample of careless responders from the set of respondents, 
# in proportion to the number of careless respondents for each model,
# The number of careless respondents for each model of careless responding
# is sampled individually
# 
# This function takes n) the total sample size and lables) which are the 
# labels for the different models of careless responding. Additionally,
# It requires and arbitrary number of additional vectors representing the
# mean and standard deviation for each careless responding model, in the order
# in which they appear in the labels vector.
sampleCareless <- function(n,labels,...) {
  pars <- list(...)
  # Create vectors for means and standard deviations
  mu <- unlist(lapply(pars, '[[', 1))
  sigma <- unlist(lapply(pars, '[[', 2))
  # Sample CL model proportions and multiply by n to get the number
  # of people responding carelessly according to each type.
  CLProps <- round(abs(rnorm(rep(length(pars),1),mu,sigma)*n))
  nCL <- sum(CLProps)
  # Randomly choose responders to be careless
  whichCL <- sample(n, nCL)
  # Create columns to keep track of who was careless and whic type.
  CL <- cbind(rep(0,n),rep(NA,n))
  colnames(CL) <- c('careless', 'crModel')
  CL[whichCL,1] <- 1
  CL[whichCL,2] <- recursiveAssign(whichCL, CLProps, labels)
  return(CL)
}

# Recursively sample a set in order randomly assign a series of labels
recursiveAssign <- function(x, d, labels) {
  # If it's the last label, just assign whatever is left
  if(length(d) == 1) {
    x[] <- labels
  } else { 
    # Draw a sample careless respondents of size (d) for the next model (label)
    selected <- sample(1:length(x),d[1])
    selected <- is.element(1:length(x),selected)
    x[selected] <- labels[1]
    # trim the size and label variables to remove the model that has just
    # been sampled.
    labels <- labels[2:length(labels)]
    d <- d[2:length(d)]
    # Call the function again to assign the remaining careless responders to 
    # their model of careless responding
    x[!selected] <- recursiveAssign(x[!selected],d,labels)
  }
  return(x)
}

genSkewed <- function(x,mu=5.75, sigma=1.25, floor=1,ceiling=7) {
  r <- round(rnorm(x, mu, sigma))
  r[r>ceiling] <- ceiling
  r[r<floor] <- floor
  return(r)
}