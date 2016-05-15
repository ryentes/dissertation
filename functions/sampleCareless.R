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
  CLProps <- round(rnorm(rep(length(pars),1),mu,sigma)*n)
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