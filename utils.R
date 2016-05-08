## Simulate careless responder following the resources model

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