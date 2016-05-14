simFactorScores <- function(x,n,seed=NULL) {
  if(!(is.matrix(x) | is.data.frame(x))) {
    stop("x must be a matrix or a data frame")
  }
  if(!(n %% 1 == 0) & n>0) {
    stop("n must be an integer greater than 1")
  }
  if(!is.null(seed)) {
    if(seed %% 1 ==0 & seed>0) set.seed(seed)
    else stop("invalid seed")
  }
  nItems=dim(x)[1]
  w <- t(chol(x))
  r <- matrix(rnorm(nItems*n,0,1), nrow=nItems, ncol=n)
  return(t(w %*% r))
}