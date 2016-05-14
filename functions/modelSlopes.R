modelSlopes <- function(ipar, names, nchar) {
  nCol <- length(names)
  nRow <- nrow(ipar)
  a <- ipar[,1]
  x <- matrix(rep(0,nRow*nCol),nrow=nRow,ncol=nCol)
  for(i in 1:nRow) {
    x[i,which(names == substr(rownames(ipar)[i],1,nchar))]=a[i]
  }
  return(x)
}