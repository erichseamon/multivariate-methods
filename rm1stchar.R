function (x, n = 1) 
{
  if (n < 0) 
    stop("'n' must be a positive integer")
  L <- length(x)
  start.col <- n
  x[start.col:length(x)] <- sapply(start.col:length(x), function(j, 
                                                                 x) {
    x[j] <- substr(x[j], start = start.col, stop = nchar(x[j]))
  }, x = x)
  return(x)
}