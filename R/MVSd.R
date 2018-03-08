
#' Mean, Variation, and Standard Deviation on a numeric vector
#' 
#' The User will enter in a numeric Vector and the function
#' will return the mean, variation, and standard deviation of
#' that vector in that order.
#' 
#' @param x A numeric vector
#' @return mean, variation, and standard deviation in a list in that order
#' @export
MVSd <- function(x)
{
  stopifnot(length(x) > 0)
  for(i in 1:length(x))
  {
    stopifnot(is.finite(x[i]))
    stopifnot(is.numeric(x[i]))
  }
  
  mu = 0
  for(i in 1:length(x))
  {mu = mu + x[i]}
  mu = mu/length(x)
  
  pvar = 0
  for(i in 1:length(x))
  {pvar = pvar + ((x[i] - mu)^2)}
  pvar = pvar/length(x)
  
  psd = sqrt(pvar)
  
  return(c(mu,pvar,psd))
}