#' Mean, Variation, and Standard Deviation on a numeric vector
#' given unequal probabilities of data values
#' 
#' The User will enter two numeric Vectors of the same size, on being tha values
#' and the other being the probability of those corresponding value. 
#' The function will return the mean, variation, and standard deviation of
#' that vector in that order.
#' 
#' @param x A numeric vector
#' @param p probabilities of the corresponding x
#' @return mean, variation, and standard deviation in a list in that order
#' @export
MVSd_Inequal <- function(x,p)
{
  stopifnot(length(x) > 0)
  stopifnot(length(x) == length(p))
  ptester = 0
  for(i in 1:length(x))
  {
    stopifnot(is.numeric(x[i]))
    stopifnot(is.finite(x[i]))
    stopifnot(is.numeric(p[i]))
    stopifnot(is.finite(p[i]))
    stopifnot(p[i] >= 0)
    ptester = ptester + p[i]
  }
  stopifnot(all.equal(ptester,1))
  mu = 0
  for(i in 1:length(x))
  {mu = mu + x[i]*p[i]}
  
  pvar = 0
  for(i in 1:length(x))
  {pvar = pvar + ((x[i] - mu)^2 * p[i])}
  
  psd = sqrt(pvar)
  
  return(c(mu,pvar,psd))
}