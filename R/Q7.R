
#' Three Methods of Probability Distribution
#'
#' Depending on which probability distribution method the
#' user wants to use, (0 for Gamma, 1 for Cauchy, 2 for Binomial)
#' the function takes the data and a given interval and
#' Returns the maximum optimized sum of the distribution
#'
#' for gamma: Q7(x,0,c(0,10))
#' for Cauchy: Q7(x,1,c(-10,10))
#' for binomial: Q7(x,2,c(-10,10))
#'
#'@param data used, type of distribution, interval
#'@param funct what function the user would like to use
#'with 0,1, and 2 corresponding with gamma cauchy and binomial
#'@param interval the interval across which the
#'optimize function will operate across
#'@return Sum of distribution
#'@export
Q7 <- function(data,funct,interval)
{
  theta <- pi
  if (funct == 0)
    log1 <- function(theta)
      sum(dgamma(data, shape = theta, log = TRUE))
  if (funct == 1)
    log1 <- function(theta)
      sum(dcauchy(data, location = theta, log = TRUE))
  if (funct == 2)
    log1 <- function(theta)
      sum(dbinom(x, 20, prob = 1 / (1 + exp(- theta)), log = TRUE))

  oout <- optimize(log1, maximum = TRUE, interval)
  return(oout$maximum)
}
