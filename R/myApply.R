#' A simple homemade apply function
#' 
#' Changes a user supplied matrix by a user supplied function on the dimension
#' that the user chooses with 1 being the first dimension and 2 being the seconnd
#' It also allows user to insert ther own further parameters 
#' 
#' @param X a matrix to be applied on
#' @param MARGIN for which dimension will be applied on
#' @param FUN the function that will change the matrix by
#' @param ... additional paramerters for the function if neccessary
#' @return the results of the permutated matrix in simple array form
#' @export
myApply <- function(X, MARGIN, FUN, ...)
{
  if(length(dim(X))!=2)
    stop("matrix is not 2d")
  if(!(MARGIN %in% c(1,2)))
    stop("Margin is not in 1 or 2")
  
  R = dim(X)[1]
  C = dim(X)[2]
  f = match.fun(FUN)
  
  if (MARGIN == 1)
  {
    result = list()
    for (i in 1:R)
      result[[i]] = f(X[i],...)
  } else if(MARGIN == 2)
  {
    result = list()
    for(j in 1:C)
      result[[j]] = f(X[,j],...)
  }
  return(simplify2array(result))
}
