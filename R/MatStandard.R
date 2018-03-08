#'
#'Standardization of Matrix Columns
#'
#'This Function standardizes all column in a matrix by subtracting
#'the mean of all values in a column divided by its standardization
#'from each value in the column
#'
#'@param A the matrix to be standardized
#'@return new standardized matrix
#'@export
MatStandard <- function(A)
{
  stopifnot(is.numeric(A))
  stopifnot(is.finite(sum(A)))
  stopifnot(nrow(A) > 1)
  S <- (apply(A,2,mean)/apply(A,2,sd))
  A <- sweep(A,2,S)
  
  return(A)
  
}