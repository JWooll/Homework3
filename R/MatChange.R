#'
#' Matrix Changer
#' 
#' This function takes in a matrix and a vector and multiplies
#' the transpose of the vector by the inverse of the matrix by the
#' original, non-transposed vector.
#' @param A A square matrix of length n X n
#' @param x a vector of length n
#' @return an altered matrix
#' @export
MatChange <- function(A,x)
{
  stopifnot(is.numeric(A))
  stopifnot(is.finite(sum(A)))
  stopifnot(is.numeric(x))
  stopifnot(is.finite(sum(x)))
  stopifnot(length(x) == length(A[1,]))
  stopifnot(length(x) == length(A[,1]))
  A = t(t(x/A)*x)
  return(A)
}