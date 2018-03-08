context("function that standardizes matrix columns")

test_that("In package function is equal to looped standardization",{
  A <- matrix(c(3,4,1,7,8,9,2,6,5),nrow = 3,ncol = 3)
  x1 <- MatStandard(A)
  x2 <- matrix(0,nrow = nrow(A),ncol = ncol(A))
  for (i in 1:ncol(A))
    for (j in 1:nrow(A))
    {
      x2[j,i] = A[j,i] - (mean(A[,i])/sd(A[,i]))
    }
  expect_identical(x1,x2)
})