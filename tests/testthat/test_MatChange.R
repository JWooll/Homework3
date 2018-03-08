context("A Matrix permutated in a specific way")

test_that("MatChange creates the same result as permutating directly",{
  A <- matrix(c(2,4,3,1), nrow = 2,ncol = 2)
  x <- 5:6
  x1 <- MatChange(A,x)
  x2 <- t(t(x/A)*x)
  expect_identical(x1,x2)        
})