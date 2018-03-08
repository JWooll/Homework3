context("Homemade apply function")

test_that("myApply matches built in R apply functions",{
  fred <- matrix(1:6,ncol = 2)
  s = array(rnorm(72),c(3,3,8))
  R = dim(fred)[1]
  C = dim(fred)[2]
  f = match.fun("quantile")
  result = list()
  for (i in 1:R)
    result[[i]] = f(fred[i],probs=(1:3)/4)
  
  x1 <- myApply(fred,1,"quantile",probs=(1:3)/4)
  x2 <- simplify2array(result)
  expect_identical(x1,x2)
})