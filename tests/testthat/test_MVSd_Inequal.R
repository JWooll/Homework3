
context("Mean, Variance, and SD of numeric vector given probability vector")

test_that("Values are same to R calculated data values", {
  x <- 1:5
  p <- c(.2,.3,.1,.25,.15)
  mu = 0
  pvar = 0
  for(i in 1:length(x))
  {mu = mu + x[i]*p[i]}
  pvar = 0
  for(i in 1:length(x))
  {pvar = pvar + ((x[i] - mu)^2 * p[i])}
  
  x1 <- MVSd_Inequal(x,p)
  x2 <- c(mu,pvar,sqrt(pvar))
  expect_identical(x1,x2)
})