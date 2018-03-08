
context("Finding the mean, variance and SD for a numeric vector")

test_that("Finds accurate data on vector", {
  dat <- 1:4
  pvar = 0
  for(i in 1:length(dat))
  {pvar = pvar + ((dat[i] - mean(dat))^2)}
  pvar = pvar/length(dat)
  x1 <- MVSd(dat)
  x2 <- c(mean(dat),pvar,sqrt(pvar))
  expect_identical(x1,x2)
})