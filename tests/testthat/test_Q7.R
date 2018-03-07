
context("Sum of Distribution")

           test_that("Q7 return accurate maximum optimized sum of distribution", {
           theta <- pi
           log1 <- function(theta)
             sum(dgamma(1:5, shape = theta, log = TRUE))
           x1 <- (Q7(1:5,0,c(0,10)))
           oout <- (optimize(log1, maximum = TRUE, c(0,10)))
           x2 <- oout$maximum
           expect_identical(x1,x2)
           })
