
context("Plotting two columns")

test_that("Tools_gg uses ggplot on two data columns", {
library(ggplot2)
x1 <- Tools_gg(AAF,"arr_delay","dep_delay",geom_point(alpha =.2))
x2 <- ggplot(data = AAF, aes(x = arr_delay, y = dep_delay,na.rm = T)) +
  geom_point(alpha = .2)
expect_identical(x1,x2)
})
