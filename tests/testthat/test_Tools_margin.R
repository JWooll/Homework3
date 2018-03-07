
context("Margins of data")

           test_that("Tools_margin finds accurate margins of a data set", {
             np <- dplyr::select(AAF,"arr_time")
             mx <- max(np,na.rm = T)
             mn <- min(np,na.rm = T)
           expect_identical(Tools_margin(AAF,"arr_time"),c(np,mx,mn))
           })
