context("test_fast_dates")

test_that("Function as.POSIXct_fast:",
          {
            expect_equal(as.POSIXct_fast("2018-06-12", format="%Y-%m-%d"), as.POSIXct("2018-06-12", format="%Y-%m-%d"))
          })

