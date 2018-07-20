context("test_fast_dates")

test_that("Function as.Date.fast:",
          {
            expect_equal(as.Date.fast("2018-06-12", format="%Y-%m-%d"), as.Date("2018-06-12", format="%Y-%m-%d"))
          })


test_that("Function as.POSIXct.fast:",
          {
            expect_equal(as.POSIXct.fast("2018-06-12", format="%Y-%m-%d"), as.POSIXct("2018-06-12", format="%Y-%m-%d"))
          })

test_that("Function parse_date_time.fast:",
          {
            expect_equal(parse_date_time.fast("2018-06-12", orders="Ymd"), parse_date_time("2018-06-12", orders="%Y-%m-%d"))
          })