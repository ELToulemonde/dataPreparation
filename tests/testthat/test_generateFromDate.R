context("test_generateFromDate.R")
requireNamespace("data.table")
Sys.setlocale("LC_TIME", "C")
verbose <- TRUE

## generateFactorFromDate
#-----------------------
data("messy_adult")
messy_adult <- findAndTransformDates(messy_adult, verbose = FALSE)
store_ncol <- ncol(messy_adult)
test_that("generateFactorFromDate ",
          {
            expect_equal(
              ncol(generateFactorFromDate(messy_adult, cols = c("date1", "date2"), drop = TRUE, type = "yearquarter", verbose = verbose)), 
              store_ncol)
          })

messy_adult <- generateFactorFromDate(messy_adult, cols = "auto", drop = TRUE, type = "yearquarter", verbose = verbose)
## date_factor
# -----------
dataSet <- as.Date(c("2014-01-01", "2015-01-01", "2015-06-01"))
test_that("Private function: date_factor",
          {
            expect_identical(date_factor(dataSet, type = "yearmonth"), factor(c("2014 Jan", "2015 Jan", "2015 Jun")))
            expect_identical(date_factor(dataSet, type = "yearquarter"), factor(c("2014 Q1", "2015 Q1", "2015 Q2")))
            expect_identical(date_factor(dataSet, type = "quarter"), factor(c("Q1", "Q1", "Q2")))
            expect_identical(date_factor(dataSet, type = "month"), factor(c("Jan", "Jan", "Jun")))
            expect_identical(date_factor(dataSet, type = "year"), factor(c("2014", "2015", "2015")))
            expect_error(date_factor(1:5), ": dataSet should contain dates.")
            expect_error(date_factor(dataSet, type = "acdaezcze"), ": type must be one of 'year', 'yearquarter', 'yearmonth', 'quarter' or 'month'.")
          })

## generateDateDiffs
#-------------------
dataSet <- data.table(ID = 1:100, 
                      date1 = seq(from = as.Date("2010-01-01"), 
                                  to = as.Date("2015-01-01"), 
                                  length.out = 100), 
                      date2 = seq(from = as.Date("1910-01-01"), 
                                  to = as.Date("2000-01-01"), 
                                  length.out = 100)
)

# Now let's compute
result <- generateDateDiffs(copy(dataSet), cols = "auto", analysisDate = as.Date("2016-11-14"), drop = TRUE, verbose = verbose)

test_that("generateDateDiffs: ",
          {
            expect_equal(ncol(result), 4)
            expect_false(any(is.na(result)))
          })


test_that("generateDateDiffs: errors",
          {
            expect_error(generateDateDiffs(copy(dataSet), cols = "auto", analysisDate = "2017-01-01", verbose = verbose),  
                         "analysisDate must be a Date")
          })


## diffTime
#----------

test_that("diffTime: ",
          {
            expect_equal(diffTime(as.Date("2017-01-02"), as.Date("2017-01-01"), units = "days"), 1)
            expect_equal(diffTime(as.Date("2017-01-02"), as.Date("2017-01-01"), units = "hours"), 24)
            expect_equal(diffTime(as.Date("2017-01-02"), as.Date("2017-01-01"), units = "mins"), 1440)
            expect_equal(diffTime(as.Date("2017-01-02"), as.Date("2017-01-01"), units = "years"), 1 / 365.25)
            expect_error(diffTime(as.Date("2017-01-02"), as.Date("2017-01-01"), units = "zadtfrey"), "Sorry this unit hasn't been implemented yet")
          })
