requireNamespace("data.table")
Sys.setlocale("LC_TIME", "C")
verbose <- TRUE

## generateFactorFromDate
#-----------------------
data("messy_adult")
messy_adult <- findAndTransformDates(messy_adult, verbose = FALSE)
store_ncol <- ncol(messy_adult)
messy_adult <- generateFactorFromDate(messy_adult, cols = c("date1", "date2", "num1"), type = "yearquarter")
test_that("generateFactorFromDate ",
          {
            expect_equal(ncol(messy_adult), store_ncol + 2)
          })


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
dataSet <- generateDateDiffs(dataSet, analysisDate = as.Date("2016-11-14"))

test_that("generateDateDiffs: ",
          {
            expect_equal(ncol(dataSet), 6)
            expect_equal(sum(is.na(dataSet)), 0)
          })


dataSet <- data.table(ID = 1:100, 
                      date1 = seq(from = as.Date("2010-01-01"), 
                                  to = as.Date("2015-01-01"), 
                                  length.out = 100), 
                      date2 = seq(from = as.Date("1910-01-01"), 
                                  to = as.Date("2000-01-01"), 
                                  length.out = 100)
)
test_that("generateDateDiffs: errors",
          {
            expect_error(generateDateDiffs(dataSet, analysisDate = "2017-01-01"),  "analysisDate must be a Date")
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