context("test_generateFromDate.R")
requireNamespace("data.table")
Sys.setlocale("LC_TIME", "C")
verbose <- TRUE

## generateFactorFromDate
# -----------------------
test_that("generateFactorFromDate: functionnal drop = TRUE indeed drop col and new col has type in name ",
          {
            # Given
            dataSet <- data.table(col = as.POSIXct(c("2018-01-31", "2019-02-03")))
            store_ncol <- ncol(dataSet)
            type <- "yearquarter"
            
            # When
            dataSet_transformed <- generateFactorFromDate(dataSet, drop = TRUE, type = type, verbose = verbose)
            
            # Then
            expect_equal(ncol(dataSet_transformed), store_ncol)
            expect_false("col" %in% colnames(dataSet_transformed))
            expect_true(paste0("col", ".", type) %in% colnames(dataSet_transformed))
          })

## date_factor
# -----------
test_that("private function: date_factor: test result for type yearmonth",
          {
            # Given
            dataSet <- as.Date(c("2014-01-01", "2015-01-01", "2015-06-01"))
            type <- "yearmonth"
            expected_result <- factor(c("2014 Jan", "2015 Jan", "2015 Jun"))
            
            # When
            result <- date_factor(dataSet, type = type)
            
            # Then
            expect_identical(result, expected_result)
          })

test_that("private function: date_factor: test result for type yearquarter",
          {
            # Given
            dataSet <- as.Date(c("2014-01-01", "2015-01-01", "2015-06-01"))
            type <- "yearquarter"
            expected_result <- factor(c("2014 Q1", "2015 Q1", "2015 Q2"))
            
            # When
            result <- date_factor(dataSet, type = type)
            
            # Then
            expect_identical(result, expected_result)
          })

test_that("private function: date_factor: test result for type quarter",
          {
            # Given
            dataSet <- as.Date(c("2014-01-01", "2015-01-01", "2015-06-01"))
            type <- "quarter"
            expected_result <- factor(c("Q1", "Q1", "Q2"))
            
            # When
            result <- date_factor(dataSet, type = type)
            
            # Then
            expect_identical(result, expected_result)
          })

test_that("private function: date_factor: test result for type month",
          {
            # Given
            dataSet <- as.Date(c("2014-01-01", "2015-01-01", "2015-06-01"))
            type <- "month"
            expected_result <- factor(c("Jan", "Jan", "Jun"))
            
            # When
            result <- date_factor(dataSet, type = type)
            
            # Then
            expect_identical(result, expected_result)
          })

test_that("private function: date_factor: test result for type year",
          {
            # Given
            dataSet <- as.Date(c("2014-01-01", "2015-01-01", "2015-06-01"))
            type <- "year"
            expected_result <- factor(c("2014", "2015", "2015"))
            
            # When
            result <- date_factor(dataSet, type = type)
            
            # Then
            expect_identical(result, expected_result)
          })

test_that("private function: date_factor: error if dataSet doesn't contain dates",
          {
            # Given
            dataSet <- 1:5
            
            # When + Then
            expect_error(date_factor(dataSet), ": dataSet should contain dates.")
          })

test_that("Private function: date_factor: error on wrong type",
          {
            # Given
            dataSet <- as.Date(c("2014-01-01", "2015-01-01", "2015-06-01"))
            wrong_type <- "acdaezcze"
            
            # When + Then
            expect_error(date_factor(dataSet, type = wrong_type), 
                         ": type must be one of 'year', 'yearquarter', 'yearmonth', 'quarter' or 'month'.")
          })

## generateDateDiffs
# ------------------
test_that("generateDateDiffs: generate 1 new col for the difference between date 1 and date 2",
          {
            # Given
            dataSet <- data.table(ID = 1:100, 
                                  date1 = seq(from = as.Date("2010-01-01"), 
                                              to = as.Date("2015-01-01"), 
                                              length.out = 100), 
                                  date2 = seq(from = as.Date("1910-01-01"), 
                                              to = as.Date("2000-01-01"), 
                                              length.out = 100)
            )
            analysis_date <- as.Date("2016-11-14")
            
            # When
            dataSet_transformed <- generateDateDiffs(copy(dataSet), analysisDate = analysis_date, drop = TRUE, verbose = verbose)
            
            # Then
            expect_equal(ncol(dataSet_transformed), ncol(dataSet) + 1)
            expect_false(any(is.na(dataSet_transformed)))
          })


test_that("generateDateDiffs: errors",
          {
            # Given
            dataSet <- data.table(date1 = seq(from = as.Date("2010-01-01"), 
                                              to = as.Date("2015-01-01"), 
                                              length.out = 100))
            wrong_analysis_date <- "2017-01-01"
            
            # When + Then
            expect_error(generateDateDiffs(dataSet, analysisDate = wrong_analysis_date, verbose = verbose),  
                         "analysisDate must be a Date")
          })


# diffTime
# --------
test_that("diffTime: diff in days",
          {
            # Given 
            date_1 <- as.Date("2017-01-03")
            date_2 <- as.Date("2017-01-02")
            units <- "days"
            
            # When
            diff_result <- diffTime(date_1, date_2, units = units)
            
            # Then
            expect_equal(diff_result, 1)
          })

test_that("diffTime: diff in hours",
          {
            # Given 
            date_1 <- as.Date("2017-01-03")
            date_2 <- as.Date("2017-01-02")
            units <- "hours"
            
            # When
            diff_result <- diffTime(date_1, date_2, units = units)
            
            # Then
            expect_equal(diff_result, 24)
          })

test_that("diffTime: diff in mins",
          {
            # Given 
            date_1 <- as.Date("2017-01-03")
            date_2 <- as.Date("2017-01-02")
            units <- "mins"
            
            # When
            diff_result <- diffTime(date_1, date_2, units = units)
            
            # Then
            expect_equal(diff_result, 1440)
          })

test_that("diffTime: diff in years",
          {
            # Given 
            date_1 <- as.Date("2017-01-03")
            date_2 <- as.Date("2017-01-02")
            units <- "years"
            
            # When
            diff_result <- diffTime(date_1, date_2, units = units)
            
            # Then
            expect_equal(diff_result, 1 / 365.25)
          })

test_that("diffTime: errors on wrong unit",
          {
            # Given 
            date_1 <- as.Date("2017-01-03")
            date_2 <- as.Date("2017-01-02")
            wrong_units <- "vyuiio"
            
            # When + Then
            expect_error(diffTime(date_1, date_2, units = wrong_units), "Sorry this unit hasn't been implemented yet")
          })

