context("test_datesManipulation.R")
requireNamespace("data.table")
verbose <- TRUE
Sys.setlocale("LC_TIME", "C")
## Build a testing set
# --------------------
# Please make sure to send a copy of it to not transform it
dataSet <- data.table(ID = 1:5, 
                      date1 = c("2015-01-01", "2016-01-01", "2015-09-01", "2015-03-01", "2015-01-31"), 
                      date2 = as.factor(c("2015_01_01", "2016_01_01", "2015_09_01", "2015_03_01", "2015_01_31")), 
                      date3 = c("2015_1_1", "2016_1_1", "2015_9_1", "2015_3_1", "2015_1_31"),
                      date4 = c("01-january-2015", "01-january-2016", "01-september-2015", "01-march-2015", "31-january-2015"),
                      hour1 = c("23:51",     "22:08",     "10:03",     "25:33",     "01:22")
)


## findAndTransformDates
#-----------------------
test_that("findAndTransformDates: functionnal test : find 4 dates in reference set messy_adult", 
          {
            # Given
            data("messy_adult")
            
            # When
            messy_adult_transformed <- findAndTransformDates(copy(messy_adult), verbose = verbose)
            
            # Then
            expect_equal(sum(sapply(messy_adult_transformed, is.POSIXct)), 4)
          })

test_that("findAndTransformDates: functionnal test : find 0 dates in reference set iris", 
          {
            # Given
            data("iris")
            
            # When
            iris_transformed <- findAndTransformDates(iris, verbose = verbose, n_test = 5)
            
            # Then
            expect_false(any(sapply(iris_transformed, is.POSIXct))) 
          })

test_that("findAndTransformDates: check exceptions : ambiguities not in expected values", 
          {
            # Given
            data("messy_adult")
            wrong_ambiguities <- 1
            
            # When + Then
            expect_error(findAndTransformDates(messy_adult, verbose = verbose, ambiguities = wrong_ambiguities))
          })

## identifyDates
#---------------
test_that("private function: identifyDates: control result",
          {
            # Given
            dataSet <- data.table(ID = 1:5, 
                                  date1 = c("2015-01-01", "2016-01-01", "2015-09-01", "2015-03-01", "2015-01-31"), 
                                  date2 = as.factor(c("2015_01_01", "2016_01_01", "2015_09_01", "2015_03_01", "2015_01_31")), 
                                  date3 = c("2015_1_1", "2016_1_1", "2015_9_1", "2015_3_1", "2015_1_31"),
                                  date4 = c("01-january-2015", "01-january-2016", "01-september-2015", "01-march-2015", "31-january-2015"),
                                  hour1 = c("23:51",     "22:08",     "10:03",     "25:33",     "01:22")
            )
            expected_dates_cols <- list(date1 = "%Y-%m-%d", date2 = "%Y_%m_%d", date3 = "%Y_%m_%d", date4 = "%d-%B-%Y")
            
            # When
            dates_found <- identifyDates(dataSet, n_test = 5, verbose = verbose)
            
            # Then
            expect_identical(dates_found, expected_dates_cols)
          })

test_that("private function: identifyDates: do nothing if told so",
          {
            # Given
            data("messy_adult")
            expected_dates_cols <- list()
            
            # When
            dates_found <- identifyDates(messy_adult, cols = list(), n_test = 5, verbose = verbose)
            
            # Then
            expect_identical(dates_found, expected_dates_cols)
          })

test_that("private function: identifyDates: ambiguities: with option warn, find nothing but print warning.", 
          {
            # Given
            dataSet <- data.table(date_col = c("2018-01-01", "2018-01-02", "2018-01-31"))
            
            # When + Then
            expect_output(dates_found <- identifyDates(dataSet, ambiguities = "WARN", n_test = 2, verbose = TRUE), 
                          " seems to be a date but there is an ambiguity in the format. ") 
            expect_equal(dates_found, list()) 
          })

test_that("private function: identifyDates: ambiguities, SOLVE find ambiguity and solve it.", 
          {
            # Given
            dataSet <- data.table(date_col = c("2018-01-01", "2018-01-02", "2018-01-31"))
            
            # When
            dates_found <- identifyDates(dataSet, n_test = 2, ambiguities = "SOLVE", verbose = verbose)
            
            # Then
            expect_equal(length(dates_found), 1) 
            expect_equal(names(dates_found), "date_col") 
            expect_equal(dates_found[["date_col"]], "%Y-%m-%d")
          })


## identifyDatesFormats 
# ---------------------
test_that("private function: identifyDatesFormaats: throw error on unexpected format",
          {
            # Given
            dataSet <- c(TRUE, FALSE)
            
            # When + Then
            expect_error(identifyDatesFormats(dataSet), 
                         ": dataSet should be some characters, numerics or factor of character.")
          })

test_that("Private function: identifyDatesFormats: find correct format in 2 formats ",
          {
            # Given 
            searched_format <- "%Y-%m-%d"
            another_format <- "%m-%d-%Y"
            dataSet <- format(Sys.Date(), searched_format)
            
            # When
            format_found <- identifyDatesFormats(dataSet, c(another_format, searched_format))
            
            # Then
            expect_equal(format_found, searched_format)
          })

# Future test for auto change LC_TIME		  
# Sys.setlocale("LC_TIME", "French_France.1252")
# test_that("Private function: identifyDatesFormats control auto changing time lc",
          # {
            # expect_equal(identifyDatesFormats("2018-May-05", formats = c("%Y-%b-%d")), "%Y-%b-%d")
          # })
# Sys.setlocale("LC_TIME", "C")
		  
		  
		  
		  
#test_that("Private function: identifyDatesFormats control Accepting time zone",
#          {
#            expect_equal(identifyDatesFormats("Tue, 26 Jun 2018 17:58:10 +0200", formats = c("%a, %d %b %Y %H:%M:%S %z")), "%a, %d %b %Y %H:%M:%S %z")
#          })
## identifyTimeStampsFormats 
# --------------------------
test_that("private function: identifyTimeStampsFormats identified time stamps i,n second",
          {
            # Given
            time_stamps_in_s <- 1352068320
            
            # When
            identified_format <- identifyTimeStampsFormats(time_stamps_in_s)
            
            # Then
            expect_equal(identified_format, "s")
          })

test_that("private function: identifyTimeStampsFormats identified time stamps in ms",
          {
            # Given
            time_stamps_in_ms <- 1352068320000
            
            # When
            identified_format <- identifyTimeStampsFormats(time_stamps_in_ms)
            
            # Then
            expect_equal(identified_format, "ms")
          })

test_that("private function: identifyTimeStampsFormats: find nothing in random digit",
          {
            # Given
            digit_that_is_not_time_stamp <- 12345
            
            # When
            identified_format <- identifyTimeStampsFormats(digit_that_is_not_time_stamp)
            
            # Then
            expect_null(identified_format)
            
          })

test_that("private function: identifyTimeStampsFormats: throw errors on non numerics",
          {
            # Given
            a_non_numeric <- "a"
            
            # When + Then
            expect_error(identifyTimeStampsFormats(a_non_numeric), 
                         ": dataSet should be some numerics.")
          })


## control_date_conversion
# ------------------------
test_that("private function: control_date_conversion ",
          {
            expect_true(control_date_conversion( c("2017-01-02", "01-September-2017"), c("2017-1-2", "1-september-2017")))
            expect_true(control_date_conversion( c("2017-01-02", "01-September-2017"), c("2017-01-02", "01-september-2017")))
            expect_true(control_date_conversion( c("2017-01-02", "01-September-2017"), c("2017-01-02", "1-september-2017")))
          })



## dateFormatUnifier
#-------------------
dataSet <- data.table( column1 = as.Date("2016-01-01"), column2 = as.POSIXct("2017-01-01") )


test_that("dateFormatUnifier:", 
          {
            expect_true(all(sapply(dateFormatUnifier(dataSet, format = "Date"), is.Date)))
            expect_error(dateFormatUnifier(dataSet, format = "adaedeaz"), "dateFormatUnifier: only format: Date, POSXIct, POSIXlt are implemented. You gave:")
          })

## is.date
#---------
dateAsDate <- as.Date("2016-01-01")
dateAsPOSIXct <- as.POSIXct("2016-01-01")

test_that("is.date:", 
          {
            expect_true(is.date(dateAsDate))
            expect_true(is.date(dateAsPOSIXct))
          })



## getPossibleDatesFormats
#-------------------------
test_that("getPossibleDatesFormats:", 
          {
            expect_is(getPossibleDatesFormats(), "character")
          })


## formatForparse_date_time
#--------------------------
test_that("Private function: formatForparse_date_time",
         {
           expect_true(is.vector(formatForparse_date_time()))
         })


