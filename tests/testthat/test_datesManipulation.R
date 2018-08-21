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
data("messy_adult")
data(iris)
test_that("findAndTransformDates: ", 
          {
            # Control that identify and transform something
            expect_equal(sum(sapply(findAndTransformDates(copy(messy_adult), verbose = verbose), is.POSIXct)), 4)
            # Doesn't nothing if there isn't anything to do
            expect_false(any(sapply(findAndTransformDates(iris, verbose = verbose, n_test = 5), is.POSIXct))) 
          })

test_that("findAndTransformDates: check exceptions", 
          {
            expect_error(result1 <- findAndTransformDates(copy(messy_adult), verbose = verbose, ambiguities = 1))
          })



## identifyDates
#---------------
test_that("private function: identifyDates: control result",
          {
            expect_identical(identifyDates(dataSet, n_test = 5, verbose = verbose), 
                             list(date1 = "%Y-%m-%d", date2 = "%Y_%m_%d", date3 = "%Y_%m_%d", date4 = "%d-%B-%Y")
                             )
			expect_identical(identifyDates(dataSet, cols = list(), n_test = 5, verbose = verbose), list()) # Control that if told to do nothing, do nothing.
          })

data("messy_adult")
messy_adult <- messy_adult[1:500, c("date1"), with = FALSE] # To check ambiguities one col is enough
messy_adult$date1 = sort(messy_adult$date1, na.last = TRUE) # Add an ambiguity
test_that("private function: identifyDates: ambiguities", 
          {
            expect_output(result <- identifyDates(copy(messy_adult), ambiguities = "WARN", verbose = verbose), " seems to be a date but there is an ambiguity in the format. ") 
            expect_null(result$formats) # Nothing found
            expect_equal(length(identifyDates(copy(messy_adult), ambiguities = "SOLVE", verbose = verbose)), 1) # Solving ambiguities add a format
          })

# Ambiguity in factor
messy_adult$date1 = as.factor(messy_adult$date1)
test_that("private function: identifyDates: ambiguities", 
          {
            expect_equal(length(identifyDates(copy(messy_adult), ambiguities = "SOLVE", verbose = verbose)), 1) # Solving ambiguities add a format
          })

## identifyDatesFormats 
# ---------------------
test_that("Private function: identifyDatesFormats ",
          {
            expect_error(identifyDatesFormats(c(TRUE, FALSE)), ": dataSet should be some characters, numerics or factor of character.")
            expect_equal(identifyDatesFormats(format(Sys.Date(), "%Y-%m-%d"), formats = c("%m-%d-%Y", "%Y-%m-%d")), "%Y-%m-%d")
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
test_that("private function: identifyTimeStampsFormats ",
          {
            expect_equal(identifyTimeStampsFormats(1352068320), "s")
            expect_equal(identifyTimeStampsFormats(1352068320000), "ms")
            expect_null(identifyTimeStampsFormats(12345))
            expect_error(identifyTimeStampsFormats("ad"), ": dataSet should be some numerics.")
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


