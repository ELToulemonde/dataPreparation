requireNamespace("data.table")
verbose <- TRUE
Sys.setlocale("LC_TIME", "C")
## findAndTransformDates
#-----------------------

dataSet <- data.table(ID = 1:5, 
                      date1 = c("2015-01-01", "2016-01-01", "2015-09-01", "2015-03-01", "2015-01-31"), 
                      date2 = c("2015_01_01", "2016_01_01", "2015_09_01", "2015_03_01", "2015_01_31"), 
                      date3 = c("2015_1_1", "2016_1_1", "2015_9_1", "2015_3_1", "2015_1_31"),
					  date4 = c("01-january-2015", "01-january-2016", "01-september-2015", "01-march-2015", "31-january-2015"),
                      hour1 = c("23:51",     "22:08",     "10:03",     "25:33",     "1:22")
)

data_transformed <- findAndTransformDates(dataSet, verbose =  verbose, n_test = 5)

test_that("findAndTransformDates:", 
          {
            expect_true(all(sapply(data_transformed, function(x)class(x)[1]) == c("integer", "POSIXct", "POSIXct", "POSIXct", "POSIXct", "POSIXct")))
          })

data(iris)
data_transformed <- findAndTransformDates(iris, verbose =  verbose, n_test = 5)
test_that("findAndTransformDates: no dates to find", 
          {
            expect_true(all(sapply(data_transformed, function(x)class(x)[1]) == c("numeric", "numeric", "numeric", "numeric", "factor")))
          })

## identifyDates
#---------------
dataSet <- data.table(ID = 1:5, 
                      date1 = c("2015-01-01", "2016-01-01", "2015-09-01", "2015-03-01", "2015-01-31"), 
                      date2 = c("2015_01_01", "2016_01_01", "2015_09_01", "2015_03_01", "2015_01_31"), 
                      hour1 = c("23:51",     "22:08",     "10:03",     "25:33",     "1:22")
)





test_that("private function identifyDates :",
          {
            expect_equal(length(identifyDates(dataSet, n_test = 5, verbose = verbose)), 2)
            expect_true(all(identifyDates(dataSet, n_test = 5, verbose = verbose)[[1]] == c( "date1", "date2", "hour1")))
            expect_true(all(identifyDates(dataSet, n_test = 5, verbose = verbose)[[2]] == c("%Y-%m-%d", "%Y_%m_%d", "%H:%M")))
          })

## identifyDatesFormats 
# ----------------
test_that("Private function: identifyDatesFormats ",
          {
            expect_error(identifyDatesFormats(dataSet[["ID"]]), "identifyDatesFormats: dataSet should be some characters")
            expect_equal(identifyDatesFormats(format(Sys.Date(), "%Y-%m-%d"), formats = c("%m-%d-%Y", "%Y-%m-%d")), "%Y-%m-%d")
          })

## identifyTimeStampsFormats 
# --------------------------
test_that("private function: identifyTimeStampsFormats ",
          {
            expect_equal(identifyTimeStampsFormats(1352068320), "s")
            expect_equal(identifyTimeStampsFormats(1352068320000), "ms")
            expect_null(identifyTimeStampsFormats(12345))
            expect_error(identifyTimeStampsFormats("ad"), ": dataSet should be some numerics.")
          })






## dateFormatUnifier
#-------------------
dataSet <- data.table( column1 = as.Date("2016-01-01"), column2 = as.POSIXct("2017-01-01") )


test_that("dateFormatUnifier:", 
          {
            expect_true(all(sapply(dateFormatUnifier(dataSet, format = "Date"), class) == c("Date", "Date")))
            expect_error(dateFormatUnifier(dataSet, format = "adaedeaz"), "dateFormatUnifier: only format: Date, POSXIct, POSIXlt are implemented. You gave:")
          })

## is.date
#-----------------

dateAuFormatDate <- as.Date("2016-01-01")
dateAuFormatPOSIXct <- as.POSIXct("2016-01-01")

test_that("is.date:", 
          {
            expect_true(is.date(dateAuFormatDate))
            expect_true(is.date(dateAuFormatPOSIXct))
          })



## getPossibleDatesFormats
#-------------------------

test_that("getPossibleDatesFormats:", 
          {
            expect_is(getPossibleDatesFormats(), "character")
          })


## formatForparse_date_time
#--------------------------
formatForparse_date_time()

