requireNamespace("data.table")
## findAndTransformDates
#-----------------------

dataSet <- data.table(ID = 1:5, 
                  date1 = c("2015-01-01", "2016-01-01", "2015-09-01", "2015-03-01", "2015-01-31"), 
                  date2 = c("2015_01_01", "2016_01_01", "2015_09_01", "2015_03_01", "2015_01_31"), 
                  hour1 = c("23:51",     "22:08",     "10:03",     "25:33",     "1:22")
                  )

data_transformed <- findAndTransformDates(dataSet, verbose =  FALSE, n_test = 5)

test_that("findAndTransformDates:", 
          {
            expect_equal(all(sapply(data_transformed, function(x)class(x)[1]) == c("integer", "POSIXct", "POSIXct", "character")), TRUE)
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
            expect_equal(length(identifyDates(dataSet, n_test = 5, verbose =FALSE)), 2)
            expect_equal(all(identifyDates(dataSet, n_test = 5, verbose =FALSE)[[1]] == c( "date1", "date2")), TRUE)
            expect_equal(all(identifyDates(dataSet, n_test = 5, verbose =FALSE)[[2]] == c("%Y-%m-%d", "%Y_%m_%d")), TRUE)
          })



## dateDiffs
#-----------
dataSet <- data.table(ID = 1:100, 
                      date1 = seq(from = as.Date("2010-01-01"), 
                                  to = as.Date("2015-01-01"), 
                                  length.out = 100), 
                      date2 = seq(from = as.Date("1910-01-01"), 
                                  to = as.Date("2000-01-01"), 
                                  length.out = 100)
)

# Now let's compute
dataSet <- diffDates(dataSet, analysisDate = as.Date("2016-11-14"))

test_that("dateDiffs: ",
          {
            expect_equal(ncol(dataSet), 6)
            expect_equal(sum(is.na(dataSet)), 0)
          })

## diffTime
#----------


## dateFormatUnifier
#-------------------
dataSet <- data.table( column1 = as.Date("2016-01-01"), column2 = as.POSIXct("2017-01-01") )
dataSet <- dateFormatUnifier(dataSet, format = "Date")




test_that("dateFormatUnifier:", 
          {
            expect_equal(all(sapply(dataSet, class) == c("Date", "Date")), TRUE)
          })

## is.date
#-----------------

dateAuFormatDate <- as.Date("2016-01-01")
dateAuFormatPOSIXct <- as.POSIXct("2016-01-01")

test_that("is.date:", 
          {
            expect_equal(is.date(dateAuFormatDate), TRUE)
            expect_equal(is.date(dateAuFormatPOSIXct), TRUE)
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