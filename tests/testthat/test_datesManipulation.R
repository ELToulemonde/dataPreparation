## findAndTransformDates
#-----------------------

dataSet <- data.table(ID = 1:5, 
                  date1 = c("2015-01-01", "2016-01-01", "2015-09-01", "2015-03-01", "2015-01-31"), 
                  date2 = c("2015_01_01", "2016_01_01", "2015_09_01", "2015_03_01", "2015_01_31"), 
                  hour1 = c("23:51",     "22:08",     "10:03",     "25:33",     "1:22")
                  )

data_transformed <- findAndTransformDates(dataSet, verbose =  TRUE, n_test = 5)

test_that("findAndTransformDates:", 
          {
            expect_equal(all(sapply(data_transformed, function(x)class(x)[1]) == c("integer", "POSIXct", "POSIXct", "character")), TRUE)
          })

## identifyDates
#---------------

## identifyFormats
#-----------------


## dateDiffs
#-----------

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