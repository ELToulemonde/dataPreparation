

## setColAsNumeric
#-----------------------
dataSet <- data.table(charCol1 = c("1", "2", "3"), charCol2 = c("4", "5", "6"))
dataSet <- setColAsNumeric(dataSet, cols = c("charCol1", "charCol2"), verbose = FALSE)

test_that("setColAsNumeric:", 
          {
            expect_equal(all(sapply(dataSet, class) == "numeric"), TRUE)
          })

## setColAsCharacter
#-------------------

dataSet <- data.table(numCol = c(1, 2, 3), factorCol = as.factor(c("a", "b", "c")))
# Set numCol and factorCol as character
dataSet <- setColAsCharacter(dataSet, cols = c("numCol", "factorCol"))

test_that("setColAsCharacter:", 
          {
            expect_equal(all(sapply(dataSet, class) == c("character", "character")), TRUE)
          }
          )

## setColAsDate
#-----------------------


dataSet <- data.table(ID = 1:5, 
                  date1 = c("2015-01-01", "2016-01-01", "2015-09-01", "2015-03-01", "2015-01-31"), 
                  date2 = c("2015_01_01", "2016_01_01", "2015_09_01", "2015_03_01", "2015_01_31")
                  )
data_transformed <- setColAsDate(dataSet, cols = "date2", format = "%Y_%m_%d", verbose = FALSE)


test_that("setColAsDate:", 
          {
            expect_equal(lapply(data_transformed, class)$ID,  "integer")
            expect_equal(lapply(data_transformed, class)$date1, "character")
            expect_equal(all(lapply(data_transformed, class)$date2 == c(c("POSIXct", "POSIXt"))), TRUE)
          })

## setColAsFactorOrLogical
#---------------------------
data("messy_adult")
messy_adult <- setColAsFactorOrLogical(messy_adult, cols = c("mail", "education"))

test_that("setColAsFactorOrLogical:", 
          {
            expect_equal(class(messy_adult[["mail"]]),  "logical")
            expect_equal(class(messy_adult[["education"]]),  "factor")
          })