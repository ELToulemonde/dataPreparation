requireNamespace("data.table")
verbose = TRUE
## setColAsNumeric
#-----------------------
dataSet <- data.table(charCol1 = c("1", "2", "3"), charCol2 = c("4", "5", "6"))
dataSet <- setColAsNumeric(dataSet, cols = c("charCol1", "charCol2"), verbose = verbose)

test_that("setColAsNumeric:", 
          {
            expect_equal(all(sapply(dataSet, class) == "numeric"), TRUE)
          })

dataSet <- data.table(date_col = seq(as.Date("2017-01-01"), as.Date("2017-12-31"), by = "day"))

test_that("setColAsNumeric:", 
          {
            expect_warning(setColAsNumeric(dataSet, cols = c("date_col"), verbose = verbose), "isn't a character a numeric or an integer, i do nothing")
          })
		  
## setColAsCharacter
#-------------------

dataSet <- data.table(numCol = c(1, 2, 3), factorCol = as.factor(c("a", "b", "c")), charcol = c("1", "2", "a"))
# Set numCol and factorCol as character
dataSet <- setColAsCharacter(dataSet, cols = c("numCol", "factorCol", "charcol"), verbose = verbose)

test_that("setColAsCharacter:", 
          {
            expect_equal(all(sapply(dataSet, class) == c("character", "character", "character")), TRUE)
          }
          )

## setColAsDate
#-----------------------


dataSet <- data.table(ID = 1:5, 
                  date1 = c("2015-01-01", "2016-01-01", "2015-09-01", "2015-03-01", "2015-01-31"), 
                  date2 = c("2015_01_01", "2016_01_01", "2015_09_01", "2015_03_01", "2015_01_31")
                  )
dataSet <- setColAsDate(dataSet, cols = "date2", format = "%Y_%m_%d", verbose = verbose)


test_that("setColAsDate:", 
          {
            expect_equal(lapply(dataSet, class)$ID,  "integer")
            expect_equal(lapply(dataSet, class)$date1, "character")
            expect_equal(all(lapply(dataSet, class)$date2 == c(c("POSIXct", "POSIXt"))), TRUE)
          })
		  
dataSet <- setColAsDate(dataSet, cols = "date1", verbose = verbose)
test_that("setColAsDate: without giving format", 
          {
            expect_equal(lapply(dataSet, class)$ID,  "integer")
            expect_equal(all(lapply(dataSet, class)$date2 == c(c("POSIXct", "POSIXt"))), TRUE)
			}
			)

# Try to transform ID
test_that("setColAsDate: raise warning, when column is not character", 
          {
            expect_warning( setColAsDate(dataSet, cols = "ID", verbose = verbose),  " isn't a character, i do nothing")
			}
			)
			
			
# set id as character, and try to transform it
dataSet$ID = as.character(dataSet$ID)
dataSet <- setColAsDate(dataSet, cols = "ID", verbose = verbose)
test_that("setColAsDate: don't transform a column that isn't a date", 
          {
            expect_equal(lapply(dataSet, class)$ID,  "character")
			}
			)

			
dataSet <- setColAsDate(dataSet, cols = "ID", format = "%Y-%m-%d", verbose = verbose)
test_that("setColAsDate: don't transform a column that isn't a date even if format is forced", 
          {
            expect_equal(lapply(dataSet, class)$ID,  "character")
			}
			)
			
			
## setColAsFactorOrLogical
#---------------------------
data("messy_adult")
messy_adult <- setColAsFactorOrLogical(messy_adult, cols = c("mail", "education"), verbose = verbose)

test_that("setColAsFactorOrLogical:", 
          {
            expect_equal(class(messy_adult[["mail"]]),  "logical")
            expect_equal(class(messy_adult[["education"]]),  "factor")
			expect_error(setColAsFactorOrLogical(messy_adult, cols = c("mail", "education"), n_levels = "a", verbose = verbose))
          })