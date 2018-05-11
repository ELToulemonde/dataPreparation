context("test_setColAs.R")
requireNamespace("data.table")
verbose <- TRUE
## setColAsNumeric
#-----------------------
dataSet <- data.table(charCol1 = c("1", "2", "3"), charCol2 = c("4", "5", "6"))

test_that("setColAsNumeric:", 
          {
            expect_true(all(sapply(setColAsNumeric(dataSet, cols = c("charCol1", "charCol2"), verbose = verbose), is.numeric)))
          })

#dataSet <- data.table(date_col = seq(as.Date("2017-01-01"), as.Date("2017-12-31"), by = "day"))


## setColAsCharacter
#-------------------
dataSet <- data.table(numCol = c(1, 2, 3), factorCol = as.factor(c("a", "b", "c")), charcol = c("1", "2", "a"))
# Set numCol and factorCol as character
test_that("setColAsCharacter:", 
          {
            expect_true(all(sapply(setColAsCharacter(dataSet, cols = "auto", verbose = verbose), is.character)))
          })

## setColAsDate
#-----------------------
dataSet <- data.table(ID = 1:6, 
                      date1 = c("2015-01-01", "2016-01-01", "2015-09-01", "2015-03-01", "2015-01-31", ""), 
                      date2 = as.factor(c("2015_01_01", "2016_01_01", "2015_09_01", "2015_03_01", "2015_01_31", ""))
                      )

# Trnasform one specific column and even if it is a factor
result <- setColAsDate(copy(dataSet), cols = "date2", format = "%Y_%m_%d", verbose = verbose)
test_that("setColAsDate:", 
          {
            expect_true(is.integer(result$ID))
            expect_true(is.character(result$date1))
            expect_true(is.POSIXct(result$date2))
          })


# Trnasform one specific column and even if it is a factor
result <- setColAsDate(copy(dataSet), cols = "date2", format = list("%Y_%m_%d"), verbose = verbose)
test_that("setColAsDate:", 
          {
            expect_true(is.integer(result$ID))
            expect_true(is.character(result$date1))
            expect_true(is.POSIXct(result$date2))
          })

# Trnasform one specific column without giving format
result <- setColAsDate(copy(dataSet), cols = "date1", verbose = verbose)
test_that("setColAsDate: without giving format", 
          {
            expect_true(is.integer(result$ID))
            expect_true(is.POSIXct(result$date1))
            expect_true(is.factor(result$date2))
          })

# Try to transform ID
test_that("setColAsDate: raise warning, when column is not character", 
          {
            expect_warning( setColAsDate(copy(dataSet), cols = "ID", verbose = verbose),  "setColAsDate: I can't handle ID, please see documentation.")
          })


# set id as character, and try to transform it
dataSet$ID <- as.character(dataSet$ID)
test_that("setColAsDate: don't transform a column that isn't a date", 
          {
            expect_true(is.character(setColAsDate(copy(dataSet), cols = "ID", verbose = verbose)$ID))
            expect_warning(result <- setColAsDate(copy(dataSet), cols = "ID", format = "%Y-%m-%d", verbose = verbose)) #don't transform a column that isn't a date even if format is forced
            expect_true(is.character(result$ID))
          })

dataSet <- data.table(time = c("10:01:55", "09:35:60"))
test_that("setColAsDate: format not used by parse_date_time", 
          {
            expect_true(is.POSIXct(setColAsDate(dataSet, cols = "time", format = "%H:%M:%S", verbose = verbose)$time))
          })

# test on time_stamp
dataSet <- data.frame( time_stamp_s = c(1483225200, 1485990000, 1488495600),
                       time_stamp_ms = c(1483225200000, 1485990000000, 1488495600000))

dataSet <- setColAsDate(dataSet, cols = "time_stamp_s", format = "s", verbose = verbose)
dataSet <- setColAsDate(dataSet, cols = "time_stamp_ms", format = "ms", verbose = verbose)
test_that("setColAsDate: check time_stamp", 
          {
            expect_true(is.POSIXct(dataSet$time_stamp_s))
            expect_true(is.POSIXct(dataSet$time_stamp_ms))
          })


## is.format
# ------------
test_that("Private function: is.format",
          {
            expect_error(is.format(1), ": format should either be list of formats or a character.")
            expect_error(is.format(list(1)), ": format should either be list of character or a character.")
          })


## parse_date_cols
# ----------------
test_that("Private function: parse_date_cols",
          {
            expect_identical(parse_date_cols(cols = NULL, format = list(a ="1", b = "2")), c("a","b"))
            expect_identical(parse_date_cols(cols = c("a", "b"), format = list(a ="1", b = "2")), c("a","b"))
            expect_error(parse_date_cols(cols = c("c", "d"), format = list(a ="1", b = "2", e ="3")), "you provide cols and format but I'm not able to match them, please feed format as named list.")
          })


## setColAsFactor
#----------------
data("messy_adult")
messy_adult <- setColAsCharacter(messy_adult, cols = "education", verbose = FALSE) # Unfactor education

test_that("setColAsFactor:", 
          {
            expect_true(is.factor(setColAsFactor(copy(messy_adult), cols = "education", verbose = verbose)[["education"]])) # Behave with default n_levels
            expect_true(is.factor(setColAsFactor(copy(messy_adult), cols = "education", n_levels = -1, verbose = verbose)[["education"]])) # behave with n_levels = -1
            expect_true(is.character(setColAsFactor(copy(messy_adult), cols = "education", n_levels = 2, verbose = verbose)[["education"]])) # Unchanged if too many factors
            expect_error(setColAsFactor(copy(messy_adult), cols = "education", n_levels = "a", verbose = verbose), ": n_levels should be an integer.") #N_levels not integer
          })
