context("test_setColAs.R")
requireNamespace("data.table")
verbose <- TRUE

## setColAsNumeric
#-----------------------
test_that("setColAsNumeric:", 
          {
            # Given
            dataSet <- data.table(char_col_1 = c("1", "2", "3"), 
                                  char_col_2 = c("4", "5", "6"))
            
            # When
            dataSet_transformerd <- setColAsNumeric(dataSet, cols = c("char_col_1", "char_col_2"), verbose = verbose)
            
            # Then
            expect_true(all(sapply(dataSet_transformerd, is.numeric)))
          })

## setColAsCharacter
#-------------------
test_that("setColAsCharacter: Set numCol and factorCol as character", 
          {
            # Given
            dataSet <- data.table(numCol = c(1, 2, 3), 
                                  factorCol = as.factor(c("a", "b", "c")), 
                                  charcol = c("1", "2", "a"))
            
            # When
            dataSet_transformed <- setColAsCharacter(dataSet, cols = "auto", verbose = verbose)
            
            # Then
            expect_true(all(sapply(dataSet_transformed, is.character)))
          })

## setColAsDate
#-----------------------
test_that("setColAsDate: transform using correct element if format list", 
          {
            # Given
            dataSet <- data.table(ID = 1:6, 
                                  date1 = c("2015-01-01", "2016-01-01", "2015-09-01", "2015-03-01", "2015-01-31", ""), 
                                  date2 = as.factor(c("2015_01_01", "2016_01_01", "2015_09_01", "2015_03_01", "2015_01_31", ""))
            )
            
            # When
            dataSet_transformed <- setColAsDate(dataSet, cols = c("date1", "date2"), format = list(date1 ="%Y-%m-%d", "%Y_%m_%d"), verbose = verbose)
            
            # Then
            expect_true(is.integer(dataSet_transformed$ID))
            expect_true(is.POSIXct(dataSet_transformed$date1))
            expect_true(is.POSIXct(dataSet_transformed$date2))
          })


test_that("setColAsDate: only transform columns it is told to transform even if they are factor", 
          {
            # Given
            dataSet <- data.table(ID = 1:6, 
                                  date1 = c("2015-01-01", "2016-01-01", "2015-09-01", "2015-03-01", "2015-01-31", ""), 
                                  date2 = as.factor(c("2015_01_01", "2016_01_01", "2015_09_01", "2015_03_01", "2015_01_31", ""))
            )
            
            # When
            dataSet_transformed <- setColAsDate(dataSet, cols = "date2", format = "%Y_%m_%d", verbose = verbose)
            
            # Then
            expect_true(is.integer(dataSet_transformed$ID))
            expect_true(is.character(dataSet_transformed$date1))
            expect_true(is.POSIXct(dataSet_transformed$date2))
          })

test_that("setColAsDate: transform one specific column without giving format", 
          {
            # Given
            dataSet <- data.table(ID = 1:6, 
                                  date1 = c("2015-01-01", "2016-01-01", "2015-09-01", "2015-03-01", "2015-01-31", ""), 
                                  date2 = as.factor(c("2015_01_01", "2016_01_01", "2015_09_01", "2015_03_01", "2015_01_31", ""))
            )
            
            # When
            dataSet_transformed <- setColAsDate(dataSet, cols = "date1", verbose = verbose)
            
            # Then
            expect_true(is.integer(dataSet_transformed$ID))
            expect_true(is.POSIXct(dataSet_transformed$date1))
            expect_true(is.factor(dataSet_transformed$date2))
          })

test_that("setColAsDate: raise warning, when column is not character", 
          {
            # Given
            dataSet <- data.table(ID = 1:6, 
                                  date1 = c("2015-01-01", "2016-01-01", "2015-09-01", "2015-03-01", "2015-01-31", ""))
            
            # When + Then
            expect_warning( setColAsDate(copy(dataSet), cols = "ID", verbose = verbose),  
                            "setColAsDate: I can't handle ID, please see documentation.")
          })

test_that("setColAsDate: raise warning, when column is character but tyope is not correct and doesn't change it", 
          {
            # Given
            dataSet <- data.table(ID = as.character(1:6), 
                                  date1 = c("2015-01-01", "2016-01-01", "2015-09-01", "2015-03-01", "2015-01-31", ""))
            store_id <- dataSet[["ID"]]
            
            # When + Then
            expect_warning(dataSet_transformed <- setColAsDate(copy(dataSet), cols = "ID", format = "%Y-%m-%d", verbose = verbose))
            expect_equal(store_id, dataSet_transformed[["ID"]])
            
          })

test_that("setColAsDate: don't transform a column that isn't a date and with no format provided", 
          {
            # Given
            dataSet <- data.table(ID = as.character(1:6))
            store_id <- dataSet[["ID"]]
            
            # When
            dataSet_transformed <- setColAsDate(copy(dataSet), cols = "ID", verbose = verbose)
            
            # Then
            expect_true(is.character(dataSet_transformed[["ID"]]))
            expect_equal(store_id, dataSet_transformed[["ID"]])
          })


test_that("setColAsDate: work even if format not used by parse_date_time", 
          {
            # Given
            dataSet <- data.table(time = c("10:01:55", "09:35:60"))
            
            # When
            dataSet_transformed <- setColAsDate(dataSet, cols = "time", format = "%H:%M:%S", verbose = verbose)
            # Then
            expect_true(is.POSIXct(dataSet_transformed[["time"]]))
          })

test_that("setColAsDate: with s time stamps", 
          {
            # Given
            dataSet <- data.table(time_stamp_s = c(1483225200, 1485990000, 1488495600))
            
            # When
            dataSet_transformed <- setColAsDate(dataSet, cols = "time_stamp_s", format = "s", verbose = verbose)
            # Then
            expect_true(is.POSIXct(dataSet_transformed[["time_stamp_s"]]))
          })

test_that("setColAsDate: with ms time stamps", 
          {
            # Given
            dataSet <- data.table(time_stamp_ms = c(1483225200000, 1485990000000, 1488495600000))
            
            # When
            dataSet_transformed <- setColAsDate(dataSet, cols = "time_stamp_ms", format = "ms", verbose = verbose)
            # Then
            expect_true(is.POSIXct(dataSet_transformed[["time_stamp_ms"]]))
          })


## is.format
# ------------
test_that("Private function: is.format: control errors",
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
test_that("setColAsFactor: Behave with default n_levels", 
          {
            # Given
            dataSet <- data.table(col = c("A", "B", "C"))
            
            # When
            result <- setColAsFactor(dataSet, cols = "col", verbose = verbose)
            
            # Then
            expect_true(is.factor(result[["col"]])) 
          })

test_that("setColAsFactor: behave with n_levels = -1", 
          {
            # Given
            dataSet <- data.table(col = c("A", "B", "C"))
            
            # When
            result <- setColAsFactor(dataSet, cols = "col", n_levels = -1, verbose = verbose)
            
            # Then
            expect_true(is.factor(result[["col"]])) 
            
          })
test_that("setColAsFactor: column is unchanged if number of values greatter than n_levels", 
          {
            # Given
            dataSet <- data.table(col = c("A", "B", "C"))
            
            # When
            result <- setColAsFactor(dataSet, cols = "col", n_levels = 2, verbose = verbose)
            
            # Then
            expect_false(is.factor(result[["col"]])) 
            expect_true(is.character(result[["col"]]))
          })
test_that("setColAsFactor:", 
          {
            # Given
            dataSet <- data.table(col = c("A", "B", "C"))
            
            # When + Then
            expect_error(setColAsFactor(dataSet, cols = "col", n_levels = "a", verbose = verbose), 
                         ": n_levels should be an integer.") #N_levels not integer
          })