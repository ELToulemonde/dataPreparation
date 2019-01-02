context("test_genericFunctions.R")
requireNamespace("data.table")
verbose <- TRUE
Sys.setlocale("LC_TIME", "C")
## findNFirstNonNull
#-------------------
test_that("findNFirstNonNull: given 1 to 50, and asking for 5 value should return 1 to 5", 
          {
            # Given 
            numerical_values = 1:50
            n_wanted_values = 5
            expected_results = 1:5
            
            # When
            result = findNFirstNonNull(numerical_values, n_wanted_values)
            
            # Then
            expect_identical(result, expected_results)
          })

test_that("findNFirstNonNull: Given 1 to 50, asking for 10 element should return 1 to 10", 
          {
            # Given 
            numerical_values = 1:50
            n_wanted_values = 10
            expected_results = 1:10
            
            # When
            result = findNFirstNonNull(numerical_values, n_wanted_values)
            
            # Then
            expect_identical(result, expected_results)
          })

test_that("findNFirstNonNull: Given a NA and then 1 to 50, asking for 10 element should return 1 to 10", 
          {
            # Given 
            numerical_values = c(NA, 1:50)
            n_wanted_values = 10
            expected_results = 1:10
            
            # When
            result = findNFirstNonNull(numerical_values, n_wanted_values)
            
            # Then
            expect_identical(result, expected_results)
          })

test_that("findNFirstNonNull: Given a empty string and then the alphabet, when asking for 3 letters, should return first 3 letters", 
          {
            # Given 
            letters_values = c("", LETTERS)
            n_wanted_values = 3
            expected_results = c("A", "B", "C")
            
            # When
            result = findNFirstNonNull(letters_values, n_wanted_values)
            
            # Then
            expect_identical(result, expected_results)
          })


test_that("findNFirstNonNull: Given two not NA values, and asking for more, should return result of len two", 
          {
            # Given 
            two_values_and_to_nas = c("A", "B", NA, NA)
            n_wanted_values = 3
            expected_results = 2
            
            # When
            result = findNFirstNonNull(two_values_and_to_nas, n_wanted_values)
            
            # Then 
            expect_equal(length(result), expected_results)
          })

## checkAndReturnDataTable
#-------------------------
test_that("checkAndReturnDataTable: Given a data.table, Should return a data.table",
          {
            # Given
            data("adult")
            setDT(adult)
            
            # When
            result <- checkAndReturnDataTable(adult)
            
            # Then
            expect_true(is.data.table(result))
          })

test_that("checkAndReturnDataTable: Given a data.frame, Should return a data.table",
          {
            # Given
            data("adult")
            adult <- as.data.frame(adult)
            
            # When
            result <- checkAndReturnDataTable(adult)
            
            # Then
            expect_true(is.data.table(result))
          })

test_that("checkAndReturnDataTable: Given a matrix, Should return a data.table",
          {
            # Given
            data("adult")
            adult <- as.matrix(adult)
            
            # When
            result <- checkAndReturnDataTable(adult)
            
            # Then
            expect_true(is.data.table(result))
          })

test_that("checkAndReturnDataTable: control errors", 
          {
            expect_error(checkAndReturnDataTable("a"))
            expect_error(checkAndReturnDataTable(1))
            expect_error(checkAndReturnDataTable(list(1,2)))
            expect_error(checkAndReturnDataTable(data.table()), "should have at least have 1 line")
            expect_error(checkAndReturnDataTable(data.frame(row.names = c(1,2))), "should have at least have 1 column")
          })



## is.verbose
# -----------
test_that("is.verbose: control input",
          {
            expect_error(is.verbose("a"))
          })

## is.verbose_levels
# ------------------
test_that("is.verbose_levels: control input",
          {
            expect_error(is.verbose_level("a"))
            expect_error(is.verbose_level(3, max_level = 2))
          })

## is.share
# ---------
test_that("is.share: control input",
          {
            expect_error(is.share("a"))
            expect_error(is.share(3))
          })

## is.col
#--------
dataSet <- data.table(a = "1")
test_that("is.col: ",
          {
            expect_null(is.col(dataSet, cols = "a"))
            expect_error(is.col(dataSet, cols = "b"), ". should be column of dataSet")
            expect_error(is.col(1, cols = "b"), "is.col: dataSet should be a data.table, data.frame or matrix")
          })

## real_cols 
# ----------
data("adult")
date_set <- data.frame(date_col = as.Date( c("2015-01-01", "2016-01-01", "2015-09-01", "2015-03-01", "2015-01-31")))
test_that("real_cols:",
          {
            expect_equal(length(real_cols(adult, c("education", "asucgzr"))), 1)
            expect_equal(real_cols(adult, cols = "auto"), colnames(adult))
            expect_null(real_cols(adult, cols = NULL))
            expect_null(real_cols(adult, cols = character(0)))
            expect_identical(real_cols(adult, cols = "auto", types = c("numeric", "integer")), c("age", "fnlwgt", "education_num", "capital_gain", "capital_loss", 
                                                                                                 "hr_per_week"))
            expect_identical(real_cols(adult, cols = c("education", "age"), types = c("numeric", "integer")), "age")
            expect_identical(real_cols(adult, cols = c("education", "age"), types = c("numeric")), "age")
            expect_identical(real_cols(date_set, cols = c("date_col"), types = c("date")), c("date_col"))
          })

## getPossibleSeparators
#------------------------
test_that("getPossibleSeparators:", 
          {
            expect_true(is.vector(getPossibleSeparators()))
          })


## printl
#--------
test_that("printl:", 
          {
            expect_output(printl("printl", " is a private function ", " easier to use than print"))
          })

## controlNumberOfRows
#--------------------
dataSet <- data.table(col1 = c(1, 2, 3))
test_that("control_nb_rows:", 
          {
            expect_equal(control_nb_rows(dataSet, 1), 1)
            expect_warning(control_nb_rows(dataSet, 10), "You want to check more rows than there are in dataSet, I set nb_rows to 3")
            expect_warning(control_nb_rows(dataSet, 0), "You want to check at least a few rows than there are in dataSet, I set nb_rows to 3")
            expect_error(control_nb_rows(dataSet, "a"), " should be a numeric.")
          })


## is.agg_function
# -----------------
b = 1
attach(list(b=b)) # A bit ugly, but i don't know how to do it another way.
test_that("is.agg_function:", 
          {
            expect_error(result <- is.agg_function(list(sum, "sum")),  "functions should be a list of names")
            expect_warning(result <- is.agg_function(list("sum", "a")), " doesn't exist, it wont be used.")
            expect_warning(result <- is.agg_function(list("sum", "b")), " is not a function, it wont be used.")
            expect_equal(length(result), 1)
            expect_warning(is.agg_function("sqrt"), " sqrt is not an aggregation function")
          })

## function.maker
# ---------------
test_that("function.maker: ",
          {
            expect_true(is.function(function.maker(function(x){sum(x, na.rm = TRUE)}, type = "numeric")))
            expect_true(is.function(function.maker(1, type = "numeric")))
            expect_true(is.function(function.maker("a", type = "character")))
            expect_true(is.function(function.maker(TRUE, type = "logical")))
          })

test_that("function.maker: warning not handling na",
          {
            expect_warning(function.maker(max, type = "numeric"))
            expect_warning(function.maker(max, type = "character"))
            expect_warning(function.maker(function(...){sum(...) / length(list(...)) > 0.5}, type = "logical"))
          })

test_that("function.maker: stop not aggregation function",
          {
            expect_error(function.maker(sqrt, type = "numeric"))
            expect_error(function.maker(function(x)paste0(x, "aaa"), type = "character"))
            expect_error(function.maker(function(x){!x}, type = "logical"))
          })

test_that("function.maker: error wrong type",
          {
            expect_error(function.maker("a", type = "numeric"))
            expect_error(function.maker(1, type = "character"))
            expect_error(function.maker("a", type = "logical"))
            expect_error(function.maker(dataSet, type = "logical"), ": is in a shape that isn't handled, please provide constant or aggregation function.")
          })


## make_new_col_name
# -------------------
test_that("function.maker: error wrong type",
          {
            expect_equal(make_new_col_name("a", c("a", "b")), "a1")
            expect_equal(make_new_col_name("a", c("a", "a1")), "a2")
            expect_equal(make_new_col_name("c", c("a", "b")), "c")
            expect_error(make_new_col_name(1, c("a", "b")), "new_col and col_names should be character.")
          })


## build_name_separator
# --------------------
test_that("build_name_separator: ",
          {
            expect_equal(build_name_separator(list()), ".")
            expect_equal(build_name_separator(list(name_separator = ",")), ",")
            expect_error(build_name_separator(list(name_separator = 1)), "name_separator should be a character.")
            expect_error(build_name_separator(list(name_separator = c(".", ";"))), "name_separator should be a character.")
            
          })

## build_factor_date_type
# -----------------------
test_that("build_factor_date_type: ",
          {
            expect_equal(build_factor_date_type(list()), "yearmonth")
            expect_equal(build_factor_date_type(list(factor_date_type = "yearmonth")), "yearmonth")
            expect_error(build_factor_date_type(list(factor_date_type = 1)), "factor_date_type should be a character.")
            expect_error(build_factor_date_type(list(factor_date_type = c(".", ";"))), "factor_date_type should be a character.")
            
          })
