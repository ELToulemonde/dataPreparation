context("test_genericFunctions.R")
requireNamespace("data.table")
verbose <- TRUE
Sys.setlocale("LC_TIME", "C")
## findNFirstNonNull
#-------------------
test_that("findNFirstNonNull: given 1 to 50, and asking for 5 value should return 1 to 5", 
          {
            # Given 
            numerical_values <- 1:50
            n_wanted_values <- 5
            expected_results <- 1:5
            
            # When
            result <- findNFirstNonNull(numerical_values, n_wanted_values)
            
            # Then
            expect_identical(result, expected_results)
          })

test_that("findNFirstNonNull: Given 1 to 50, asking for 10 element should return 1 to 10", 
          {
            # Given 
            numerical_values <- 1:50
            n_wanted_values <- 10
            expected_results <- 1:10
            
            # When
            result <- findNFirstNonNull(numerical_values, n_wanted_values)
            
            # Then
            expect_identical(result, expected_results)
          })

test_that("findNFirstNonNull: Given a NA and then 1 to 50, asking for 10 element should return 1 to 10", 
          {
            # Given 
            numerical_values <- c(NA, 1:50)
            n_wanted_values <- 10
            expected_results <- 1:10
            
            # When
            result <- findNFirstNonNull(numerical_values, n_wanted_values)
            
            # Then
            expect_identical(result, expected_results)
          })

test_that("findNFirstNonNull: Given a empty string and then the alphabet, when asking for 3 letters, should return first 3 letters", 
          {
            # Given 
            letters_values <- c("", LETTERS)
            n_wanted_values <- 3
            expected_results <- c("A", "B", "C")
            
            # When
            result <- findNFirstNonNull(letters_values, n_wanted_values)
            
            # Then
            expect_identical(result, expected_results)
          })


test_that("findNFirstNonNull: Given two not NA values, and asking for more, should return result of len two", 
          {
            # Given 
            two_values_and_to_nas <- c("A", "B", NA, NA)
            n_wanted_values <- 3
            expected_results <- 2
            
            # When
            result <- findNFirstNonNull(two_values_and_to_nas, n_wanted_values)
            
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

test_that("checkAndReturnDataTable: Given a data.table with not unique names, Should return a data.table with unique names",
          {
            # Given
            data("adult")
            names(adult) <- rep("some_str", ncol(adult))
            
            # When
            expect_warning(result <- checkAndReturnDataTable(adult))
            
            # Then
            expect_equal(length(names(result)), length(unique(names(result))))
          })


test_that("checkAndReturnDataTable: control errors: data set type", 
          {
            # Given
            wrong_dataSet <- "a"
            
            # When + Then
            expect_error(checkAndReturnDataTable(wrong_dataSet),
                         "should be a data.table, a data.frame or a matrix.")
          })

test_that("checkAndReturnDataTable: control errors: a data set should not be empty", 
          {
            # Given
            empty_dataSet <- data.table()
            
            # When + Then
            expect_error(checkAndReturnDataTable(empty_dataSet), 
                         "should have at least have 1 line")
          })

test_that("checkAndReturnDataTable: control errors: a string is not a valide data set", 
          {
            # Given
            one_col_dataSet <- data.frame(row.names = c(1, 2))
            
            # When + Then
            expect_error(checkAndReturnDataTable(one_col_dataSet), 
                         "should have at least have 1 column")
          })

## is.verbose
# -----------
test_that("is.verbose: control input",
          {
            # Given
            wrong_verbose <- "a"
            
            # When + Then
            expect_error(is.verbose(wrong_verbose),
                         "verbose should be logical")
          })

test_that("is.verbose: control input error",
          {
            # Given
            correct_verbose <- TRUE
            
            # When + Then
            expect_null(is.verbose(correct_verbose))
          })

## is.verbose_levels
# ------------------
test_that("is.verbose_levels: control input wrong level",
          {
            # Given
            wrong_verbose_level <- "a"
            
            # When + Then
            expect_error(is.verbose_level(wrong_verbose_level),
                         "or an integer lower than")
          })

test_that("is.verbose_levels: too high level",
          {
            # Given
            too_high_level <- 3
            max_level <- 2
            
            # When + Then
            expect_error(is.verbose_level(too_high_level, max_level = max_level),
                         " or an integer lower than ")
          })

test_that("is.verbose_levels: correct level",
          {
            # Given
            correct_lvel <- 1
            max_level <- 2
            
            # When + Then
            expect_null(is.verbose_level(correct_lvel, max_level = max_level))
          })

## is.share
# ---------
test_that("is.share: wrong type",
          {
            # Given
            wrong_share <- "a"
            
            # When + Then
            expect_error(is.share(wrong_share),
                         "should be a numeric between 0 and 1")
          })

test_that("is.share: out of range share",
          {
            # Given
            out_of_range_share <- 2
            
            # When + Then
            expect_error(is.share(out_of_range_share),
                         "should be a numeric between 0 and 1")
          })

test_that("is.share: correct value",
          {
            # Given
            correct_share <- 1
            
            # When + Then
            expect_null(is.share(correct_share))
          })

## is.col
#--------
test_that("is.col: col is in data set",
          {
            # Given
            dataSet <- data.table(a = "1")
            
            # When + Then
            expect_null(is.col(dataSet, cols = "a"))
          })

test_that("is.col: 2 cols are in data set",
          {
            # Given
            dataSet <- data.table(a = "1", b = 2)
            
            # When + Then
            expect_null(is.col(dataSet, cols = c("a", "b")))
          })

test_that("is.col: one of 2 cols is not in dataset",
          {
            # Given
            dataSet <- data.table(a = "1")
            
            # When + Then
            expect_error(is.col(dataSet, cols = c("a", "b")), ". should be column of dataSet")
          })

test_that("is.col: col is not in dataset",
          {
            # Given
            dataSet <- data.table(a = "1")
            
            # When + Then
            expect_error(is.col(dataSet, cols = "b"), ". should be column of dataSet")
          })

test_that("is.col: error data set is not correct",
          {
            # Given
            wrong_dataSet <- 1
            
            # When + Then
            expect_error(is.col(wrong_dataSet, cols = "b"), "is.col: dataSet should be a data.table, data.frame or matrix")
          })

## real_cols 
# ----------
test_that("real_cols: select cols that are indeed in dataSet",
          {
            # Given 
            dataSet <- data.table(col_1 = 1)
            
            # When
            col_selected <- real_cols(dataSet, cols = c("col_1", "col_2"))
            
            # Then
            expect_equal(col_selected, c("col_1"))
          })
test_that("real_cols: auto mode give all column names", 
          {
            # Given 
            dataSet <- data.table(col_1 = 1, col_2 = 2)
            
            # When
            col_selected <- real_cols(dataSet, cols = "auto")
            
            # Then
            expect_equal(col_selected, colnames(dataSet))
          })

test_that("real_cols: auto mode and specified type give all coresponding cols", 
          {
            # Given 
            dataSet <- data.table(col_1 = 1, col_2 = "2")
            
            # When
            col_selected <- real_cols(dataSet, cols = "auto", types = "numeric")
            
            # Then
            expect_equal(col_selected, c("col_1"))
          })

test_that("real_cols: with char 0 col list throw null answer",
          {
            # Given
            dataSet = data.table(col_1 = 1)
            
            # When 
            selected_cols <- real_cols(adult, cols = character(0))
            
            # Then
            expect_null(selected_cols)
          })

test_that("real_cols: with NULL col list throw null answer",
          {
            # Given
            dataSet = data.table(col_1 = 1)
            
            # When 
            selected_cols <- real_cols(adult, cols = NULL)
            
            # Then
            expect_null(selected_cols)
          })

test_that("real_cols: is able to filter on numeric",
          {
            # Given
            date_set <- data.frame(date_col = as.Date( c("2015-01-01", "2016-01-01", "2015-09-01", "2015-03-01", "2015-01-31")),
                                   id = 1:5)
            
            # When
            cols_selected <- real_cols(date_set, cols = c("date_col", "id"), types = c("numeric"))
            
            # Then
            expect_identical(cols_selected, c("id"))
          })

test_that("real_cols: is able to filter on dates",
          {
            # Given
            date_set <- data.frame(date_col = as.Date( c("2015-01-01", "2016-01-01", "2015-09-01", "2015-03-01", "2015-01-31")),
                                   id = 1:5)
            
            # When
            cols_selected <- real_cols(date_set, cols = c("date_col", "id"), types = c("date"))
            
            # Then
            expect_identical(cols_selected, c("date_col"))
          })

## getPossibleSeparators
#------------------------
test_that("getPossibleSeparators:", 
          {
            # Given
            
            # When
            possible_separator <- getPossibleSeparators()
            
            # Then
            expect_true(is.vector(possible_separator))
          })


## printl
#--------
test_that("printl:", 
          {
            # Given + When + Then
            expect_output(printl("printl", " is a private function ", " easier to use than print"),
                          "printl is a private function  easier to use than print"
                          )
          })

## controlNumberOfRows
#--------------------
test_that("control_nb_rows: asking for less than number of rows is ok", 
          {
            # Given
            dataSet <- data.table(col1 = c(1, 2, 3))
            acceptable_n_rows <- 1
            
            # When 
            n_row_controled <- control_nb_rows(dataSet, acceptable_n_rows)
            
            # Then
            expect_equal(n_row_controled, acceptable_n_rows)
          })

test_that("control_nb_rows: asking for more than number of rows is not ok, warn + reduce to number of rows", 
          {
            # Given
            dataSet <- data.table(col1 = c(1, 2, 3))
            not_acceptable_n_rows <- 10
            
            # When + Then
            expect_warning(n_row_controled <- control_nb_rows(dataSet, not_acceptable_n_rows),
                           "You want to check more rows than there are in dataSet, I set nb_rows to")
            expect_equal(n_row_controled, nrow(dataSet))
          })

test_that("control_nb_rows: asking for 0 rows is not ok, warn + reduce to min(number of rows, 30)", 
          {
            # Given
            dataSet <- data.table(col1 = c(1, 2, 3))
            not_acceptable_n_rows <- 0
            
            # When + Then
            expect_warning(n_row_controled <- control_nb_rows(dataSet, not_acceptable_n_rows),
                           "You want to check at least a few rows than there are in dataSet, I set nb_rows to")
            expect_equal(n_row_controled, min(nrow(dataSet), 30))
          })

test_that("conrtol_nb_rows: number of row should be numeric",
          {
            # Given
            dataSet <- data.table(col1 = c(1, 2, 3))
            wrong_number_of_row <- "a"
            
            # When + Then
            expect_error(control_nb_rows(dataSet, nb_rows = wrong_number_of_row), " should be a numeric.")
          })

## is.agg_function
# -----------------
test_that("is.agg_function: thow error when functions doesn't contains only character",
          {
            # Given
            functions <- list(sum, "sum")
            
            # When + Then
            expect_error(is.agg_function(functions),  "functions should be a list of names")
          })

test_that("is.agg_function: thow warning when function is not found",
          {
            # Given
            functions <- list("sum", "a")
            
            # When + Then
            expect_warning(is.agg_function(functions), " doesn't exist, it won't be used.")
          })

test_that("is.agg_function: thow warning when a function is not a function. And remove it from the list",
          {
            # Given
            b <- 1
            attach(list(b=b)) # A bit ugly, but i don't know how to do it another way.
            functions <- list("sum", "b")
            
            # When + Then
            expect_warning(result <- is.agg_function(functions),  
                           " is not a function, it won't be used.")
            expect_equal(list("sum"), result)
            
            # Clean up
            detach(list(b=b))
          })

test_that("is.agg_function: thow warning when a function is not an aggregation function. And remove it from the list",
          {
            # Given
            functions <- list("sum", "sqrt")
            
            # When + Then
            expect_warning(result <- is.agg_function(functions),  
                           " sqrt is not an aggregation function")
            expect_equal(list("sum"), result)
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
            dataSet <- data.table()
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
