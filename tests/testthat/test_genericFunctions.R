requireNamespace("data.table")
verbose <- TRUE
## findNFirstNonNull
#-------------------


test_that("findNFirstNonNull: test numerics", 
          {
            expect_equal(all(findNFirstNonNull(1:50, 5) == 1:5), TRUE)
            expect_equal(all(findNFirstNonNull(1:50, 10) == 1:10), TRUE)
            expect_equal(all(findNFirstNonNull(c(NA, 1:50), 10) == 1:10), TRUE)
          })


test_that("findNFirstNonNull: test character", 
          {
            expect_equal(all(findNFirstNonNull(LETTERS, 3) == c("A", "B", "C")), TRUE)
            expect_equal(all(findNFirstNonNull(LETTERS, 5) == c("A", "B", "C", "D", "E")), TRUE)
            expect_equal(all(findNFirstNonNull(c(NA, LETTERS), 5) == c("A", "B", "C", "D", "E")), TRUE)
          })


test_that("findNFirstNonNull: not enough not NAs values", 
          {
            expect_equal(length(findNFirstNonNull(c("A", "B", NA, NA), 3)), 2)
          })
## checkAndReturnDataTable
#-------------------------
data("messy_adult")

test_that("checkAndReturnDataTable", 
          {
            expect_equal(all(class(checkAndReturnDataTable(messy_adult)) == c("data.table", "data.frame")), TRUE)
            expect_equal(all(class(checkAndReturnDataTable(as.data.frame(messy_adult))) == c("data.table", "data.frame")), TRUE)
            expect_equal(all(class(checkAndReturnDataTable(as.matrix(messy_adult))) == c("data.table", "data.frame")), TRUE)
            
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
## checkIfIsColumn
#------------------

dataSet <- data.table(a = "1")
is.col(dataSet, cols = "a")

expect_error(is.col(dataSet, cols = "b"), ". should be column of dataSet")
expect_error(is.col(1, cols = "b"), "is.col: dataSet should be a data.table, data.frame or matrix")

## real_cols 
# ----------
data("messy_adult")
test_that("real_cols:",
          {
            expect_equal(length(real_cols(c("mail", "asucgzr"), names(messy_adult), function_name = "test")), 1)
          }
)
## getPossibleSeparators
#------------------------
result <- getPossibleSeparators()


## printl
#--------
if (verbose){
  printl("printl", " is a private function ", " easier to use than print")
}


## controlNumberOfRows
#--------------------
dataSet <- data.table(col1 = c(1, 2, 3))
control_nb_rows(dataSet, 1)

test_that("control_nb_rows:", 
          {
            expect_equal(control_nb_rows(dataSet, 1), 1)
            expect_warning(control_nb_rows(dataSet, 10), "You want to check more rows than there are in dataSet, I set nb_rows to 3")
            expect_warning(control_nb_rows(dataSet, 0), "You want to check at least a few rows than there are in dataSet, I set nb_rows to 3")
            expect_error(control_nb_rows(dataSet, "a"), " should be a numeric.")
          })


## true.aggFunction
# -----------------
test_that("true.aggFunction:", 
          {
            expect_warning(result <- true.aggFunction(list(sum = sum, a = "a")), " is not a function, it wont be used.")
            expect_equal(length(result), 1)
          })


## function.maker
# ---------------
test_that("function.maker: ",
          {
            expect_equal(class(function.maker(function(x){sum(x, na.rm = TRUE)}, type = "numeric")), "function")
            expect_equal(class(function.maker(1, type = "numeric")), "function")
            expect_equal(class(function.maker("a", type = "character")), "function")
            expect_equal(class(function.maker(TRUE, type = "logical")), "function")
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
