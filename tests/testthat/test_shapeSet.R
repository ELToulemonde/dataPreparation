## Documentation for unit testing
#--------------------------------
# http://r-pkgs.had.co.nz/tests.html
# http://stat545.com/packages05_foofactors-package-02.html
context("test_shapeSet.R")
verbose <- TRUE
## prepareSet
#----------------
test_that("shapeSet: test class of results: shape set shape to data.table by default", 
          {
            # Given
            data("adult")
            
            # When
            adult_shaped <- shapeSet(copy(adult), verbose = verbose)
            
            # Then
            expect_true(is.data.table(adult_shaped))
            expect_true(is.matrix(shapeSet(copy(adult), finalForm = "numerical_matrix")))
          })

test_that("shapeSet: test class of results: shape set shape to matrix if asked for numerical_matrix", 
          {
            # Given
            data("adult")
            
            # When
            adult_shaped <- shapeSet(copy(adult), verbose = verbose, finalForm = "numerical_matrix")
            
            # Then
            expect_true(is.matrix(adult_shaped))
          })

test_that("shapeSet: shape set should encode logical into 0, 1", 
          {
            # Given
            dataSet <- data.table(logical_col = sample(c(TRUE, FALSE), 100, replace = TRUE))
            
            # When
            dataSet_shaped <- shapeSet(dataSet, verbose = verbose)
            
            # Then
            expect_true(all(dataSet_shaped[["logical_col"]] %in% c(0, 1)))
          })


## setAsNumericMatrix
# --------------------
test_that("setAsNumericMatrix: throw error if data set contains some col that are not numeric, logical or factor",
          {
            # Given
            dataSet <- data.table(date_col = as.Date("2018-01", "2018-01-31"))
            
            # When + Then
            expect_error(setAsNumericMatrix(dataSet), 
                         "some columns are not numerical/logical/factor. Consider using shapeSet()")
          })

test_that("setAsNumericMatrix: one hot encode factors",
          {
            # Given
            dataSet <- data.table(factor_col = as.factor(c("a", "b", "c")))
            
            # When 
            dataSet_as_numeric_matrix <- setAsNumericMatrix(dataSet, intercept = FALSE, allCols = TRUE, sparse=FALSE)
            
            # Then
            expect_equal(ncol(dataSet_as_numeric_matrix), uniqueN(dataSet[["factor_col"]]))
          })

test_that("setAsNumericMatrix: one hot encode factors with allCols = false nth column is not represented",
          {
            # Given
            dataSet <- data.table(factor_col = as.factor(c("a", "b", "c")),
                                  factor_col_2 = as.factor(c("a", "b", "c")))
            
            # When 
            dataSet_as_numeric_matrix <- setAsNumericMatrix(dataSet, intercept = FALSE, allCols = FALSE, sparse=FALSE)
            
            # Then
            expect_equal(ncol(dataSet_as_numeric_matrix), 5)
          })

test_that("setAsNumericMatrix: if intercept is true add a column first one with all ones",
          {
            # Given
            dataSet <- data.table(factor_col = 1:10)
            
            # When 
            dataSet_as_numeric_matrix <- setAsNumericMatrix(dataSet, intercept = TRUE, allCols = TRUE, sparse=FALSE)
            
            # Then
            expect_equal(ncol(dataSet_as_numeric_matrix), ncol(dataSet) + 1)
            expect_true(all(dataSet_as_numeric_matrix[, 1] == 1))
          })

test_that("setAsNumericMatrix: one hot encode factors with allCOls at false nth column is not represented",
          {
            # Given
            dataSet <- data.table(factor_col = as.factor(c("a", "b", "c")),
                                  factor_col_2 = as.factor(c("a", "b", "c")))
            
            # When 
            dataSet_as_numeric_matrix <- setAsNumericMatrix(dataSet, intercept = FALSE, allCols = FALSE, sparse=TRUE)
            
            # Then
            expect_equal(ncol(dataSet_as_numeric_matrix), 5)
          })

test_that("setAsNumericMatrix: trhow error if not called on data.table",
           {
             # Given
             wrong_frame <- "cszdez"
             
             # When + Then
             expect_error(setAsNumericMatrix(wrong_frame), 
                          "setAsNumericMatrix: dataSet is not a data.table")
           })