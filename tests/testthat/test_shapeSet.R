## Documentation for unit testing
#--------------------------------
# http://r-pkgs.had.co.nz/tests.html
# http://stat545.com/packages05_foofactors-package-02.html
context("test_shapeSet.R")
verbose <- TRUE
## prepareSet
#----------------

data("adult")
adult$logical <- adult$age > 25 # Add a logical column
test_that("prepareSet: test class of results:", 
          {
            expect_true(is.data.table(shapeSet(copy(adult), verbose = verbose)))
            expect_true(is.matrix(shapeSet(copy(adult), finalForm = "numerical_matrix")))
          })
adult$logical <- NULL
## setAsNumericMatrix
# --------------------
data("messy_adult")
test_that("setAsNumericMatrix: ",
          {
            expect_error(setAsNumericMatrix(messy_adult), "some columns are not numerical/logical/factor. Consider using shapeSet()")
          })

data("adult")
adult <- shapeSet(adult, verbose = FALSE, finalForm = "data.table")

test_that("setAsNumericMatrix: ",
          {
            expect_equal(ncol(setAsNumericMatrix(adult)), 102)
            expect_equal(ncol(setAsNumericMatrix(adult, allCols = TRUE, sparse = TRUE, intercept = TRUE)), 111)
            expect_error(setAsNumericMatrix("cszdez"), "setAsNumericMatrix: dataSet is not a data.table")
          })