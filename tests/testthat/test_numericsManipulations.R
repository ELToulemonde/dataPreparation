context("test_numericsManipulations.R")
requireNamespace("data.table")
verbose <- TRUE

## findAndTransformNumerics
#--------------------------
dataSet <- data.table(ID = 1:5,
                      col1 = c("1.2", "1.3", "1.2", "1", "6"), 
                      col2 = c("1,2", "1,3", "1,2", "1", "6")
)

data_transformed <- findAndTransformNumerics(dataSet, n_test = 5, verbose = verbose)
test_that("findAndTransformNumerics:",
          {
            expect_true(all(sapply(data_transformed, is.numeric)))
          })


data("messy_adult")
dataSet <- messy_adult[, .(date1, date3, date4)]
data_transformed <- findAndTransformNumerics(dataSet, n_test = 5, verbose = verbose)
test_that("findAndTransformNumerics: no numerics",
          {
            expect_true(all(sapply(data_transformed, is.character)))
          })

## identifyNumerics
# -----------------
dataSet <- data.table(ID = 1:5,
                      col1 = c("1.2", "1.3", "1.2", "1", "6"), 
                      col2 = c("1,2", "1,3", "1,2", "1", "6")
                      )
test_that("private function identifyNumerics :",
          {
            expect_equal(length(identifyNumerics(dataSet, n_test = 5, verbose = verbose)), 2)
            expect_identical(identifyNumerics(dataSet, cols = list(), n_test = 5, verbose = verbose), list(dont_strip = NULL, strip = NULL)) # Control that if told to do nothing, do nothing.
          })

## identifyNumericsFormats
# ------------------------	
test_that("private function identifyNumericsFormats: ",
          {
            expect_equal(identifyNumericsFormats(dataSet$col1), "notstrip")
            expect_equal(identifyNumericsFormats(dataSet$col2), "strip")
            expect_null(identifyNumericsFormats("a"))
            expect_error(identifyNumericsFormats(factor(c(1,2,3))), "identifyNumericsFormats: dataSet should be some characters")
          })

## as.numericStrip
# ----------------
test_that("Private function as.numericStrip:",
          {
            expect_equal(as.numericStrip("1,2"), 1.2)
          })
