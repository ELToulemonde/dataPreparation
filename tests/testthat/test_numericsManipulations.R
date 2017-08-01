requireNamespace("data.table")
verbose = TRUE
## findAndTransformNumerics
#--------------------------


dataSet <- data.table(ID = 1:5,
                 col1 = c("1.2", "1.3", "1.2", "1", "6"), 
                 col2 = c("1,2", "1,3", "1,2", "1", "6")
                 )

data_transformed <- findAndTransformNumerics(dataSet, n_test = 5, verbose = verbose)

test_that("findAndTransformNumerics:",
          {
            expect_equal(all(sapply(data_transformed, class) == c("integer", "numeric", "numeric")), TRUE)
          })


data("messy_adult")
dataSet <- messy_adult[, .(date1, date2, date3)]
data_transformed <- findAndTransformNumerics(dataSet, n_test = 5, verbose = verbose)
test_that("findAndTransformNumerics: no numerics",
          {
            expect_equal(all(sapply(data_transformed, class) == "character"), TRUE)
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
            expect_equal(identifyNumerics(dataSet, n_test = 5, verbose = verbose)[[1]], "col1")
            expect_equal(identifyNumerics(dataSet, n_test = 5, verbose = verbose)[[2]], "col2")
          })
	
## identifyNumericsFormats
# ------------------------	
test_that("private function identifyNumericsFormats  : error",
		   {
            expect_error(identifyNumericsFormats(factor(c(1,2,3))), "identifyNumericsFormats: dataSet should be some characters")
          })
## as.numericStrip
# ----------------

test_that("Private function as.numericStrip:",
          {
            expect_equal(as.numericStrip("1,2"), 1.2)
          })



