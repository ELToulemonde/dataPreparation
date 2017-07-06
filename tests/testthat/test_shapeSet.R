## Documentation for unit testing
#--------------------------------
# http://r-pkgs.had.co.nz/tests.html
# http://stat545.com/packages05_foofactors-package-02.html

## prepareSet
#----------------



data(adult)
adult_prep <- shapeSet(copy(adult), verbose = FALSE)


adult_numMat <- shapeSet(copy(adult), finalForm = "numerical_matrix")
test_that("prepareSet: test class of results:", 
          {
            expect_equal(class(adult_prep)[1], "data.table")
			expect_equal(class(adult_numMat), "matrix")
          })




## toNumericMatrixTransform
# -------------------------