## Documentation for unit testing
#--------------------------------
# http://r-pkgs.had.co.nz/tests.html
# http://stat545.com/packages05_foofactors-package-02.html
verbose <- TRUE
## prepareSet
#----------------



data(messy_adult)
messy_adult <- unFactor(messy_adult, verbose = FALSE)
messy_adult <- generateFromCharacter(messy_adult, cols = "auto", verbose = FALSE, drop = TRUE)
adult_prep <- shapeSet(copy(messy_adult), verbose = verbose)


adult_numMat <- shapeSet(copy(adult), finalForm = "numerical_matrix")
test_that("prepareSet: test class of results:", 
          {
            expect_equal(class(adult_prep)[1], "data.table")
            expect_equal(class(adult_numMat), "matrix")
          })


rm(messy_adult)

## setAsNumericMatrix
# --------------------
data(messy_adult)
test_that("setAsNumericMatrix: ",
          {
            expect_error(setAsNumericMatrix(messy_adult), "some columns are not numerical/logical/factor. Consider using shapeSet()")
          }
)

data(messy_adult)
messy_adult <- unFactor(messy_adult, n_unfactor = 53, verbose = FALSE)
messy_adult <- generateFromCharacter(messy_adult, cols = "auto", verbose = FALSE, drop = TRUE)
messy_adult <- shapeSet(messy_adult, verbose = FALSE, finalForm = "data.table")
messy_adult <- fastFilterVariables(messy_adult, verbose = FALSE)
test_that("setAsNumericMatrix: ",
          {
            expect_equal(ncol(setAsNumericMatrix(messy_adult)), 126)
            expect_equal(ncol(setAsNumericMatrix(messy_adult, allCols = TRUE, sparse = TRUE)), 137)
          }
)

