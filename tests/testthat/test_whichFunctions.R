context("test_whichFunctions.R")
requireNamespace("data.table")
verbose <- TRUE

## whichAreConstant
#------------------
test_that("whichAreConstant: should find string constant column", 
          {
            # Given
            dataSet <- data.table(constantCol = rep("a", 100), nonConstantCol = rnorm(100))
            
            # When
            constant_cols <- whichAreConstant(dataSet, verbose = verbose)
            
            # Then
            expect_equal(constant_cols, 1)
          })



## whichAreInDouble
#------------------
test_that("whichAreInDouble: should give col 2 and 3 on a 3 column matrix full of a constant",
          {
            # Given
            M <- matrix(1, nrow = 1e3, ncol = 3)
            
            # When
            double_columns <- whichAreInDouble(M, verbose = verbose)
            
            # Then
            expect_equal(double_columns, c(2L, 3L))
          })


test_that("whichAreInDouble: should give 3 on a 3 column matrix full of a constant with some NA on col 2",
          {
            # Given
            M <- matrix(1, nrow = 1e3, ncol = 3)
            M[1, 2] <- NA
            
            # When
            double_columns <- whichAreInDouble(M, verbose = verbose)
            
            # Then
            expect_equal(double_columns, c(3L))
          })


test_that("whichAreInDouble: should give 2 on a 3 column matrix full of a constant with same NA on col 1 and col 2",
          {
            # Given
            M <- matrix(1, nrow = 1e3, ncol = 3)
            M[1, 1] <- NA
            M[1, 2] <- NA
            
            # When
            double_columns <- whichAreInDouble(M, verbose = verbose)
            
            # Then
            expect_equal(double_columns, c(2L))
          })

test_that("whichAreInDouble: should give nothing on a single column matrix",
          {
            # Given
            M <- matrix(1, nrow = 1e3, ncol = 1)
            
            # When
            double_columns <- whichAreInDouble(M, verbose = verbose)
            
            # Then
            expect_null(double_columns)
          })


## whichAreBijection
# ------------------
data("adult")


test_that("whichAreBijection: adult set contains on bijection eduction and education_num should be spotted",
          {
            # Given
            data("adult")
            cols <- c("education", "education_num")
            
            # When
            bijection_cols <- whichAreBijection(adult[, cols], verbose = verbose)
            
            # Then
            expect_equal(bijection_cols, 2)
          })

test_that("whichAreBijection: adult set contains on bijection eduction and education_num should be spotted. When second one is asked to be kept, the other one is returned",
          {
            # Given
            data("adult")
            cols <- c("education", "education_num")
            
            # When
            bijection_cols <- whichAreBijection(adult[, cols], keep_cols = "education_num", verbose = verbose)
            
            # Then
            expect_equal(bijection_cols, 1)
          })

test_that("whichAreBijection: one column data set has no bijection", 
          {
            # Given
            data("adult")
            cols <- c("education")
            
            # When
            bijection_cols <- whichAreBijection(adult[, cols, drop = FALSE], verbose = verbose)
            
            # Then
            expect_null(bijection_cols)
          })

## whichAreIncluded
# ------------------
test_that("whichAreIncluded: education and education_num are duplicated so should be spoted as included",
          {
            # Given
            data("messy_adult")
            messy_adult <- messy_adult[1:500, ] # reduce it to compute faster
            cols <- c("education", "education_num")
            
            # When
            included_cols <- whichAreIncluded(messy_adult[, c(cols), with=FALSE], verbose = verbose)
            
            # Then
            expect_equal(included_cols, 1L)
          })

test_that("whichAreIncluded: when a column is derivated from another, it should be spotted as included",
          {
            # Given
            data("messy_adult")
            messy_adult <- messy_adult[1:500, ] # reduce it to compute faster
            messy_adult$are50OrMore <- messy_adult$age > 50
            cols <- c("age", "are50OrMore")
            
            # When
            included_cols <- whichAreIncluded(messy_adult[, c(cols), with=FALSE], verbose = verbose)
            
            # Then
            expect_equal(included_cols, 2L)
          })

test_that("whichAreIncluded: a single column set should not have included columns",
          {
            # Given
            data("messy_adult")
            messy_adult <- messy_adult[1:500, ] # reduce it to compute faster
            messy_adult$are50OrMore <- messy_adult$age > 50
            cols <- c("age")
            
            # When
            included_cols <- whichAreIncluded(messy_adult[, c(cols), with=FALSE], verbose = verbose)
            
            # Then
            expect_null(included_cols)
          })

test_that("whichAreIncluded: when a column with unique value on each row is added, all other column are included",
          {
            # Given
            data("messy_adult")
            messy_adult <- messy_adult[1:500, ] # reduce it to compute faster
            messy_adult$id <- 1:nrow(messy_adult) # build id
            existing_cols_index = (1:ncol(messy_adult))[names(messy_adult) != "id"]
            
            # When
            included_cols <- whichAreIncluded(messy_adult, verbose = verbose)
            
            # Then
            expect_equal(included_cols, existing_cols_index)
          })

test_that("whichAreIncluded: when a column with unique value on each row is added, all other column are included even if id is 1st col (order doesn't mawtter)",
          {
            # Given
            data("messy_adult")
            messy_adult <- messy_adult[1:500, ] # reduce it to compute faster
            messy_adult$id <- 1:nrow(messy_adult) # build id
            setcolorder(messy_adult, c("id", setdiff(names(messy_adult), "id")))             
            existing_cols_index = (1:ncol(messy_adult))[names(messy_adult) != "id"]
            
            # When
            included_cols <- whichAreIncluded(messy_adult, verbose = verbose)
            
            # Then
            expect_equal(included_cols, existing_cols_index)
          })



