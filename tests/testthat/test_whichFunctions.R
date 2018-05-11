context("test_whichFunctions.R")
requireNamespace("data.table")
verbose <- TRUE

## whichAreConstant
#------------------
dataSet <- data.table(constantCol = rep("a", 100), nonConstantCol = rnorm(100))

test_that("whichAreConstant:", 
          {
            expect_equal(whichAreConstant(dataSet, verbose = verbose), 1)
          })



## whichAreInDouble
#------------------
# Simple matrix
M <- matrix(1, nrow = 1e6, ncol = 3)

# Matrix with NA
M1 <- M
M1[1, 2] <- NA

M2 <- M1
M2[1, 1] <- NA


test_that("whichAreInDouble give correct RESULTS", 
          {
            expect_identical(whichAreInDouble(M, verbose = verbose), as.integer(c(2, 3)))
            expect_identical(whichAreInDouble(M1, verbose = verbose), as.integer(c(3)))
            expect_identical(whichAreInDouble(M2, verbose = verbose), as.integer(c(2)))
          })

data("messy_adult")
test_that("whichAreInDouble: exceptions", 
          {
            expect_null(whichAreInDouble(messy_adult[,.(date1)], verbose = verbose))
          })


## whichAreBijection
# ------------------
data("adult")
test_that("whichAreBijection", 
          {
            expect_equal(whichAreBijection(adult, verbose = verbose), 5)
            expect_equal(whichAreBijection(adult, keep_cols = "education_num", verbose = verbose), 4)
            # Nothing if one column
            expect_null(whichAreBijection(adult[,c("education"), drop = FALSE], verbose = verbose))
          })

## whichAreIncluded
# ------------------
data("messy_adult")
# Reduce it to make it faster
messy_adult <- messy_adult[1:5000, ]
messy_adult$are50OrMore <- messy_adult$age > 50
test_that("whichAreIncluded: build column", 
          {
            expect_identical(whichAreIncluded(messy_adult, verbose = verbose), as.integer(c(3, 5, 7, 13, 25)))
            expect_identical(whichAreIncluded(messy_adult, keep_cols = "education", verbose = verbose), as.integer(c(3, 5, 7, 14, 25)))
            expect_null(whichAreIncluded(messy_adult[, .(age)], verbose = verbose))
          })

# As one can, see this column that doesn't have additional info than age is spotted.

# But you should be carefull, if there is a column id, every column will be dropped:
rm(messy_adult)
data("messy_adult")
messy_adult$id <- 1:nrow(messy_adult) # build id
test_that("whichAreIncluded: id at the end", 
          {
            expect_identical(whichAreIncluded(messy_adult, verbose = verbose), 1:24)
          })
# Set id as first column
setcolorder(messy_adult, c("id", setdiff(names(messy_adult), "id"))) 
test_that("whichAreIncluded: id at the beginning", 
          {
            expect_identical(whichAreIncluded(messy_adult, verbose = verbose), 2:25)
          })



