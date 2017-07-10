requireNamespace("data.table")
verbose = TRUE

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
            expect_equal(all(whichAreInDouble(M, verbose = verbose) == c(2, 3)), TRUE)
            expect_equal(all(whichAreInDouble(M1, verbose = verbose) == c(3)), TRUE)
            expect_equal(all(whichAreInDouble(M2, verbose = verbose) == c(2)), TRUE)
          })
		  
data("messy_adult")
test_that("whichAreInDouble: exceptions", 
          {
            expect_equal(is.null(whichAreInDouble(messy_adult[,.(date1)], verbose = verbose)), TRUE)
          })


## whichAreBijection
# ------------------
data("adult")


test_that("whichAreBijection", 
          {
            expect_equal(all(whichAreBijection(adult, verbose = verbose) == c(5)), TRUE)
          })

data("messy_adult")
test_that("whichAreInDouble: exceptions", 
          {
            expect_equal(is.null(whichAreBijection(messy_adult[,.(date1)], verbose = verbose)), TRUE)
          })

## whichAreIncluded
# ------------------

data(messy_adult)

# Reduce it to make it faster
messy_adult = messy_adult[1:5000, ]

# Check for included columns

test_that("whichAreIncluded: standard test", 
          {
            expect_equal(all(whichAreIncluded(messy_adult, verbose = verbose) == c(7, 4, 9, 14)), TRUE)
          })


# Return columns that are also constant, double and bijection
# Let's add a truly just included column
messy_adult$are50OrMore <- messy_adult$age > 50

test_that("whichAreIncluded: build column", 
          {
            expect_equal(all(whichAreIncluded(messy_adult, verbose = verbose) == c(7, 4, 9, 25, 14)), TRUE)
          })
# As one can, see this column that doesn't have additional info than age is spotted.

# But you should be carefull, if there is a column id, every column will be dropped:
messy_adult$id = 1:nrow(messy_adult) # build id
setcolorder(messy_adult, c("id", setdiff(names(messy_adult), "id"))) # Set id as first column


test_that("whichAreIncluded: id", 
          {
            expect_equal(all(whichAreIncluded(messy_adult, verbose = verbose) == 2:26), TRUE)
          })
