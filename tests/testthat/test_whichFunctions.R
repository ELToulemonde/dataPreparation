requireNamespace("data.table")
## whichAreConstant
#----------------
dataSet <- data.table(constantCol = rep("a", 100), nonConstantCol = rnorm(100))

test_that("whichAreConstant:", 
          {
            expect_equal(whichAreConstant(dataSet, verbose = FALSE), 1)
          })


		  
## whichAreInDouble
#--------------------------
# Simple matrix
M <- matrix(1, nrow = 1e6, ncol = 3)

# Matrix with NA
M1 <- M
M1[1, 2] <- NA

M2 <- M1
M2[1, 1] <- NA


test_that("whichAreInDouble give correct RESULTS", 
          {
            expect_equal(all(whichAreInDouble(M, verbose = FALSE) == c(2, 3)), TRUE)
            expect_equal(all(whichAreInDouble(M1, verbose = FALSE) == c(3)), TRUE)
            expect_equal(all(whichAreInDouble(M2, verbose = FALSE) == c(2)), TRUE)
          })


## whichAreBijection
# ------------------
data("adult")


test_that("wwhichAreBijection", 
          {
            expect_equal(all(whichAreBijection(adult, verbose = FALSE) == c(5)), TRUE)
          })


## whichAreIncluded
# ------------------

data(messy_adult)

# Reduce it to make it faster
messy_adult = messy_adult[1:5000, ]

# Check for included columns

test_that("whichAreIncluded: standard test", 
          {
            expect_equal(all(whichAreIncluded(messy_adult, verbose = FALSE) == c(7, 4, 9, 14)), TRUE)
          })


# Return columns that are also constant, double and bijection
# Let's add a truly just included column
messy_adult$are50OrMore <- messy_adult$age > 50

test_that("whichAreIncluded: build column", 
          {
            expect_equal(all(whichAreIncluded(messy_adult, verbose = FALSE) == c(7, 4, 9, 25, 14)), TRUE)
          })
# As one can, see this column that doesn't have additional info than age is spotted.

# But you should be carefull, if there is a column id, every column will be dropped:
messy_adult$id = 1:nrow(messy_adult) # build id
setcolorder(messy_adult, c("id", setdiff(names(messy_adult), "id"))) # Set id as first column


test_that("whichAreIncluded: id", 
          {
            expect_equal(all(whichAreIncluded(messy_adult, verbose = FALSE) == 2:26), TRUE)
          })
