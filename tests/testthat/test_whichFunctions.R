## whichAreConstant
#----------------
dataSet <- data.table(constantCol = rep("a", 100), nonConstantCol = rnorm(100))

test_that("whichAreConstant:", 
          {
            expect_equal(whichAreConstant(dataSet), 1)
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