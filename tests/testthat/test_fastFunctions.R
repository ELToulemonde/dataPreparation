context("test_fastFunctions.R")
requireNamespace("data.table")
verbose <- TRUE
## fastFilterVariables
#---------------------
data("messy_adult")
# Make it smaller to go faster
messy_adult <- messy_adult[1:5000, ]
messy_adult$age2 = messy_adult$age # add a double
messy_adult$are50OrMore <- messy_adult$age > 50 # Add an included

test_that("fastFilterVariables: ", 
          {
            expect_equal(ncol(fastFilterVariables(messy_adult, level = 4, verbose = verbose)), 20)
          })

## is.filtering_level
#--------------------
test_that("Private function: is.filtering_level ", 
          {
            expect_error(is.filtering_level(level = "a"), ": level should be 1, 2, 3 or 4.")
          })

## fastRound
##----------
M <- as.data.table(matrix(runif (3e3), ncol = 10))
M[, stringColumn := "a string"] 

test_that("fastRound: ", 
          {
            expect_true(all(fastRound(M, verbose = verbose)[,1] == round(M[, 1], 2)))
            expect_true(all(fastRound(M, digits = 1, verbose = verbose)[,1] == round(M[, 1], 1)))
            expect_error(fastRound(M, digits = "a", verbose = verbose), ": digits should be an integer")
          })


## Handle NA Values
#-------------------
dataSet <-  data.table(numCol = c(1, 2, 3, NA), 
                       charCol = c("", "a", NA, "c"), 
                       booleanCol = c(TRUE, NA, FALSE, NA))

test_that("fastHandleNa: There are no more NAs", 
          {
            expect_false(any(is.na(fastHandleNa(dataSet))))
          })

data("messy_adult")
messy_adult$mail[sample(1:nrow(messy_adult), 10)] = NA
test_that("fastHandleNa: There are no more NAs with factor", 
          {
            expect_false(any(is.na(fastHandleNa(messy_adult))))
          })

## fastIsEqual
#--------------
data("messy_adult")
test_that("fastIsEqual: testing various scenariis", 
          {
            expect_false(fastIsEqual(1:9, 1:10)) # Different legnth
            expect_false(fastIsEqual(messy_adult[["education"]], messy_adult[["education_num"]])) # different values
            expect_true(fastIsEqual(1:10, 1:10)) # Same vector
            expect_true(fastIsEqual(1:1001, 1:1001)) # Same long vector
            expect_true(fastIsEqual(LETTERS, LETTERS)) # With characters
            expect_true(fastIsEqual(1, 1)) # Integers
            expect_false(fastIsEqual(1, 2)) # Integers
            expect_true(fastIsEqual(messy_adult, messy_adult)) # data.table
          })




## fastIsBijection
# -----------------
data("adult")
df = data.frame(col1 = c(rep(0, 9), 1), col2 = c(rep(1, 9), 1)) # Tricky non-bijection on the threshold
test_that("private function: fastIsBijection", 
          {
            expect_true(fastIsBijection(adult[["education"]], adult[["education_num"]]))
            expect_false(fastIsBijection(adult[["education"]], adult[["income"]]))
            expect_false(fastIsBijection(df[["col1"]], df[["col2"]]))
          })

## fastMaxNbElt
# -------------
test_that("private function: fastMaxNbElt", 
          {
            expect_false(fastMaxNbElt(sample(1:5, 100, replace = TRUE), 1))
            expect_false(fastMaxNbElt(sample(1:5, 100, replace = TRUE), 4))
            expect_true(fastMaxNbElt(sample(1:5, 100, replace = TRUE), 5))
          })



## fastIsIncluded
# ----------------
# data("messy_adult")
# messy_adult$ageover50 = messy_adult$age >50

# test_that("private function: fastIsBijection", 
# {
# expect_equal(fastIsIncluded(messy_adult[["constant"]], messy_adult[["date2"]]),1)
# expect_equal(fastIsIncluded(messy_adult[["education_num"]], messy_adult[["education"]]), "bijection")
# expect_equal(fastIsIncluded(messy_adult[["age"]], messy_adult[["education"]]), NULL)
# expect_equal(fastIsIncluded(messy_adult[["age"]], messy_adult[["ageover50"]]), 2)
# expect_equal(fastIsIncluded(messy_adult[["ageover50"]], messy_adult[["ageover50"]]), "bijection")
# expect_equal(fastIsIncluded(messy_adult[["ageover50"]], messy_adult[["age"]]), 1)
# })
