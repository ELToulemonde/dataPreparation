requireNamespace("data.table")
verbose <- TRUE
## fastFilterVariables
#---------------------
data("messy_adult")
# Make it smaller to go faster
messy_adult <- messy_adult[1:5000, ]
messy_adult$age2 = messy_adult$age # add a double

test_that("fastFilterVariables: ", 
          {
			expect_equal(ncol(fastFilterVariables(messy_adult, level = 4, verbose = verbose)), 20)
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

# To set NAs to 0, FALSE and "" (respectivly for numeric, boolean, character)
data_withoutNA <- fastHandleNa(dataSet)

test_that("fastHandleNa: There are no more NAs", 
          {
            expect_equal(sum(is.na(data_withoutNA)), 0)
          })

data("messy_adult")
messy_adult$mail[sample(1:nrow(messy_adult), 10)] = NA
messy_adult <- fastHandleNa(messy_adult)
test_that("fastHandleNa: There are no more NAs with factor", 
          {
            expect_equal(sum(is.na(messy_adult)), 0)
          })

## fastIsEqual
#--------------
data("messy_adult")

test_that("private function: fastIsEqual", 
          {
            expect_false(fastIsEqual(1:9, 1:10))
            expect_false(fastIsEqual(messy_adult[["education"]], messy_adult[["education_num"]]))
            expect_true(fastIsEqual(1:10, 1:10))
            expect_true(fastIsEqual(1:1001, 1:1001))
            expect_true(fastIsEqual(LETTERS, LETTERS))
            expect_true(fastIsEqual(1, 1))
            expect_false(fastIsEqual(1, 2))
            expect_true(fastIsEqual(messy_adult, messy_adult))
          })




## fastIsBijection
# -----------------
data("adult")

test_that("private function: fastIsBijection", 
          {
            expect_true(fastIsBijection(adult[["education"]], adult[["education_num"]]))
            expect_false(fastIsBijection(adult[["education"]], adult[["income"]]))
          })

## fastMaxNbElt
# -------------
test_that("private function: fastMaxNbElt", 
          {
            expect_false(fastMaxNbElt(sample(1:5, 100, replace = TRUE), 1))
            expect_false(fastMaxNbElt(sample(1:5, 100, replace = TRUE), 4))
            expect_true(fastMaxNbElt(sample(1:5, 100, replace = TRUE), 5))
          })

