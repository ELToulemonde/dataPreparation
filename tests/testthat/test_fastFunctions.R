requireNamespace("data.table")
## fastFilterVariables
#---------------------
data("messy_adult")
# Make it smaller to go faster
messy_adult <- messy_adult[1:5000, ]


test_that("fastHandleNa: There are no more NAs", 
          {
            expect_equal(ncol(fastFilterVariables(messy_adult, verbose = FALSE)), 20)
          })

## fastRound
##----------
M <- as.data.table(matrix(runif (3e4), ncol = 10))
M[, stringColumn := "a string"] 

test_that("fastHandleNa: There are no more NAs", 
          {
            expect_equal(all(fastRound(M, verbose = FALSE)[,1] == round(M[, 1], 2)), TRUE)
            expect_equal(all(fastRound(M, digits = 1, verbose = FALSE)[,1] == round(M[, 1], 1)), TRUE)
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


## fastIsEqual
#--------------
data("messy_adult")

test_that("private function: fastIsEqual", 
          {
            expect_equal(fastIsEqual(messy_adult[["education"]], messy_adult[["education_num"]]), FALSE)
            expect_equal(fastIsEqual(1:10, 1:10), TRUE)
			expect_equal(fastIsEqual(1:1001, 1:1001), TRUE)
            expect_equal(fastIsEqual(LETTERS, LETTERS), TRUE)
            expect_equal(fastIsEqual(1, 1), TRUE)
            expect_equal(fastIsEqual(1, 2), FALSE)
          }
)




## fastIsBijection
# -----------------
data("adult")
setDF(adult)

test_that("private function: fastIsBijection", 
          {
            expect_error(fastIsBijection(adult[, 1]))
          }
)
test_that("private function: fastIsBijection", 
          {
            expect_equal(fastIsBijection(adult[, c("education", "education_num")]), TRUE)
            expect_equal(fastIsBijection(adult[, c("education", "income")]), FALSE)
          }
)

## fastMaxNbElt
# -------------
test_that("private function: fastMaxNbElt", 
          {
            expect_equal(fastMaxNbElt(sample(1:5, 100, replace = TRUE), 1), FALSE)
            expect_equal(fastMaxNbElt(sample(1:5, 100, replace = TRUE), 4), FALSE)
            expect_equal(fastMaxNbElt(sample(1:5, 100, replace = TRUE), 5), TRUE)
          })

