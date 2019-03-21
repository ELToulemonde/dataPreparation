context("test_fastFunctions.R")
requireNamespace("data.table")
verbose <- TRUE
# fastFilterVariables
#--------------------
test_that("fastFilterVariables: functionnal test on reference set", 
          {
            # Given
            data("messy_adult")
            messy_adult <- messy_adult[1:5000, ] # Make it smaller to go faster
            messy_adult$age2 <- messy_adult$age # add a double
            messy_adult$are50OrMore <- messy_adult$age > 50 # Add an included
            
            # When
            messy_adult_filtered <- fastFilterVariables(messy_adult, level = 4, verbose = verbose)
            # Then
            expect_equal(ncol(messy_adult_filtered), 20)
          })

# is.filtering_level
#-------------------
test_that("Private function: is.filtering_level ", 
          {
            # Given
            wrong_level <- "a"
            
            # When + Then
            expect_error(is.filtering_level(level = wrong_level), 
                         ": level should be 1, 2, 3 or 4.")
          })

# fastRound
# ----------
test_that("fastRound: ", 
          {
            # Given
            M <- as.data.table(matrix(runif (3e3), ncol = 10))
            M[, stringColumn := "a string"] 
            
            # When
            M_rounded <- fastRound(M, verbose = verbose)
            
            # Then
            expect_true(all(M_rounded[,1] == round(M[, 1], 2)))
          })

test_that("fastRound: ", 
          {
            # Given
            M <- as.data.table(matrix(runif (3e3), ncol = 10))
            M[, stringColumn := "a string"] 
            n_digits <- 1
            # When
            M_rounded <- fastRound(M, digits = n_digits, verbose = verbose)
            
            # Then
            expect_true(all(M_rounded[,1] == round(M[, 1], n_digits)))
          })

test_that("fastRound: ", 
          {
            # Given
            M <- as.data.table(matrix(runif (3e3), ncol = 10))
            wrong_digits <- "a"
            
            # When + Then
            expect_error(fastRound(M, digits = wrong_digits, verbose = verbose), 
                         ": digits should be an integer")
          })

# Handle NA Values
# ----------------
test_that("fastHandleNa: There are no more NAs on numercial column", 
          {
            # Given
            dataSet <-  data.table(num_col = c(1, 2, 3, NA))
            
            # When
            dataSet_without_na <- fastHandleNa(dataSet)
            
            # Then
            expect_false(any(is.na(dataSet_without_na)))
          })

test_that("fastHandleNa: There are no more NAs on character column", 
          {
            # Given
            dataSet <-  data.table(character_col = c("", "a", NA, "c"))
            
            # When
            dataSet_without_na <- fastHandleNa(dataSet)
            
            # Then
            expect_false(any(is.na(dataSet_without_na)))
          })

test_that("fastHandleNa: There are no more NAs logical column", 
          {
            # Given
            dataSet <-  data.table(logical_factor = c(TRUE, NA, FALSE, NA))
            
            # When
            dataSet_without_na <- fastHandleNa(dataSet)
            
            # Then
            expect_false(any(is.na(dataSet_without_na)))
          })

test_that("fastHandleNa: There are no more NAs on factor col", 
          {
            # Given
            dataSet <-  data.table(factor_col = as.factor(c("", "a", NA, "c")))
            
            # When
            dataSet_without_na <- fastHandleNa(dataSet)
            
            # Then
            expect_false(any(is.na(dataSet_without_na)))
          })


# fastIsEqual
#------------
data("messy_adult")
test_that("fastIsEqual: different length objects", 
          {
            # Given
            dataSet_1 <- 1:9 
            dataSet_2 <- 1:10
            
            # When
            result <- fastIsEqual(dataSet_1, dataSet_2)
            
            # Then
            expect_false(result)
          })

test_that("fastIsEqual: different content type", 
          {
            # Given
            dataSet_1 <- c(1, 2, 3)
            dataSet_2 <- c("A", "B", "C")
            
            # When
            result <- fastIsEqual(dataSet_1, dataSet_2)
            
            # Then
            expect_false(result)
          })

test_that("fastIsEqual: same vector", 
          {
            # Given
            dataSet_1 <- 1:10
            dataSet_2 <- 1:10
            
            # When
            result <- fastIsEqual(dataSet_1, dataSet_2)
            
            # Then
            expect_true(result)
          })

test_that("fastIsEqual: same long vector", 
          {
            # Given
            dataSet_1 <- 1:1001
            dataSet_2 <- 1:1001
            
            # When
            result <- fastIsEqual(dataSet_1, dataSet_2)
            
            # Then
            expect_true(result)
          })

test_that("fastIsEqual: same character vector", 
          {
            # Given
            dataSet_1 <- LETTERS
            dataSet_2 <- LETTERS
            
            # When
            result <- fastIsEqual(dataSet_1, dataSet_2)
            
            # Then
            expect_true(result)
          })

test_that("fastIsEqual: same integer", 
          {
            # Given
            dataSet_1 <- 1
            dataSet_2 <- 1
            
            # When
            result <- fastIsEqual(dataSet_1, dataSet_2)
            
            # Then
            expect_true(result)
          })

test_that("fastIsEqual: different integers", 
          {
            # Given
            dataSet_1 <- 1
            dataSet_2 <- 2
            
            # When
            result <- fastIsEqual(dataSet_1, dataSet_2)
            
            # Then
            expect_false(result)
          })

test_that("fastIsEqual: two data.table", 
          {
            # Given
            data("messy_adult")
            
            # When
            result <- fastIsEqual(copy(messy_adult), copy(messy_adult))
            
            # Then
            expect_true(result)
          })

## fastIsBijection
# -----------------
test_that("private function: fastIsBijection: true bijection", 
          {
            # Given
            dataSet <- data.table(int_code = 1:26,
                                  letters = LETTERS)
            
            # When
            result <- fastIsBijection(dataSet[["int_code"]], dataSet[["letters"]])
            
            # Then
            expect_true(result)
          })

test_that("private function: fastIsBijection: not bijection", 
          {
            # Given
            dataSet <- data.table(alternative_1_2 = rep(c(1, 2), 13),
                                  letters = LETTERS)
            
            # When
            result <- fastIsBijection(dataSet[["alternative_1_2"]], dataSet[["letters"]])
            
            # Then
            expect_false(result)
          })

test_that("private function: fastIsBijection: Tricky non-bijection on the threshold", 
          {
            # Given
            df <- data.frame(col1 = c(rep(0, 9), 1), col2 = c(rep(1, 9), 1))
            
            # When
            
            # Then
            expect_true(fastIsBijection(adult[["education"]], adult[["education_num"]]))
            expect_false(fastIsBijection(adult[["education"]], adult[["income"]]))
            expect_false(fastIsBijection(df[["col1"]], df[["col2"]]))
          })

## fastMaxNbElt
# -------------
test_that("private function: fastMaxNbElt", 
          {
            # Given
            dataSet <- sample(1:5, 100, replace = TRUE)
            max_number_unique_elt <- 1
            
            # When 
            result <- fastMaxNbElt(dataSet, max_number_unique_elt)
            
            # Then
            expect_false(result)
            expect_false(fastMaxNbElt(sample(1:5, 100, replace = TRUE), 4))
            expect_true(fastMaxNbElt(sample(1:5, 100, replace = TRUE), 5))
          })

test_that("private function: fastMaxNbElt", 
          {
            # Given
            dataSet <- sample(1:5, 100, replace = TRUE)
            max_number_unique_elt <- 4
            
            # When 
            result <- fastMaxNbElt(dataSet, max_number_unique_elt)
            
            # Then
            expect_false(result)
            expect_true(fastMaxNbElt(sample(1:5, 100, replace = TRUE), 5))
          })

test_that("private function: fastMaxNbElt", 
          {
            # Given
            dataSet <- sample(1:5, 100, replace = TRUE)
            max_number_unique_elt <- 5
            
            # When 
            result <- fastMaxNbElt(dataSet, max_number_unique_elt)
            
            # Then
            expect_true(result)
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
