context("test_factorManipulations.R")
requireNamespace("data.table")
verbose <- TRUE
## unFactor
# ---------
test_that("unFactor: n_unfactor 2 unfactor only column with 2 unique values",
          {
            # Given
            dataSet <- data.frame(true_factor = factor(rep(c(1, 2), 13)),
                                  false_factor = factor(LETTERS))		
            
            # When
            dataSet_unfactored <- unFactor(dataSet, n_unfactor = 2, verbose = verbose)
            
            # Then
            expect_equal(lapply(dataSet_unfactored, class), list(true_factor = "factor", false_factor = "character"))
          })

test_that("unFactor: n_unfactor 0 unfactor nothing",
          {
            # Given
            dataSet <- data.frame(true_factor = factor(rep(c(1, 2), 13)),
                                  false_factor = factor(LETTERS))		
            
            # When
            dataSet_unfactored <- unFactor(dataSet, n_unfactor = 0, verbose = verbose)
            
            # Then
            expect_equal(lapply(dataSet_unfactored, class), list(true_factor = "character", false_factor = "character"))
          })

test_that("unFactor: n_unfactor to -1 unfactor all no question ask",
          {
            # Given
            dataSet <- data.frame(true_factor = factor(rep(c(1, 2), 13)),
                                  false_factor = factor(LETTERS))		
            
            # When
            dataSet_unfactored <- unFactor(dataSet, n_unfactor = - 1, verbose = verbose)
            
            # Then
            expect_equal(lapply(dataSet_unfactored, class), list(true_factor = "factor", false_factor = "factor"))
          })

test_that("unFactor: level to error",
          {
            # Given
            wrong_n_unfactor <- "a"
            
            # When + Then
            expect_error(unFactor(dataSet, n_unfactor = wrong_n_unfactor, verbose = verbose), 
                         ": n_unfactor should be a numeric, you provided a ")
          })
