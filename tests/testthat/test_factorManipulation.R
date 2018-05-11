context("test_factorManipulations.R")
requireNamespace("data.table")
verbose <- TRUE
## unFactor
# ---------
dataSet <- data.frame(true_factor = factor(rep(c(1,2), 13)),
                      false_factor = factor(LETTERS))

test_that("unFactor: change level",
          {
            expect_true(all(sapply(unFactor(dataSet, n_unfactor = 5, verbose = verbose), class) == c("factor", "character")))
            expect_true(all(sapply(unFactor(dataSet, n_unfactor = 0, verbose = verbose), class) == c("character", "character")))
          })

dataSet <- data.frame(true_factor = factor(rep(c(1,2), 13)),
                      false_factor = factor(LETTERS))		
test_that("unFactor: level to -1",
          {
            expect_true(all(sapply(unFactor(dataSet, n_unfactor = - 1, verbose = verbose), class) == c("factor", "factor")))
          })

test_that("unFactor: level to error",
          {
            expect_error(unFactor(dataSet, n_unfactor = "a", verbose = verbose), ": n_unfactor should be a numeric, you provided a ")
          })