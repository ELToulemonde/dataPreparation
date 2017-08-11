requireNamespace("data.table")
verbose <- TRUE
## unFactor
# ---------
dataSet <- data.frame(true_factor = factor(rep(c(1,2), 13)),
                      false_factor = factor(LETTERS))

test_that("unFactor: change level",
          {
            expect_equal(all(sapply(unFactor(dataSet, n_unfactor = 5, verbose = verbose), class) == c("factor", "character")), TRUE)
            expect_equal(all(sapply(unFactor(dataSet, n_unfactor = 0, verbose = verbose), class) == c("character", "character")), TRUE)
          })

dataSet <- data.frame(true_factor = factor(rep(c(1,2), 13)),
                      false_factor = factor(LETTERS))		
test_that("unFactor: level to -1",
          {
            expect_equal(all(sapply(unFactor(dataSet, n_unfactor = - 1, verbose = verbose), class) == c("factor", "factor")), TRUE)
          })

test_that("unFactor: level to error",
          {
            expect_error(unFactor(dataSet, n_unfactor = "a", verbose = verbose), ": n_unfactor should be a numeric, you provided a ")
          })