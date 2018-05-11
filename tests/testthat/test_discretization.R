context("test_discretization.R")
verbose = TRUE
## build_bins
# ---------------
data(messy_adult)
result1 <- build_bins(copy(messy_adult), cols = "age", n_bins = 9, type = "equal_width", verbose = verbose)
result2 <- build_bins(copy(messy_adult), cols = "age", n_bins = 10, type = "equal_freq", verbose = verbose)
result3 <- build_bins(copy(messy_adult), cols = "auto", n_bins = 10, type = "equal_freq", verbose = verbose)
result4 <- build_bins(copy(messy_adult), cols = "auto", n_bins = 10, type = "equal_width", verbose = verbose)
result5 <- build_bins(copy(messy_adult), cols = "education", n_bins = 10, type = "equal_width", verbose = verbose)
test_that("build_bins: ",
          {
            expect_equal(length(unique(result1$age)), 9 + 1)
            expect_equal(length(unique(result2$age)), 10 + 1)
            expect_error(build_bins(copy(messy_adult), cols = "auto", n_bins = 10, type = "aa", verbose = verbose), ": type should either be equal_width or equal_freq")
          })

## fastDiscretization
# -------------------

data("adult")
adult$age[1] <- NA # add a NA
adult <- fastDiscretization(adult, bins = NULL, verbose = verbose)
test_that("fastDiscretization: ",
          {
            expect_equal(sum(sapply(adult, is.numeric)), 0)
          })
		  
## equal_width_splits
# -------------------
test_that("Private function equal_width_split: ",
          {
            expect_identical(equal_width_splits(c(1, 2, 3), n_bins = 2), c(1, 2, 3))
            expect_identical(equal_width_splits(c(1, 2, 2.1, 2.2, 3), n_bins = 2), c(1, 2, 3))
            expect_identical(equal_width_splits(1, n_bins = 10), 1)
            expect_error(equal_width_splits("a", 2), "dataSet should be a vector of numerics and n_bins a numeric.")
            expect_error(equal_width_splits(c(1,2), "a"), "dataSet should be a vector of numerics and n_bins a numeric.")
          })


## equal_freq_splits
# -------------------
test_that("Private function equal_width_split: ",
          {
            expect_identical(equal_freq_splits(c(1, 2, 3), n_bins = 2), c(-Inf, 2, +Inf))
            expect_identical(equal_freq_splits(c(1, 2, 2.1, 2.2, 3), n_bins = 2), c(-Inf, 2.1, +Inf))
            expect_identical(equal_freq_splits(1, n_bins = 10), c(-Inf, 1, +Inf))
            expect_error(equal_freq_splits("a", 2), "dataSet should be a vector of numerics and n_bins a numeric.")
            expect_error(equal_freq_splits(c(1,2), "a"), "dataSet should be a vector of numerics and n_bins a numeric.")
          })


## build_splits_names
# -------------------
test_that("Private function build_splits_names: ",
          {
            expect_identical(build_splits_names(c(0, 1, 2)), c("[0, 1[", "[1, 2]"))
            expect_identical(build_splits_names(c(-Inf, 2, +Inf)), c("]-Inf, 2[", "[2, +Inf["))
          })
