context("test_generateFromFactor.R")
requireNamespace("data.table")
verbose <- TRUE
## generateFromFactor
# -------------------
data("messy_adult")
store_ncol <- ncol(messy_adult)
n_factor <- sum(sapply(messy_adult, is.factor))
messy_adult <- generateFromFactor(messy_adult, cols = "type_employer")

test_that("generateFromFactor: don't drop: ",
          {
            expect_equal(ncol(messy_adult), store_ncol + 3)
          })
		  
data("messy_adult")
store_ncol <- ncol(messy_adult)
n_factor <- sum(sapply(messy_adult, is.factor))
messy_adult <- generateFromFactor(messy_adult, cols = "auto", drop = TRUE, verbose = verbose)

test_that("generateFromFactor: drop: ",
          {
            expect_equal(ncol(messy_adult), store_ncol + 2 * n_factor)
          })


## one_hot_encoder
# ----------------
data("adult")
setDT(adult)
adult$strCol = sample(c("a", "b"), nrow(adult), replace = TRUE) # Add a str to test it 


test_that("one_hot_encoder: ",
          {
            expect_equal(ncol(one_hot_encoder(adult, verbose = verbose, drop = TRUE)), 112)
          })


test_that("one_hot_encoder: expect error: ",
          {
            expect_error(one_hot_encoder(adult, type = "character"), ": type should either be ")
          })


## build_encoding
# ----------------
rm("adult") # Otherwise it doesn't really reload it :-(
data("adult")
setDT(adult)
test_that("build_encoding: ",
          {
            expect_equal(length(build_encoding(copy(adult), verbose = verbose)), 9)
            expect_equal(length(build_encoding(copy(adult), cols = "type_employer", verbose = verbose, min_frequency = 0.1)[[1]]$values), 1)# Control that only value left
          })
