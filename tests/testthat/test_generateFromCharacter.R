context("test_generateFromCharacter.R")
requireNamespace("data.table")
verbose <- TRUE
## generateFromCharacter
# -------------------
data("messy_adult")
messy_adult <- unFactor(messy_adult, verbose = FALSE)
store_ncol <- ncol(messy_adult)
n_char <- sum(sapply(messy_adult, is.character))
messy_adult <- generateFromCharacter(messy_adult, cols = "mail")

test_that("generateFromCharacter: don't drop: ",
          {
            expect_equal(ncol(messy_adult), store_ncol + 3)
          })

data("messy_adult")
messy_adult <- unFactor(messy_adult, verbose = FALSE)
store_ncol <- ncol(messy_adult)
n_char <- sum(sapply(messy_adult, is.character))
messy_adult <- generateFromCharacter(messy_adult, cols = "auto", drop = TRUE)

test_that("generateFromCharacter: drop: ",
          {
            expect_equal(ncol(messy_adult), store_ncol + 2 * n_char)
          })
