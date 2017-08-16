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

test_that("generateFromFactor: warning: ",
          {
            expect_warning(generateFromFactor(messy_adult, cols = "age"), "generateFromFactor: age isn't a factor, i do nothing.")
          })

data("messy_adult")
store_ncol <- ncol(messy_adult)
n_factor <- sum(sapply(messy_adult, is.factor))
messy_adult <- generateFromFactor(messy_adult, cols = "auto", drop_cols = TRUE)

test_that("generateFromFactor: drop: ",
          {
            expect_equal(ncol(messy_adult), store_ncol + 2 * n_factor)
          })