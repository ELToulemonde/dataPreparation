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
messy_adult <- generateFromFactor(messy_adult, cols = "auto", drop = TRUE)

test_that("generateFromFactor: drop: ",
          {
            expect_equal(ncol(messy_adult), store_ncol + 2 * n_factor)
          })
		  
		  
## one_hot_encoder
# ----------------
data(adult)
res <- one_hot_encoder(adult, "auto", drop = TRUE, verbose = verbose)
test_that("one_hot_encoder: ",
          {
            expect_equal(ncol(res), 110)
          })