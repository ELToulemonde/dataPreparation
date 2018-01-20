requireNamespace("data.table")
verbose <- TRUE
## build_scales
# -------------
data(adult)
scales <- build_scales(adult, cols = "auto", verbose = TRUE)

test_that("build_scale: ",
          {
            expect_equal(length(scales), 6)
            expect_equal(length(scales[[1]]), 2)
            })


## fastScale
# ----------
adult <- fastScale(adult, scales = NULL, verbose = TRUE)

test_that("fastScale: ",
          {
            expect_true(mean(adult$age) < 10^-2)
            expect_true(abs(sd(adult$age) - 1) < 10^-2)
          })

## fastUnScale
# ----------
adult <- fastUnScale(adult, scales = scales, verbose = TRUE)

test_that("fastUnScale: ",
          {
            expect_true(mean(adult$age) - scales[['age']][['mean']] < 10^-2)
            expect_true(sd(adult$age) - scales[['age']][['sd']] < 10^-2)
          })