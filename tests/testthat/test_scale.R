requireNamespace("data.table")
verbose <- TRUE
## build_scales
# -------------

data(adult)
scales <- build_scales(adult, cols = "auto", verbose = verbose)

test_that("build_scale: ",
          {
            expect_equal(length(scales), 6)
            expect_equal(length(scales[[1]]), 2)
            })


## fastScale
# ----------
# Without setting scale
result <- fastScale(copy(adult), scales = NULL, verbose = verbose)

test_that("fastScale: ",
          {
            expect_true(mean(result$age) < 10^-2)
            expect_true(sd(result$age) - 1 < 10^-2)
            expect_error(fastScale(result, way = "unscale", verbose = verbose), ": to unscale, scales must be feeded by user.")
          })

# with setting scale
data(adult)

store_mean = mean(adult$age)
store_sd = sd(adult$age)
scales <- build_scales(adult, cols = "age", verbose = verbose)

result <- fastScale(copy(adult), scales = scales, verbose = verbose)
unscaled <- fastScale(copy(result), scales = scales, way = "unscale", verbose = verbose)

test_that("fastScale: ",
          {
            expect_true(mean(result$age) < 10^-2) # compared to 10^-2 to allow a small gap
            expect_true(sd(result$age) - 1 < 10^-2)
            expect_true(abs(mean(unscaled$age) - store_mean) < 10^-2)
            expect_true(abs(sd(unscaled$age) - store_sd) < 10^-2)
          })


## control_scale_way
# ------------------
test_that("Private function: control_scale_way",
          {
            expect_error(control_scale_way(1), ": way should be a character either 'scale' or 'unscale'")
            expect_error(control_scale_way("a"), ": way should either be 'scale' or 'unscale'")
            
          })
