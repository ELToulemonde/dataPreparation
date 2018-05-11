context("test_scale.R")
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


## .control_scale_way
# -------------------
test_that("Private function: .control_scale_way",
          {
            expect_error(.control_scale_way(1), ": way should be a character either 'scale' or 'unscale'")
            expect_error(.control_scale_way("a"), ": way should either be 'scale' or 'unscale'")
          })


## .build_and_control_scale
# -------------------------
scales <- build_scales(adult, cols = "age", verbose = FALSE)
test_that("Private function: .build_and_control_scale",
          {
            expect_equal(length(.build_and_control_scale(NULL, adult, "scale")), 6)
            expect_error(.build_and_control_scale(NULL, adult, "unscale"), ": to unscale, scales must be feeded by user.")
            expect_error(.build_and_control_scale("a", adult, "unscale"),  ": scales should be a named list. Please build it using build_scales.")
            expect_equal(.build_and_control_scale(list(), adult, "unscale"), list())
            expect_error(.build_and_control_scale(list("a"), adult, "unscale"),  ": scales should be a named list of list having 2 elements: mean and sd. Please build it using build_scales.")
            expect_error(.build_and_control_scale(list(list("a")), adult, "unscale"),  ": scales should be a named list of list having 2 elements: mean and sd. Please build it using build_scales.")
            expect_error(.build_and_control_scale(list(list(mean = "a", sd = 5)), adult, "unscale"),  ": scales should be a named list of list having 2 numeric elements: mean and sd. Please build it using build_scales.")
            expect_identical(.build_and_control_scale(scales, adult, "scale"), scales)
          })