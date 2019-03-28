context("test_scale.R")
requireNamespace("data.table")
verbose <- TRUE
## build_scales
# -------------
test_that("build_scale: scale compute a mean an a sd for each numeric col",
          {
            # Given
            some_values <- 1:11
            dataSet <- data.table(col_1 = some_values)
            
            # When
            scales <- build_scales(dataSet, verbose = verbose)
            
            # Then
            expect_equal(length(scales), sum(sapply(dataSet, is.numeric)))
            expect_equal(length(scales[["col_1"]]), 2)
            expect_equal(scales[["col_1"]]$mean, mean(some_values))
            expect_equal(scales[["col_1"]]$sd, sd(some_values))
            })


## fastScale
# ----------
test_that("fastScale: should be able to apply scale",
          {
            # Given
            dataSet <- data.table(col_1 = 1:10)
            scales <- build_scales(dataSet, verbose = verbose)
            
            # When
            dataSet_scaled <- fastScale(dataSet, scales = scales, verbose = verbose)
            
            # Then
            expect_true(mean(dataSet_scaled[["col_1"]]) < 10^-2) # compared to 10^-2 to allow a small gap
            expect_true(sd(dataSet_scaled[["col_1"]]) - 1 < 10^-2)
          })


test_that("fastScale: should be able to apply unscale and retrieve original values",
          {
            # Given
            dataSet <- data.table(col_1 = 1:10)
            scales <- build_scales(dataSet, verbose = verbose)
            dataSet_scaled <- fastScale(dataSet, scales = scales, verbose = verbose)
            
            # When
            dataSet_unscaled <- fastScale(dataSet_scaled, scales = scales, way = "unscale", verbose = verbose)
            
            # Then
            expect_true(sum(sum(abs(dataSet_unscaled - dataSet))) < 10^-2)
          })


## .control_scale_way
# -------------------
test_that("Private function: .control_scale_way: non character way",
          {
            # Given
            wrong_way <- 1
            
            # When + Then
            expect_error(.control_scale_way(wrong_way), 
                         ": way should be a character either 'scale' or 'unscale'")
          })

test_that("Private function: .control_scale_way: unchorect character way",
          {
            # Given
            wrong_way <- "a"
            
            # When + Then
            expect_error(.control_scale_way(wrong_way), 
                         ": way should either be 'scale' or 'unscale'")
          })


## .build_and_control_scale
# -------------------------
test_that("Private function: .build_and_control_scale: if scale is not given it is built",
          {
            # Given
            dataSet <- data.table(col_1 = 1:10)
            scales <- build_scales(dataSet, verbose = FALSE)
            
            # When
            built_scales <- .build_and_control_scale(NULL, dataSet, "scale")
            
            # Then
            expect_identical(built_scales, scales)  
          })


test_that("Private function: .build_and_control_scale: for way 'unscale' scales must be given",
          {
            # Given
            dataSet <- data.table(col_1 = 1:10)
            scales <- build_scales(dataSet, verbose = FALSE)
            
            # When + Then
            expect_error(.build_and_control_scale(NULL, dataSet, "unscale"), 
                         ": to unscale, scales must be feeded by user.")
          })


test_that("Private function: .build_and_control_scale",
          {
            # Given
            dataSet <- data.table(col_1 = 1:10)
            scales <- build_scales(dataSet, verbose = FALSE)
            
            # When + Then
            expect_error(.build_and_control_scale("a", dataSet, "unscale"),  
                         ": scales should be a named list. Please build it using build_scales.")
          })


test_that("Private function: .build_and_control_scale: for unscale empty list is acceptable scales",
          {
            # Given
            dataSet <- data.table(col_1 = 1:10)
            
            # When
            built_scales <- .build_and_control_scale(list(), dataSet, "unscale")
            
            # Then
            expect_equal(built_scales, list())            
          })

test_that("Private function: .build_and_control_scale: throw error on un conformed scales",
          {
            # Given
            dataSet <- data.table(col_1 = 1:10)
            unconform_scale_list <- list("a")
            
            # When + Then
            expect_error(.build_and_control_scale(unconform_scale_list, dataSet, "unscale"),  
                         ": scales should be a named list of list having 2 elements: mean and sd. Please build it using build_scales.")
          })

test_that("Private function: .build_and_control_scale: throw errors on second level unconformity",
          {
            # Given
            dataSet <- data.table(col_1 = 1:10)
            unconform_scales <- list(list("a"))
            
            # When + Then
            expect_error(.build_and_control_scale(unconform_scales, dataSet, "unscale"),  
                         ": scales should be a named list of list having 2 elements: mean and sd. Please build it using build_scales.")
                
          })

test_that("Private function: .build_and_control_scale: throw error if some values are not numeric",
          {
            # Given
            dataSet <- data.table(col_1 = 1:10)
            unconform_scales <- list(list(mean = "a", sd = 5))
                                     
            # When + Then 
            expect_error(.build_and_control_scale(unconform_scales, dataSet, "unscale"),  
                         ": scales should be a named list of list having 2 numeric elements: mean and sd. Please build it using build_scales.")
          })

