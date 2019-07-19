context("test_rowFiltering.R")
requireNamespace("data.table")
verbose <- TRUE

## remove_sd_outlier
# -----------------
test_that("remove_sd_outlier: should remove value that are to extreme",
          {
            # Given
            col_vals <- runif(1000)
            col_mean <- mean(col_vals)
            col_sd <- sd(col_vals)
            extrem_val <- col_mean + 6 * col_sd
            dataSet <- data.table(num_col = c(col_vals, extrem_val))
            
            # When
            dataSet <- remove_sd_outlier(dataSet, cols = "auto", n_sigmas = 3, verbose = verbose)
            
            # Then
            expect_false(extrem_val %in% dataSet[["num_col"]])
          })

test_that("remove_sd_outlier: edge case, value shoudl be kept if it is equal to mean + n_sigmas * sd",
          {
            # Given
            col_vals <- runif(1000)
            col_mean <- mean(col_vals)
            col_sd <- sd(col_vals)
            extrem_val <- col_mean + 3 * col_sd
            dataSet <- data.table(num_col = c(col_vals, extrem_val))
            
            # When
            dataSet <- remove_sd_outlier(dataSet, cols = "auto", n_sigmas = 3, verbose = verbose)
            
            # Then
            expect_true(extrem_val %in% dataSet[["num_col"]])
          })
## remove_rare_categorical
# ------------------------
test_that("remove_rare_categorical: should remove categorical that happen less than 1% of time",
          {
            # Given
            dataSet <- data.table(cat_col = c(sample(c("A", "B"), 1000, replace=TRUE), "C"))
            n_rows <- nrow(dataSet)
            # When
            dataSet <- remove_rare_categorical(dataSet, cols = "cat_col", 
                                               threshold = 0.01, verbose = verbose)
            
            # Then
            expect_false("C" %in% dataSet[["cat_col"]])
            expect_equal(nrow(dataSet), n_rows - 1)
          })


## remove_percentile_outlier
# --------------------------
test_that("remove_percentile_outlier: should remove value that are to extreme",
          {
            # Given
            dataSet <- data.table(num_col = 1:100)
            
            # When
            dataSet <- remove_percentile_outlier(dataSet, cols = "auto", percentile = 1, verbose = verbose)
            
            # Then
            expect_false(1 %in% dataSet[["num_col"]])
          })

test_that("remove_percentile_outlier: edge case, value shoudl be kept if it is equal to mean + n_sigmas * sd",
          {
            # Given
            dataSet <- data.table(num_col = 1:100)
            
            # When
            dataSet <- remove_percentile_outlier(dataSet, cols = "auto", percentile = 1, verbose = verbose)
            
            # Then
            expect_true(2 %in% dataSet[["num_col"]])
          })
