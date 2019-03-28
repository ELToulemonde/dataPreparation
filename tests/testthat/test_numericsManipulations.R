context("test_numericsManipulations.R")
requireNamespace("data.table")
verbose <- TRUE

## findAndTransformNumerics
#--------------------------
test_that("findAndTransformNumerics: find and transform to numeric columns that are hidden in string wheter they have decimal separator ',' or '.'",
          {
            # Given
            dataSet <- data.table(col1 = c("1.2", "1.3", "1.2", "1", "6"), 
                                  col2 = c("1,2", "1,3", "1,2", "1", "6"))
            
            # When
            data_transformed <- findAndTransformNumerics(dataSet, n_test = 5, verbose = verbose)
            
            # Then
            expect_true(is.numeric(data_transformed[["col1"]]))
            expect_true(is.numeric(data_transformed[["col2"]]))
          })


test_that("findAndTransformNumerics: doesn't transform to numeric character cols",
          {
            # Given
            dataSet <- data.table(character_col = c("A", "B"))
            
            # When
            data_transformed <- findAndTransformNumerics(dataSet, n_test = 2, verbose = verbose)
            
            # Then
            expect_true(is.character(data_transformed[["character_col"]]))
          })

## identifyNumerics
# -----------------
test_that("private function identifyNumerics: find numerics wheter they have decimal separator ',' or '.'",
          {
            # Given
            dataSet <- data.table(col1 = c("1.2", "1.3", "1.2", "1", "6"), 
                                  col2 = c("1,2", "1,3", "1,2", "1", "6"))
            
            # When
            numeric_cols <- identifyNumerics(dataSet, n_test = 5, verbose = verbose)
            
            # Then
            expect_equal(2, length(numeric_cols))
            expect_equal("col1", numeric_cols$dont_strip)
            expect_equal("col2", numeric_cols$strip)
            expect_identical(identifyNumerics(dataSet, cols = list(), n_test = 5, verbose = verbose), list(dont_strip = NULL, strip = NULL)) # Control that if told to do nothing, do nothing.
          })

test_that("private function identifyNumerics: if told to do nothing, do nothing.",
          {
            # Given
            dataSet <- data.table(col1 = c("1.2", "1.3", "1.2", "1", "6"))
            
            # When
            numeric_cols <- identifyNumerics(dataSet, cols = list(), n_test = 5, verbose = verbose)
            
            # Then
            expect_identical(numeric_cols, list(dont_strip = NULL, strip = NULL))
          })

## identifyNumericsFormats
# ------------------------	
test_that("private function: identifyNumericsFormats: give notstrip when numeric col hiden in character with '.' decimal separator is thrown",
          {
            # Given
            dataSet <- data.table(col = c("1.2", "1.3", "1.2", "1", "6"))
            
            # When
            result <- identifyNumericsFormats(dataSet$col)
            
            # Then
            expect_equal("notstrip", result)
          })

test_that("private function: identifyNumericsFormats: give strip when numeric col hiden in character with ',' decimal separator is thrown",
          {
            # Given
            dataSet <- data.table(col = c("1,2", "1,3", "1,2", "1", "6"))
            
            # When
            result <- identifyNumericsFormats(dataSet$col)
            
            # Then
            expect_equal("strip", result)
          })

test_that("private function: identifyNumericsFormats: give NULL when col doesn't contain hidden numeric",
          {
            # Given
            dataSet <- data.table(col = LETTERS)
            
            # When
            result <- identifyNumericsFormats(dataSet$col)
            
            # Then
            expect_null(result)
          })

test_that("private function: identifyNumericsFormats: should throw error when called on not character col",
          {
            # Given
            dataSet <- data.table(col = factor(c(1, 2, 3)))
            
            # When + Then
            expect_error(identifyNumericsFormats(dataSet$col), 
                         "identifyNumericsFormats: dataSet should be some characters")
          })

## as.numericStrip
# ----------------
test_that("private function as.numericStrip: should convert character containing a numeric with ',' decimal seprator into correct numeric",
          {
            # Given
            char_num <- "1,2"
            expected_result <- 1.2
            
            # When
            result <- as.numericStrip(char_num)
            expect_equal(expected_result, result)
          })
