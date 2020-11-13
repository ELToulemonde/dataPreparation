context("test_numerics_manipulations.R")
requireNamespace("data.table")
verbose <- TRUE

## find_and_transform_numerics
#--------------------------
test_that("find_and_transform_numerics: find and transform to numeric columns that are hidden
          in string wheter they have decimal separator ',' or '.'", {
    # Given
    data_set <- data.table(col1 = c("1.2", "1.3", "1.2", "1", "6"),
    col2 = c("1,2", "1,3", "1,2", "1", "6"))

    # When
    data_transformed <- find_and_transform_numerics(data_set, n_test = 5, verbose = verbose)

    # Then
    expect_true(is.numeric(data_transformed[["col1"]]))
    expect_true(is.numeric(data_transformed[["col2"]]))
})


test_that("find_and_transform_numerics: doesn't transform to numeric character cols", {
    # Given
    data_set <- data.table(character_col = c("A", "B"))

    # When
    data_transformed <- find_and_transform_numerics(data_set, n_test = 2, verbose = verbose)

    # Then
    expect_true(is.character(data_transformed[["character_col"]]))
})

## identify_numerics
# -----------------
test_that("private function identify_numerics: find numerics wheter they have decimal separator ',' or '.'", {
    # Given
    data_set <- data.table(col1 = c("1.2", "1.3", "1.2", "1", "6"),
    col2 = c("1,2", "1,3", "1,2", "1", "6"))

    # When
    numeric_cols <- identify_numerics(data_set, n_test = 5, verbose = verbose)

    # Then
    expect_equal(2, length(numeric_cols))
    expect_equal("col1", numeric_cols$dont_strip)
    expect_equal("col2", numeric_cols$strip)
    expect_identical(identify_numerics(data_set, cols = list(), n_test = 5, verbose = verbose),
                     list(dont_strip = NULL, strip = NULL))
})

test_that("private function identify_numerics: if told to do nothing, do nothing.", {
    # Given
    data_set <- data.table(col1 = c("1.2", "1.3", "1.2", "1", "6"))

    # When
    numeric_cols <- identify_numerics(data_set, cols = list(), n_test = 5, verbose = verbose)

    # Then
    expect_identical(numeric_cols, list(dont_strip = NULL, strip = NULL))
})

## identify_numerics_formats
# ------------------------
test_that("private function: identify_numerics_formats: give notstrip when numeric col hiden in character
          with '.' decimal separator is thrown", {
    # Given
    data_set <- data.table(col = c("1.2", "1.3", "1.2", "1", "6"))

    # When
    result <- identify_numerics_formats(data_set$col)

    # Then
    expect_equal(NUMERIC_COL_NOT_TO_STRIP, result)
})

test_that("private function: identify_numerics_formats: give strip when numeric col hiden in character with ','
          decimal separator is thrown", {
    # Given
    data_set <- data.table(col = c("1,2", "1,3", "1,2", "1", "6"))

    # When
    result <- identify_numerics_formats(data_set$col)

    # Then
    expect_equal(NUMERIC_COL_TO_STRIP, result)
})

test_that("private function: identify_numerics_formats: give 'Not a numeric' when col doesn't contain hidden numeric", {
    # Given
    data_set <- data.table(col = LETTERS)

    # When
    result <- identify_numerics_formats(data_set$col)

    # Then
    expect_equal("Not a numeric", result)
})

test_that("private function: identify_numerics_formats: should throw error when called on not character col", {
    # Given
    data_set <- data.table(col = factor(c(1, 2, 3)))

    # When and Then
    expect_error(identify_numerics_formats(data_set$col),
    "identify_numerics_formats: data_set should be some characters")
})

## as.numeric_strip
# ----------------
test_that("private function as.numeric_strip: should convert character containing a numeric with ','
          decimal seprator into correct numeric", {
    # Given
    char_num <- "1,2"
    expected_result <- 1.2

    # When
    result <- as.numeric_strip(char_num)
    expect_equal(expected_result, result)
})
