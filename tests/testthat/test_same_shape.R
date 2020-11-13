context("test_same_shape.R")
requireNamespace("data.table")
verbose <- TRUE

## same_shape
#------------
test_that("same_shape: find and create missing column", {
    # Given
    reference_set <- data.table(col_1 = c(1, 2, 3),
    col_2 = c(2, 3, 4))
    data_set <- data.table(col_1 = c(1, 2))

    # When
    data_set_redone <- same_shape(data_set, reference_set, verbose = verbose)

    # Then
    expect_true("col_2" %in% names(data_set_redone))
})

test_that("same_shape: find and remove unwanted column",  {
    # Given
    reference_set <- data.table(col_1 = c(1, 2, 3))
    data_set <- data.table(col_1 = c(1, 2),
    col_2 = c(2, 3))

    # When
    data_set_redone <- same_shape(data_set, reference_set, verbose = verbose)

    # Then
    expect_false("col_2" %in% names(data_set_redone))
})

test_that("same_shape: find and and transform to numeric misstyped character col", {
    # Given
    reference_set <- data.table(col_1 = c(1, 2, 3))
    data_set <- data.table(col_1 = c("1", "2"))

    # When
    data_set_redone <- same_shape(data_set, reference_set, verbose = verbose)

    # Then
    expect_true(is.numeric(data_set_redone[["col_1"]]))
})

test_that("same_shape: find and and transform to POSIXct misstyped character col", {
    # Given
    reference_set <- data.table(col_1 = as.POSIXct(c("2018-01-31", "2019-07-12")))
    data_set <- data.table(col_1 = c("2018-02-12", "2018-09-26"))

    # When
    data_set_redone <- same_shape(data_set, reference_set, verbose = verbose)

    # Then
    expect_true(is.POSIXct(data_set_redone[["col_1"]]))
})

test_that("same_shape: find and and add missing levels on factors", {
    # Given
    reference_set <- data.table(col_1 = as.factor(c(1, 2, 3)))
    data_set <- data.table(col_1 = as.factor(c(1, 2)))

    # When
    data_set_redone <- same_shape(data_set, reference_set, verbose = verbose)

    # Then
    expect_true("3" %in% levels(data_set_redone[["col_1"]]))
})

test_that("same_shape: find and and remove unwanted levels on factors", {
    # Given
    reference_set <- data.table(col_1 = as.factor(c(1, 2)))
    data_set <- data.table(col_1 = as.factor(c(1, 2, 3)))

    # When
    data_set_redone <- same_shape(data_set, reference_set, verbose = verbose)

    # Then
    expect_false("3" %in% levels(data_set_redone[["col_1"]]))
})

test_that("same_shape: throw warning when column is in a weird class that we don't know how to transform", {
    # Given
    reference_set <- data.table(col_1 = c(1, 2, 3))
    class(reference_set[["col_1"]]) <- "a_weird_class"
    data_set <- data.table(col_1 = c(1, 2, 3))

    # When and Then
    expect_warning(same_shape(data_set, reference_set, verbose = verbose),
    " and i don't know how to transform it.")
})

test_that("same_shape: when column is in a weird class if method to transform exist", {
    # Given
    reference_set <- data.table(col_1 = c(1, 2, 3))
    class(reference_set[["col_1"]]) <- "weird_class"
    as.weird_class <- function(x) {
      class(x) <- "weird_class"; return(x)
    }
    attach(list(as.weird_class = as.weird_class))
    data_set <- data.table(col_1 = c(1, 2, 3))

    # When
    data_set_redone <- same_shape(data_set, reference_set, verbose = verbose)

    # Then
    expect_equal(class(data_set_redone[["col_1"]]), "weird_class")

    # Clean up
    detach(list(as.weird_class = as.weird_class))
})

test_that("same_shape: throw warning when column is in a weird class if method to transform
          exist but doesn't perform correctly.", {
    # Given
    reference_set <- data.table(col_1 = c(1, 2, 3))
    class(reference_set[["col_1"]]) <- "weird_class"
    as.weird_class <- function(x) {
      x
    }
    attach(list(as.weird_class = as.weird_class))
    data_set <- data.table(col_1 = c(1, 2, 3))

    # When and Then
    expect_warning(same_shape(data_set, reference_set, verbose = verbose),
    ": transformation didn't work. Please control that function ")
})

# test df
test_that("same_shape: transform shape into numerical matrix", {
    # Given
    data("adult")
    adult <- adult[1 : 150, ] # reduce it to save time
    adult2 <- copy(adult)
    setDT(adult2)
    adult_num <- shape_set(adult2, final_form = "numerical_matrix", verbose = FALSE)

    # When
    adult_reshaped <- same_shape(adult, adult_num, verbose = verbose)

    # Then
    expect_true(is.matrix(adult_reshaped))
})


test_that("same_shape: transform shape into data.frame", {
    # Given
    data_set_1 <- data.frame(col = 1 : 10)
    data_set_2 <- copy(data_set_1)
    setDT(data_set_2)

    # When
    reshaped_data_set_2 <- same_shape(data_set_2, data_set_1, verbose = verbose)

    # Then
    expect_true(is.data.frame(reshaped_data_set_2))
})

# Internal class check function
test_that("is_class_dataframe: matrix and data.table are not data.frame", {
    # Given
    some_class <- "matrix"
    future_matrix_class <- c("matrix", "array")
    data_table_class <- c("data.table", "data.frame")

    # When and Then
    expect_false(is_class_dataframe(some_class))
    expect_false(is_class_dataframe(future_matrix_class))
    expect_false(is_class_dataframe(data_table_class))
})

test_that("is_class_dataframe: data.frame is data.frame", {
    # Given
    some_class <- "data.frame"

    # When and Then
    expect_true(is_class_dataframe(some_class))
})

test_that("is_class_matrix: data.frame and data.table are not matrix", {
    # Given
    some_class <- "data.frame"
    data_table_class <- c("data.table", "data.frame")

    # When and Then
    expect_false(is_class_matrix(some_class))
    expect_false(is_class_matrix(data_table_class))
})

test_that("is_class_matrix: matrix and future matrix is matrix", {
    # Given
    some_class <- "matrix"
    future_matrix_class <- c("matrix", "array")

    # When and Then
    expect_true(is_class_matrix(some_class))
    expect_true(is_class_matrix(future_matrix_class))
})
