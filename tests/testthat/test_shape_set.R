## Documentation for unit testing
#--------------------------------
# http://r-pkgs.had.co.nz/tests.html
# http://stat545.com/packages05_foofactors-package-02.html
context("test_shape_set.R")
verbose <- TRUE
## prepare_set
#----------------
test_that("shape_set: test class of results: shape set shape to data.table by default", {
    # Given
    data("adult")

    # When
    adult_shaped <- shape_set(copy(adult), verbose = verbose)

    # Then
    expect_true(is.data.table(adult_shaped))
    expect_true(is.matrix(shape_set(copy(adult), final_form = "numerical_matrix")))
})

test_that("shape_set: test class of results: shape set shape to matrix if asked for numerical_matrix", {
    # Given
    data("adult")

    # When
    adult_shaped <- shape_set(copy(adult), verbose = verbose, final_form = "numerical_matrix")

    # Then
    expect_true(is.matrix(adult_shaped))
})

test_that("shape_set: shape set should encode logical into 0, 1", {
    # Given
    data_set <- data.table(logical_col = sample(c(TRUE, FALSE), 100, replace = TRUE))

    # When
    data_set_shaped <- shape_set(data_set, verbose = verbose)

    # Then
    expect_true(all(data_set_shaped[["logical_col"]] %in% c(0, 1)))
})


## set_as_numeric_matrix
# --------------------
test_that("set_as_numeric_matrix: throw error if data set contains some col that are not numeric, logical or factor", {
    # Given
    data_set <- data.table(date_col = as.Date("2018-01", "2018-01-31"))

    # When and Then
    expect_error(set_as_numeric_matrix(data_set),
    "some columns are not numerical/logical/factor. Consider using shape_set()")
})

test_that("set_as_numeric_matrix: one hot encode factors", {
    # Given
    data_set <- data.table(factor_col = as.factor(c("a", "b", "c")))

    # When
    data_set_as_numeric_matrix <- set_as_numeric_matrix(data_set, intercept = FALSE, all_cols = TRUE, sparse = FALSE)

    # Then
    expect_equal(ncol(data_set_as_numeric_matrix), uniqueN(data_set[["factor_col"]]))
})

test_that("set_as_numeric_matrix: one hot encode factors with all_cols = false nth column is not represented", {
    # Given
    data_set <- data.table(factor_col = as.factor(c("a", "b", "c")),
    factor_col_2 = as.factor(c("a", "b", "c")))

    # When
    data_set_as_numeric_matrix <- set_as_numeric_matrix(data_set, intercept = FALSE, all_cols = FALSE, sparse = FALSE)

    # Then
    expect_equal(ncol(data_set_as_numeric_matrix), 5)
})

test_that("set_as_numeric_matrix: if intercept is true add a column first one with all ones", {
    # Given
    data_set <- data.table(factor_col = 1 : 10)

    # When
    data_set_as_numeric_matrix <- set_as_numeric_matrix(data_set, intercept = TRUE, all_cols = TRUE, sparse = FALSE)

    # Then
    expect_equal(ncol(data_set_as_numeric_matrix), ncol(data_set) + 1)
    expect_true(all(data_set_as_numeric_matrix[, 1] == 1))
})

test_that("set_as_numeric_matrix: one hot encode factors with allCOls at false nth column is not represented", {
    # Given
    data_set <- data.table(factor_col = as.factor(c("a", "b", "c")),
    factor_col_2 = as.factor(c("a", "b", "c")))

    # When
    data_set_as_numeric_matrix <- set_as_numeric_matrix(data_set, intercept = FALSE,
                                                        all_cols = FALSE, sparse = TRUE)

    # Then
    expect_equal(ncol(data_set_as_numeric_matrix), 5)
})

test_that("set_as_numeric_matrix: trhow error if not called on data.table", {
    # Given
    wrong_frame <- "cszdez"

    # When and Then
    expect_error(set_as_numeric_matrix(wrong_frame),
    "set_as_numeric_matrix: data_set is not a data.table")
})