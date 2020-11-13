context("test_which_functions.R")
requireNamespace("data.table")
verbose <- TRUE

## which_are_constant
#------------------
test_that("which_are_constant: should find string constant column", {
    # Given
    data_set <- data.table(constantCol = rep("a", 100), nonConstantCol = rnorm(100))

    # When
    constant_cols <- which_are_constant(data_set, verbose = verbose)

    # Then
    expect_equal(constant_cols, 1L)
})

## which_are_in_double
#------------------
test_that("which_are_in_double: should give col 2 and 3 on a 3 column matrix full of a constant", {
    # Given
    given_matrix <- matrix(1, nrow = 1e3, ncol = 3)

    # When
    double_columns <- which_are_in_double(given_matrix, verbose = verbose)

    # Then
    expect_equal(double_columns, c(2L, 3L))
})


test_that("which_are_in_double: should give 3 on a 3 column matrix full of a constant with some NA on col 2", {
    # Given
    given_matrix <- matrix(1, nrow = 1e3, ncol = 3)
    given_matrix[1, 2] <- NA

    # When
    double_columns <- which_are_in_double(given_matrix, verbose = verbose)

    # Then
    expect_equal(double_columns, c(3L))
})


test_that("which_are_in_double: should give 2 on a 3 column matrix full of a
          constant with same NA on col 1 and col 2", {
    # Given
    given_matrix <- matrix(1, nrow = 1e3, ncol = 3)
    given_matrix[1, 1] <- NA
    given_matrix[1, 2] <- NA

    # When
    double_columns <- which_are_in_double(given_matrix, verbose = verbose)

    # Then
    expect_equal(double_columns, c(2L))
})

test_that("which_are_in_double: should give nothing on a single column matrix", {
    # Given
    given_matrix <- matrix(1, nrow = 1e3, ncol = 1)

    # When
    double_columns <- which_are_in_double(given_matrix, verbose = verbose)

    # Then
    expect_equal(double_columns, integer())
})


## which_are_bijection
# ------------------
data("adult")


test_that("which_are_bijection: adult set contains on bijection eduction and education_num should be spotted", {
    # Given
    data("adult")
    cols <- c("education", "education_num")

    # When
    bijection_cols <- which_are_bijection(adult[, cols], verbose = verbose)

    # Then
    expect_equal(bijection_cols, 2)
})

test_that("which_are_bijection: adult set contains on bijection eduction and education_num should be spotted.
          When second one is asked to be kept, the other one is returned", {
    # Given
    data("adult")
    cols <- c("education", "education_num")

    # When
    bijection_cols <- which_are_bijection(adult[, cols], keep_cols = "education_num", verbose = verbose)

    # Then
    expect_equal(bijection_cols, 1)
})

test_that("which_are_bijection: one column data set has no bijection", {
    # Given
    data("adult")
    cols <- c("education")

    # When
    bijection_cols <- which_are_bijection(adult[, cols, drop = FALSE], verbose = verbose)

    # Then
    expect_equal(bijection_cols, integer())
})

## which_are_included
# ------------------
test_that("which_are_included: education and education_num are duplicated so should be spoted as included", {
    # Given
    data("messy_adult")
    messy_adult <- messy_adult[1 : 500, ] # reduce it to compute faster
    cols <- c("education", "education_num")

    # When
    included_cols <- which_are_included(messy_adult[, c(cols), with = FALSE], verbose = verbose)

    # Then
    expect_equal(included_cols, 1L)
})

test_that("which_are_included: education and education_num are duplicated so should be spoted as
          included even if education is specified in keep cols", {
    # Given
    data("messy_adult")
    messy_adult <- messy_adult[1 : 500, ] # reduce it to compute faster
    cols <- c("education", "education_num")

    # When
    included_cols <- which_are_included(messy_adult[, c(cols), with = FALSE],
                                        keep_cols = "education", verbose = verbose)

    # Then
    expect_equal(included_cols, 2L)
})

test_that("which_are_included: when a column is derivated from another, it should be spotted as included", {
    # Given
    data("messy_adult")
    messy_adult <- messy_adult[1 : 500, ] # reduce it to compute faster
    messy_adult$are_50_or_more <- messy_adult$age > 50
    cols <- c("age", "are_50_or_more")

    # When
    included_cols <- which_are_included(messy_adult[, c(cols), with = FALSE], verbose = verbose)

    # Then
    expect_equal(included_cols, 2L)
})

test_that("which_are_included: a single column set should not have included columns", {
    # Given
    data("messy_adult")
    messy_adult <- messy_adult[1 : 500, ] # reduce it to compute faster
    messy_adult$are_50_or_more <- messy_adult$age > 50
    cols <- c("age")

    # When
    included_cols <- which_are_included(messy_adult[, c(cols), with = FALSE], verbose = verbose)

    # Then
    expect_null(included_cols)
})

test_that("which_are_included: when a column with unique value on each row is added, all other column are included", {
    # Given
    data("messy_adult")
    messy_adult <- messy_adult[1 : 500, ] # reduce it to compute faster
    messy_adult$id <- seq_len(nrow(messy_adult)) # build id
    existing_cols_index <- seq_len(ncol(messy_adult))[names(messy_adult) != "id"]

    # When
    included_cols <- which_are_included(messy_adult, verbose = verbose)

    # Then
    expect_equal(included_cols, existing_cols_index)
})

test_that("which_are_included: when a column with unique value on each row is added,
          all other column are included even if id is 1st col (order doesn't mawtter)", {
    # Given
    data("messy_adult")
    messy_adult <- messy_adult[1 : 500, ] # reduce it to compute faster
    messy_adult$id <- seq_len(nrow(messy_adult)) # build id
    setcolorder(messy_adult, c("id", setdiff(names(messy_adult), "id")))
    existing_cols_index <- seq_len(ncol(messy_adult))[names(messy_adult) != "id"]

    # When
    included_cols <- which_are_included(messy_adult, verbose = verbose)

    # Then
    expect_equal(included_cols, existing_cols_index)
})