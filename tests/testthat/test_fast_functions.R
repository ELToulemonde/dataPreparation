context("test_fast_functions.R")
requireNamespace("data.table")
verbose <- TRUE
# fast_filter_variables
#--------------------
test_that("fast_filter_variables: functionnal test on reference set", {
    # Given
    data("messy_adult")
    messy_adult <- messy_adult[1 : 5000, ] # Make it smaller to go faster
    messy_adult$age2 <- messy_adult$age # add a double
    messy_adult$are_50_or_more <- messy_adult$age > 50 # Add an included

    # When
    messy_adult_filtered <- fast_filter_variables(messy_adult, level = 4, verbose = verbose)
    # Then
    expect_equal(ncol(messy_adult_filtered), 20)
})

# is.filtering_level
#-------------------
test_that("Private function: is.filtering_level ", {
    # Given
    wrong_level <- "a"

    # When and Then
    expect_error(is.filtering_level(level = wrong_level),
    ": level should be 1, 2, 3 or 4.")
})

# fast_round
# ----------
test_that("fast_round: ", {
    # Given
    a_matrix <- as.data.table(matrix(runif(3e3), ncol = 10))
    a_matrix[, string_column := "a string"]

    # When
    a_matrix_rounded <- fast_round(a_matrix, verbose = verbose)

    # Then
    expect_true(all(a_matrix_rounded[, 1] == round(a_matrix[, 1], 2)))
})

test_that("fast_round: ", {
    # Given
    a_matrix <- as.data.table(matrix(runif(3e3), ncol = 10))
    a_matrix[, string_column := "a string"]
    n_digits <- 1
    # When
    a_matrix_rounded <- fast_round(a_matrix, digits = n_digits, verbose = verbose)

    # Then
    expect_true(all(a_matrix_rounded[, 1] == round(a_matrix[, 1], n_digits)))
})

test_that("fast_round: ", {
    # Given
    a_matrix <- as.data.table(matrix(runif(3e3), ncol = 10))
    wrong_digits <- "a"

    # When and Then
    expect_error(fast_round(a_matrix, digits = wrong_digits, verbose = verbose),
    ": digits should be an integer")
})

# Handle NA Values
# ----------------
test_that("fast_handle_na: There are no more NAs on numercial column", {
    # Given
    data_set <- data.table(num_col = c(1, 2, 3, NA))

    # When
    data_set_without_na <- fast_handle_na(data_set)

    # Then
    expect_false(any(is.na(data_set_without_na)))
})

test_that("fast_handle_na: There are no more NAs on character column", {
    # Given
    data_set <- data.table(character_col = c("", "a", NA, "c"))

    # When
    data_set_without_na <- fast_handle_na(data_set)

    # Then
    expect_false(any(is.na(data_set_without_na)))
})

test_that("fast_handle_na: There are no more NAs logical column", {
    # Given
    data_set <- data.table(logical_factor = c(TRUE, NA, FALSE, NA))

    # When
    data_set_without_na <- fast_handle_na(data_set)

    # Then
    expect_false(any(is.na(data_set_without_na)))
})

test_that("fast_handle_na: There are no more NAs on factor col", {
    # Given
    data_set <- data.table(factor_col = as.factor(c("", "a", NA, "c")))

    # When
    data_set_without_na <- fast_handle_na(data_set)

    # Then
    expect_false(any(is.na(data_set_without_na)))
})


# fast_is_equal
#------------
data("messy_adult")
test_that("fast_is_equal: different length objects", {
    # Given
    data_set_1 <- 1 : 9
    data_set_2 <- 1 : 10

    # When
    result <- fast_is_equal(data_set_1, data_set_2)

    # Then
    expect_false(result)
})

test_that("fast_is_equal: different content type", {
    # Given
    data_set_1 <- c(1, 2, 3)
    data_set_2 <- c("A", "B", "C")

    # When
    result <- fast_is_equal(data_set_1, data_set_2)

    # Then
    expect_false(result)
})

test_that("fast_is_equal: same vector", {
    # Given
    data_set_1 <- 1 : 10
    data_set_2 <- 1 : 10

    # When
    result <- fast_is_equal(data_set_1, data_set_2)

    # Then
    expect_true(result)
})

test_that("fast_is_equal: same long vector", {
    # Given
    data_set_1 <- 1 : 1001
    data_set_2 <- 1 : 1001

    # When
    result <- fast_is_equal(data_set_1, data_set_2)

    # Then
    expect_true(result)
})

test_that("fast_is_equal: same character vector", {
    # Given
    data_set_1 <- LETTERS
    data_set_2 <- LETTERS

    # When
    result <- fast_is_equal(data_set_1, data_set_2)

    # Then
    expect_true(result)
})

test_that("fast_is_equal: same integer", {
    # Given
    data_set_1 <- 1
    data_set_2 <- 1

    # When
    result <- fast_is_equal(data_set_1, data_set_2)

    # Then
    expect_true(result)
})

test_that("fast_is_equal: different integers", {
    # Given
    data_set_1 <- 1
    data_set_2 <- 2

    # When
    result <- fast_is_equal(data_set_1, data_set_2)

    # Then
    expect_false(result)
})

test_that("fast_is_equal: two data.table", {
    # Given
    data("messy_adult")

    # When
    result <- fast_is_equal(copy(messy_adult), copy(messy_adult))

    # Then
    expect_true(result)
})

test_that("fast_is_equal: two data.table not equals", {
    # Given
    data("messy_adult")
    messy_adult2 <- copy(messy_adult)
    messy_adult2$age[2] <- 10

    # When
    result <- fast_is_equal(copy(messy_adult), messy_adult2)

    # Then
    expect_false(result)
})

## exponential_equality_check
# -------------------------

test_that("private fucntion: exponential_equality_check: should work on small arrays when True", {
    # Given
    object_1 <- c(1, 2)
    object_2 <- c(1, 2)

    # When
    result <- exponential_equality_check(object_1, object_2)

    # Then
    expect_true(result)
})

test_that("private fucntion: exponential_equality_check: should work on small arrays when False", {
    # Given
    object_1 <- c(1, 2)
    object_2 <- c(1, 3)

    # When
    result <- exponential_equality_check(object_1, object_2)

    # Then
    expect_false(result)
})
## fast_is_bijection
# -----------------
test_that("private function: fast_is_bijection: true bijection", {
    # Given
    data_set <- data.table(int_code = 1 : 26,
    letters = LETTERS)

    # When
    result <- fast_is_bijection(data_set[["int_code"]], data_set[["letters"]])

    # Then
    expect_true(result)
})

test_that("private function: fast_is_bijection: not bijection", {
    # Given
    data_set <- data.table(alternative_1_2 = rep(c(1, 2), 13),
    letters = LETTERS)

    # When
    result <- fast_is_bijection(data_set[["alternative_1_2"]], data_set[["letters"]])

    # Then
    expect_false(result)
})

test_that("private function: fast_is_bijection: Tricky non-bijection on the threshold", {
    # Given
    df <- data.frame(col1 = c(rep(0, 9), 1), col2 = c(rep(1, 9), 1))

    # When

    # Then
    expect_true(fast_is_bijection(adult[["education"]], adult[["education_num"]]))
    expect_false(fast_is_bijection(adult[["education"]], adult[["income"]]))
    expect_false(fast_is_bijection(df[["col1"]], df[["col2"]]))
})

test_that("private function: fast_is_bijection: Work on dates that have multiple classes", {
    # Given
    df <- data.frame(col1 = c(as.POSIXct("2021-01-01 09:01:01")),
                     col2 = c(as.POSIXct("2021-01-01 09:01:01")))

    # When + Then
    expect_true(fast_is_bijection(df[["col1"]], df[["col2"]]))
})

## fast_max_nb_elt
# -------------
test_that("private function: fast_max_nb_elt", {
    # Given
    data_set <- sample(1 : 5, 100, replace = TRUE)
    max_number_unique_elt <- 1

    # When
    result <- fast_is_there_less_than(data_set, max_number_unique_elt)

    # Then
    expect_false(result)
    expect_false(fast_is_there_less_than(sample(1 : 5, 100, replace = TRUE), 4))
    expect_true(fast_is_there_less_than(sample(1 : 5, 100, replace = TRUE), 5))
})

test_that("private function: fast_max_nb_elt", {
    # Given
    data_set <- sample(1 : 5, 100, replace = TRUE)
    max_number_unique_elt <- 4

    # When
    result <- fast_is_there_less_than(data_set, max_number_unique_elt)

    # Then
    expect_false(result)
    expect_true(fast_is_there_less_than(sample(1 : 5, 100, replace = TRUE), 5))
})

test_that("private function: fast_max_nb_elt", {
    # Given
    data_set <- sample(1 : 5, 100, replace = TRUE)
    max_number_unique_elt <- 5

    # When
    result <- fast_is_there_less_than(data_set, max_number_unique_elt)

    # Then
    expect_true(result)
})
