context("test_factorManipulations.R")
requireNamespace("data.table")
verbose <- TRUE
## un_factor
# ---------
test_that("un_factor: n_unfactor 2 unfactor only column with 2 unique values", {
    # Given
    data_set <- data.frame(true_factor = factor(rep(c(1, 2), 13)),
    false_factor = factor(LETTERS))

    # When
    data_set_unfactored <- un_factor(data_set, n_unfactor = 2, verbose = verbose)

    # Then
    expect_equal(lapply(data_set_unfactored, class), list(true_factor = "factor", false_factor = "character"))
})

test_that("un_factor: n_unfactor 0 unfactor nothing", {
    # Given
    data_set <- data.frame(true_factor = factor(rep(c(1, 2), 13)),
    false_factor = factor(LETTERS))

    # When
    data_set_unfactored <- un_factor(data_set, n_unfactor = 0, verbose = verbose)

    # Then
    expect_equal(lapply(data_set_unfactored, class), list(true_factor = "character", false_factor = "character"))
})

test_that("un_factor: n_unfactor to -1 unfactor all no question ask", {
    # Given
    data_set <- data.frame(true_factor = factor(rep(c(1, 2), 13)),
    false_factor = factor(LETTERS))

    # When
    data_set_unfactored <- un_factor(data_set, n_unfactor = - 1, verbose = verbose)

    # Then
    expect_equal(lapply(data_set_unfactored, class), list(true_factor = "factor", false_factor = "factor"))
})

test_that("un_factor: level to error", {
    # Given
    wrong_n_unfactor <- "a"

    # When and Then
    expect_error(un_factor(data_set, n_unfactor = wrong_n_unfactor, verbose = verbose),
    ": n_unfactor should be a numeric, you provided a ")
})

test_that("get_most_frequent_element: return correct element", {
  # Given
  example_list <- c(1, 1, 2, 3, 1, 4, 1)
  
  # When
  most_frequent_element <- get_most_frequent_element(example_list)
  
  # Then
  expect_equal(most_frequent_element, "1")
})


