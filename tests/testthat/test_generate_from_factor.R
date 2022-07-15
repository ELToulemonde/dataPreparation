context("test_generate_from_factor.R")
requireNamespace("data.table")
verbose <- TRUE
## generate_from_factor
# -------------------
test_that("generate_from_factor: drop: functionnal test on reference set", {
    # Given
    data("messy_adult")
    messy_adult <- messy_adult[1 : 500, ] # Reduce it to save time
    store_ncol <- ncol(messy_adult)
    n_factor <- sum(sapply(messy_adult, is.factor))

    # When
    messy_adult <- generate_from_factor(messy_adult, drop = TRUE, verbose = verbose)

    # Then
    expect_equal(ncol(messy_adult), store_ncol + 2 * n_factor)
})

test_that("generate_from_factor: test don't drop => keep original col", {
    # Given
    data_set <- data.table(factor_col = as.factor(LETTERS))
    store_ncol <- ncol(data_set)
    n_factor <- sum(sapply(data_set, is.factor))

    # When
    data_set_transformed <- generate_from_factor(data_set, drop = FALSE, verbose = verbose)

    # Then
    expect_equal(ncol(data_set_transformed), store_ncol + 3 * n_factor)
    expect_true(all(names(data_set) %in% names(data_set_transformed)))
})

## one_hot_encoder
# ----------------
test_that("one_hot_encoder: ", {
    # Given
    data_set <- data.table(character_col = LETTERS)

    # When
    data_set_one_hot_encoded <- one_hot_encoder(data_set, verbose = verbose, drop = TRUE)

    # Then
    expect_equal(ncol(data_set_one_hot_encoded), uniqueN(LETTERS))
})


test_that("one_hot_encoder: expect error: ", {
    # Given
    data_set <- data.table(character_col = LETTERS)
    wrong_type <- "character"

    # When and Then
    expect_error(one_hot_encoder(data_set, type = wrong_type), ": type should either be ")
})


## build_encoding
# ----------------
test_that("build_encoding: ", {
    # Given
    data_set <- data.table(factor_col = as.factor(LETTERS))
    n_factor <- sum(sapply(data_set, is.factor))

    # When
    encoding <- build_encoding(data_set)

    # Then
    expect_equal(length(encoding), n_factor)
    expect_equal(length(encoding[["factor_col"]]$new_cols), uniqueN(LETTERS))
    expect_equal(encoding[["factor_col"]]$values, unique(LETTERS))
})


test_that("build_encoding: min_frequency allows to drop rare values", {
    # Given
    data_set <- data.table(factor_col = c(rep("A", 100), "B"))

    # When
    encoding <- build_encoding(data_set, verbose = verbose, min_frequency = 0.1)

    # Then
    expect_equal(encoding[["factor_col"]]$values, "A")
})

# Target encoding
# ---------------
test_that("target_encode: should set correct value for each student", {
    # Given
    data_set <- data.table(student = c("Marie", "Marie", "Pierre", "Louis", "Louis"),
    grades = c(1, 1, 2, 3, 4))
    target_encoding <- build_target_encoding(data_set, cols_to_encode = "student",
    target_col = "grades", functions = c("mean"))

    expected_result <- data.table(student = c("Marie", "Marie", "Pierre", "Louis", "Louis"),
    grades = c(1, 1, 2, 3, 4),
    grades_mean_by_student = c(1, 1, 2, 3.5, 3.5))
    # When
    result <- target_encode(data_set, target_encoding = target_encoding)

    # Then
    expect_equal(result, expected_result)
})

test_that("target_encode: if drop is asked col_to_encode should not be in columns anymore", {
    # Given
    col_to_encode <- "student"
    data_set <- data.table(student = c("Marie", "Marie", "Pierre", "Louis", "Louis"),
    grades = c(1, 1, 2, 3, 4))
    target_encoding <- build_target_encoding(data_set, cols_to_encode = col_to_encode,
    target_col = "grades", functions = c("mean"))

    # When
    result <- target_encode(data_set, target_encoding, drop = TRUE)

    # Then
    expect_false(col_to_encode %in% names(result))
})

# Build target encoding
# ---------------------
test_that("build_target_encoding: build_target_encoding should return a data.table with for
          each unique value mean of target", {
    # Given
    data_set <- data.table(col1 = c("a", "a", "b", "c", "c"),
    target = c(1, 1, 2, 3, 4))
    expected_target_encoded <- data.table(col1 = c("a", "b", "c"),
    target_mean_by_col1 = c(1, 2, 3.5))
    # When
    target_encoded <- build_target_encoding(data_set, cols_to_encode = "col1",
    target_col = "target", functions = "mean")

    # Then
    expect_equal(target_encoded[[1]], expected_target_encoded)
})


test_that("build_target_encoding: build_target_encoding should return length and mean when asked for", {
    # Given
    data_set <- data.table(col1 = c("a", "a", "b", "c", "c"),
    target = c(1, 1, 2, 3, 4))
    expected_target_encoded <- data.table(col1 = c("a", "b", "c"),
    target_mean_by_col1 = c(1, 2, 3.5),
    target_length_by_col1 = c(2, 1, 2))
    # When
    target_encoded <- build_target_encoding(data_set, cols_to_encode = "col1",
    target_col = "target", functions = c("mean", "length"))

    # Then
    expect_equal(target_encoded[[1]], expected_target_encoded)
})


test_that("build_target_encoding: build_target_encoding should return a list of data.table
          one for each col to encode", {
    # Given
    data_set <- data.table(col1 = c("a", "a", "b", "c", "c"),
                           col2 = c("z", "z", "z", "y", "y"),
                           target = c(1, 1, 2, 3, 4))
    cols_to_encode <- c("col1", "col2")

    # When
    target_encoded <- build_target_encoding(data_set, cols_to_encode = cols_to_encode,
    target_col = "target", functions = c("mean", "length"))

    # Then
    expect_true(is.list(target_encoded))
    expect_equal(length(target_encoded), length(cols_to_encode))
    expect_true(all(sapply(target_encoded, is.data.table)))
})

test_that("compute_probability_ratio: probability_ratio is correct", {
    # Given
    example_list <- c(1, 1, 1, 2, 2, 3)
            
    # When
    probability_ratio <- compute_probability_ratio(example_list)
    
    # Then
    expect_equal(probability_ratio, 6)
    })

test_that("compute_weight_of_evidence: weigth_of_evidence is correct", {
  # Given
  example_list <- c(1, 1, 1, 2, 2, 3)
  
  # When
  weigth_of_evidence <- compute_weight_of_evidence(example_list)
  
  # Then
  expect_equal(weigth_of_evidence, log(6))
})
