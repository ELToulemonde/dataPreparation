context("test_generate_from_character.R")
requireNamespace("data.table")
verbose <- TRUE
## generate_from_character
# -------------------
test_that("generate_from_character: don't drop so generate 3 new cols", {
    # Given
    data_set <- data.table(character_col = LETTERS,
    other_col = LETTERS)
    store_ncol <- ncol(data_set)

    # When
    data_set_transformed <- generate_from_character(data_set, cols = "character_col")

    # Then
    expect_equal(ncol(data_set_transformed), store_ncol + 3)
})


test_that("generate_from_character: drop generate 3 col and suppress one", {
    # Given
    data_set <- data.table(character_col = LETTERS)
    store_ncol <- ncol(data_set)

    # When
    data_set_transformed <- generate_from_character(data_set, drop = TRUE)

    # Then
    expect_equal(ncol(data_set_transformed), store_ncol + 2)
})
