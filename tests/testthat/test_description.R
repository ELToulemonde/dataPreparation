context("test_description.R")
requireNamespace("data.table")
verbose <- TRUE

# Description
# -----------
test_that("description: errors: control level", {
    # Given
    data("messy_adult")
    wrong_level <- - 1

    # When and Then
    expect_error(description(messy_adult, level = wrong_level, verbose = verbose))
})

test_that("description: functionnal test: code should not fail", {
    # Given
    toydata <- data.table(int_col = 1 : 2,
    logical_col = c(TRUE, FALSE),
    date_col = c(Sys.Date(), Sys.Date()),
    character_col = c("A", "B"),
    factor_col = as.factor(c("A", "B")))
    # When and Then
    expect_null(description(toydata, path_to_write = "report.txt", verbose = verbose))


    # Clean up
    if (file.exists("report.txt")) file.remove("report.txt")
})