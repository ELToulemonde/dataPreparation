context("test_fast_dates")

# as.POSIXct_fast
# ----------------
test_that("Function as.POSIXct_fast: give same result as as.POSIXct", {
    # Given
    date_character <- "2018-06-12"
    format <- "%Y-%m-%d"
    expected_result <- as.POSIXct(date_character, format = format)

    # When
    result <- as.POSIXct_fast(date_character, format = format)

    # Then
    expect_equal(result, expected_result)
})