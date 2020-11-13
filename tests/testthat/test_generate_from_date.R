context("test_generate_from_date.R")
requireNamespace("data.table")
Sys.setlocale("LC_TIME", "C")
verbose <- TRUE

## generate_factor_from_date
# -----------------------
test_that("generate_factor_from_date: functionnal drop = TRUE indeed drop col and new col has type in name ", {
    # Given
    data_set <- data.table(col = as.POSIXct(c("2018-01-31", "2019-02-03")))
    store_ncol <- ncol(data_set)
    type <- "yearquarter"

    # When
    data_set_transformed <- generate_factor_from_date(data_set, drop = TRUE, type = type, verbose = verbose)

    # Then
    expect_equal(ncol(data_set_transformed), store_ncol)
    expect_false("col" %in% names(data_set_transformed))
    expect_true(paste0("col", ".", type) %in% names(data_set_transformed))
})

## build_date_factor
# -----------
test_that("private function: build_date_factor: test result for type yearmonth", {
    # Given
    data_set <- as.Date(c("2014-01-01", "2015-01-01", "2015-06-01"))
    type <- "yearmonth"
    expected_result <- factor(c("2014 Jan", "2015 Jan", "2015 Jun"))

    # When
    result <- build_date_factor(data_set, type = type)

    # Then
    expect_identical(result, expected_result)
})

test_that("private function: build_date_factor: test result for type yearquarter", {
    # Given
    data_set <- as.Date(c("2014-01-01", "2015-01-01", "2015-06-01"))
    type <- "yearquarter"
    expected_result <- factor(c("2014 Q1", "2015 Q1", "2015 Q2"))

    # When
    result <- build_date_factor(data_set, type = type)

    # Then
    expect_identical(result, expected_result)
})

test_that("private function: build_date_factor: test result for type quarter", {
    # Given
    data_set <- as.Date(c("2014-01-01", "2015-01-01", "2015-06-01"))
    type <- "quarter"
    expected_result <- factor(c("Q1", "Q1", "Q2"))

    # When
    result <- build_date_factor(data_set, type = type)

    # Then
    expect_identical(result, expected_result)
})

test_that("private function: build_date_factor: test result for type month", {
    # Given
    data_set <- as.Date(c("2014-01-01", "2015-01-01", "2015-06-01"))
    type <- "month"
    expected_result <- factor(c("Jan", "Jan", "Jun"))

    # When
    result <- build_date_factor(data_set, type = type)

    # Then
    expect_identical(result, expected_result)
})

test_that("private function: build_date_factor: test result for type year", {
    # Given
    data_set <- as.Date(c("2014-01-01", "2015-01-01", "2015-06-01"))
    type <- "year"
    expected_result <- factor(c("2014", "2015", "2015"))

    # When
    result <- build_date_factor(data_set, type = type)

    # Then
    expect_identical(result, expected_result)
})

test_that("private function: build_date_factor: error if data_set doesn't contain dates", {
    # Given
    data_set <- 1 : 5

    # When and Then
    expect_error(build_date_factor(data_set), ": data_set should contain dates.")
})

test_that("Private function: build_date_factor: error on wrong type", {
    # Given
    data_set <- as.Date(c("2014-01-01", "2015-01-01", "2015-06-01"))
    wrong_type <- "acdaezcze"

    # When and Then
    expect_error(build_date_factor(data_set, type = wrong_type),
    ": type must be one of 'year', 'yearquarter', 'yearmonth', 'quarter' or 'month'.")
})

## generate_date_diffs
# ------------------
test_that("generate_date_diffs: generate 1 new col for the difference between date 1 and date 2", {
    # Given
    data_set <- data.table(ID = 1 : 100,
    date1 = seq(from = as.Date("2010-01-01"),
    to = as.Date("2015-01-01"),
    length.out = 100),
    date2 = seq(from = as.Date("1910-01-01"),
    to = as.Date("2000-01-01"),
    length.out = 100)
    )
    analysis_date <- as.Date("2016-11-14")

    # When
    data_set_transformed <- generate_date_diffs(copy(data_set), analysis_date = analysis_date,
                                                drop = TRUE, verbose = verbose)

    # Then
    expect_equal(ncol(data_set_transformed), ncol(data_set) + 1)
    expect_false(any(is.na(data_set_transformed)))
})


test_that("generate_date_diffs: errors", {
    # Given
    data_set <- data.table(date1 = seq(from = as.Date("2010-01-01"),
    to = as.Date("2015-01-01"),
    length.out = 100))
    wrong_analysis_date <- "2017-01-01"

    # When and Then
    expect_error(generate_date_diffs(data_set, analysis_date = wrong_analysis_date, verbose = verbose),
    "analysis_date must be a Date")
})


# extended_diff_time
# --------
test_that("extended_diff_time: diff in days", {
    # Given
    date_1 <- as.Date("2017-01-03")
    date_2 <- as.Date("2017-01-02")
    units <- "days"

    # When
    diff_result <- extended_diff_time(date_1, date_2, units = units)

    # Then
    expect_equal(diff_result, 1)
})

test_that("extended_diff_time: diff in hours", {
    # Given
    date_1 <- as.Date("2017-01-03")
    date_2 <- as.Date("2017-01-02")
    units <- "hours"

    # When
    diff_result <- extended_diff_time(date_1, date_2, units = units)

    # Then
    expect_equal(diff_result, 24)
})

test_that("extended_diff_time: diff in mins", {
    # Given
    date_1 <- as.Date("2017-01-03")
    date_2 <- as.Date("2017-01-02")
    units <- "mins"

    # When
    diff_result <- extended_diff_time(date_1, date_2, units = units)

    # Then
    expect_equal(diff_result, 1440)
})

test_that("extended_diff_time: diff in years", {
    # Given
    date_1 <- as.Date("2017-01-03")
    date_2 <- as.Date("2017-01-02")
    units <- "years"

    # When
    diff_result <- extended_diff_time(date_1, date_2, units = units)

    # Then
    expect_equal(diff_result, 1 / 365.25)
})

test_that("extended_diff_time: errors on wrong unit", {
    # Given
    date_1 <- as.Date("2017-01-03")
    date_2 <- as.Date("2017-01-02")
    wrong_units <- "vyuiio"

    # When and Then
    expect_error(extended_diff_time(date_1, date_2, units = wrong_units), "Sorry this unit hasn't been implemented yet")
})