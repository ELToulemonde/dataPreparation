context("test_dates_manipulation.R")
requireNamespace("data.table")
verbose <- TRUE
Sys.setlocale("LC_TIME", "C")

## find_and_transform_dates
#-----------------------
test_that("find_and_transform_dates: functionnal test : find 4 dates in reference set messy_adult", {
    # Given
    data("messy_adult")

    # When
    messy_adult_transformed <- find_and_transform_dates(copy(messy_adult), verbose = verbose)

    # Then
    expect_equal(sum(sapply(messy_adult_transformed, is.POSIXct)), 4)
})

test_that("find_and_transform_dates: functionnal test : find 0 dates in reference set iris", {
    # Given
    data("iris")

    # When
    iris_transformed <- find_and_transform_dates(iris, verbose = verbose, n_test = 5)

    # Then
    expect_false(any(sapply(iris_transformed, is.POSIXct)))
})

test_that("find_and_transform_dates: check exceptions : ambiguities not in expected values", {
    # Given
    data("messy_adult")
    wrong_ambiguities <- 1

    # When and Then
    expect_error(find_and_transform_dates(messy_adult, verbose = verbose, ambiguities = wrong_ambiguities))
})

## identify_dates
#---------------
test_that("private function: identify_dates: do nothing if told so", {
    # Given
    data("messy_adult")
    expected_dates_cols <- list()

    # When
    dates_found <- identify_dates(messy_adult, cols = list(), n_test = 5, verbose = verbose)

    # Then
    expect_identical(dates_found, expected_dates_cols)
})

test_that("private function: identify_dates: ambiguities: with option warn, find nothing but print warning.", {
    # Given
    data_set <- data.table(date_col = c("2018-01-01", "2018-01-02", "2018-01-31"))

    # When and Then
    expect_output(dates_found <- identify_dates(data_set, ambiguities = "WARN", n_test = 2, verbose = TRUE),
    " seems to be a date but there is an ambiguity in the format. ")
    expect_equal(dates_found, list())
})

test_that("private function: identify_dates: ambiguities, SOLVE find ambiguity and solve it.", {
    # Given
    data_set <- data.table(date_col = c("2018-01-01", "2018-01-02", "2018-01-31"))

    # When
    dates_found <- identify_dates(data_set, n_test = 2, ambiguities = "SOLVE", verbose = verbose)

    # Then
    expect_equal(length(dates_found), 1)
    expect_equal(names(dates_found), "date_col")
    expect_equal(dates_found[["date_col"]], "%Y-%m-%d")
})


## identify_dates_formats
# ---------------------
test_that("private function: identify_dates_formats: standard format ", {
    # Given
    searched_format <- "%Y-%m-%d"
    data_set <- c("2015-01-01", "2016-01-01", "2015-09-01", "2015-03-01", "2015-01-31")

    # When
    format_found <- identify_dates_formats(data_set)

    # Then
    expect_equal(format_found, searched_format)
})

test_that("private function: identify_dates_formats: standard format ", {
    # Given
    searched_format <- "%Y_%m_%d"
    data_set <- as.factor(c("2015_01_01", "2016_01_01", "2015_09_01", "2015_03_01", "2015_01_31"))

    # When
    format_found <- identify_dates_formats(data_set)

    # Then
    expect_equal(format_found, searched_format)
})

test_that("private function: identify_dates_formats: standard format ", {
    # Given
    searched_format <- "%Y_%m_%d"
    data_set <- c("2015_1_1", "2016_1_1", "2015_9_1", "2015_3_1", "2015_1_31")

    # When
    format_found <- identify_dates_formats(data_set)

    # Then
    expect_equal(format_found, searched_format)
})

test_that("private function: identify_dates_formats: standard format ", {
    # Given
    searched_format <- "%d-%B-%Y"
    data_set <- c("01-january-2015", "01-january-2016", "01-september-2015", "01-march-2015", "31-january-2015")

    # When
    format_found <- identify_dates_formats(data_set)

    # Then
    expect_equal(format_found, searched_format)
})

test_that("private function: identify_dates_formats: standard format ", {
    # Given
    searched_format <- "%Y-%m-%d %H:%M"
    data_set <- c("2018-01-31 23:51", "2018-02-12 22:08", "2018-03-10 10:03", "2018-04-05 23:33", "2019-01-02 01:22")

    # When
    format_found <- identify_dates_formats(data_set)

    # Then
    expect_equal(format_found, searched_format)
})

test_that("private function: identify_datesFormaats: throw error on unexpected format", {
    # Given
    data_set <- c(TRUE, FALSE)

    # When and Then
    expect_error(identify_dates_formats(data_set),
    ": data_set should be some characters, numerics or factor of character.")
})

test_that("Private function: identify_dates_formats: find correct format in 2 formats ", {
    # Given
    searched_format <- "%Y-%m-%d"
    another_format <- "%m-%d-%Y"
    data_set <- format(Sys.Date(), searched_format)

    # When
    format_found <- identify_dates_formats(data_set, c(another_format, searched_format))

    # Then
    expect_equal(format_found, searched_format)
})

## identify_time_stamps_formats
# --------------------------
test_that("private function: identify_time_stamps_formats identified time stamps i,n second", {
    # Given
    time_stamps_in_s <- 1352068320

    # When
    identified_format <- identify_time_stamps_formats(time_stamps_in_s)

    # Then
    expect_equal(identified_format, "s")
})

test_that("private function: identify_time_stamps_formats identified time stamps in ms", {
    # Given
    time_stamps_in_ms <- 1352068320000

    # When
    identified_format <- identify_time_stamps_formats(time_stamps_in_ms)

    # Then
    expect_equal(identified_format, "ms")
})

test_that("private function: identify_time_stamps_formats: find nothing in random digit", {
    # Given
    digit_that_is_not_time_stamp <- 12345

    # When
    identified_format <- identify_time_stamps_formats(digit_that_is_not_time_stamp)

    # Then
    expect_null(identified_format)
})

test_that("private function: identify_time_stamps_formats: throw errors on non numerics", {
    # Given
    a_non_numeric <- "a"

    # When and Then
    expect_error(identify_time_stamps_formats(a_non_numeric),
    ": data_set should be some numerics.")
})


## control_date_conversion
# ------------------------
test_that("private function: control_date_conversion: conversion is ok even with
          lower / upper differences ", {
    # Given
    un_converted <- "01-September-2017"
    original <- "01-september-2017"

    # When
    control_result <- control_date_conversion(un_converted, original)
    # Then
    expect_true(control_result)
})

test_that("private function: control_date_conversion: conversion is ok even if all 0 are dropped ", {
    # Given
    un_converted <- "2017-01-02"
    original <- "2017-1-2"

    # When
    control_result <- control_date_conversion(un_converted, original)
    # Then
    expect_true(control_result)
})

test_that("private function: control_date_conversion: conversion is ok with list of converion ok ", {
    # Given
    un_converted <- c("2017-01-02", "01-September-2017")
    original <- c("2017-1-2", "1-september-2017")

    # When
    control_result <- control_date_conversion(un_converted, original)
    # Then
    expect_true(control_result)
})

## date_format_unifier
#-------------------
data_set <- data.table(column1 = as.Date("2016-01-01"), column2 = as.POSIXct("2017-01-01"))


test_that("date_format_unifier:", {
    expect_true(all(sapply(date_format_unifier(data_set, format = "Date"), is.Date)))
    expect_error(date_format_unifier(data_set, format = "adaedeaz"),
                 "date_format_unifier: only format: Date, POSXIct, POSIXlt are implemented. You gave:")
})

## is.date
#---------
test_that("is.date: a date is a date", {
    # Given
    date_as_date <- as.Date("2016-01-01")

    # When
    result <- is.date(date_as_date)

    # Then
    expect_true(result)
})

test_that("is.date: a POSIXct is a date", {
    # Given
    date_as_posixct <- as.POSIXct("2016-01-01")

    # When
    result <- is.date(date_as_posixct)

    # Then
    expect_true(result)
})


## get_all_possible_dates_formats
#-------------------------
test_that("get_all_possible_dates_formats:", {
    expect_is(get_all_possible_dates_formats(), "character")
})


## parse_date_time_formats
#--------------------------
test_that("Private function: parse_date_time_formats", {
    expect_true(is.vector(parse_date_time_formats()))
})