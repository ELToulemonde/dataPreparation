context("test_set_col_as.R")
requireNamespace("data.table")
verbose <- TRUE

## set_col_as_numeric
#-----------------------
test_that("set_col_as_numeric:", {
    # Given
    data_set <- data.table(char_col_1 = c("1", "2", "3"),
    char_col_2 = c("4", "5", "6"))

    # When
    data_set_transformerd <- set_col_as_numeric(data_set, cols = c("char_col_1", "char_col_2"), verbose = verbose)

    # Then
    expect_true(all(sapply(data_set_transformerd, is.numeric)))
})

## set_col_as_character
#-------------------
test_that("set_col_as_character: Set numCol and factorCol as character", {
    # Given
    data_set <- data.table(numCol = c(1, 2, 3),
    factorCol = as.factor(c("a", "b", "c")),
    charcol = c("1", "2", "a"))

    # When
    data_set_transformed <- set_col_as_character(data_set, cols = "auto", verbose = verbose)

    # Then
    expect_true(all(sapply(data_set_transformed, is.character)))
})

## set_col_as_date
#-----------------------
test_that("set_col_as_date: transform using correct element if format list", {
    # Given
    data_set <- data.table(ID = 1 : 6,
    date1 = c("2015-01-01", "2016-01-01", "2015-09-01", "2015-03-01", "2015-01-31", ""),
    date2 = as.factor(c("2015_01_01", "2016_01_01", "2015_09_01", "2015_03_01", "2015_01_31", ""))
    )

    # When
    data_set_transformed <- set_col_as_date(data_set, cols = c("date1", "date2"),
                                            format = list(date1 = "%Y-%m-%d", "%Y_%m_%d"), verbose = verbose)

    # Then
    expect_true(is.integer(data_set_transformed$ID))
    expect_true(is.POSIXct(data_set_transformed$date1))
    expect_true(is.POSIXct(data_set_transformed$date2))
})


test_that("set_col_as_date: only transform columns it is told to transform even if they are factor", {
    # Given
    data_set <- data.table(ID = 1 : 6,
    date1 = c("2015-01-01", "2016-01-01", "2015-09-01", "2015-03-01", "2015-01-31", ""),
    date2 = as.factor(c("2015_01_01", "2016_01_01", "2015_09_01", "2015_03_01", "2015_01_31", ""))
    )

    # When
    data_set_transformed <- set_col_as_date(data_set, cols = "date2", format = "%Y_%m_%d", verbose = verbose)

    # Then
    expect_true(is.integer(data_set_transformed$ID))
    expect_true(is.character(data_set_transformed$date1))
    expect_true(is.POSIXct(data_set_transformed$date2))
})

test_that("set_col_as_date: transform one specific column without giving format", {
    # Given
    data_set <- data.table(ID = 1 : 6,
    date1 = c("2015-01-01", "2016-01-01", "2015-09-01", "2015-03-01", "2015-01-31", ""),
    date2 = as.factor(c("2015_01_01", "2016_01_01", "2015_09_01", "2015_03_01", "2015_01_31", ""))
    )

    # When
    data_set_transformed <- set_col_as_date(data_set, cols = "date1", verbose = verbose)

    # Then
    expect_true(is.integer(data_set_transformed$ID))
    expect_true(is.POSIXct(data_set_transformed$date1))
    expect_true(is.factor(data_set_transformed$date2))
})

test_that("set_col_as_date: raise warning, when column is not character", {
    # Given
    data_set <- data.table(ID = 1 : 6,
    date1 = c("2015-01-01", "2016-01-01", "2015-09-01", "2015-03-01", "2015-01-31", ""))

    # When and Then
    expect_warning(set_col_as_date(copy(data_set), cols = "ID", verbose = verbose),
    "set_col_as_date: I can't handle ID, please see documentation.")
})

test_that("set_col_as_date: raise warning, when column is character but tyope is not correct and doesn't change it", {
    # Given
    data_set <- data.table(ID = as.character(1 : 6),
    date1 = c("2015-01-01", "2016-01-01", "2015-09-01", "2015-03-01", "2015-01-31", ""))
    store_id <- data_set[["ID"]]

    # When and Then
    expect_warning(data_set_transformed <- set_col_as_date(copy(data_set), cols = "ID",
                                                           format = "%Y-%m-%d", verbose = verbose))
    expect_equal(store_id, data_set_transformed[["ID"]])
})

test_that("set_col_as_date: don't transform a column that isn't a date and with no format provided", {
    # Given
    data_set <- data.table(ID = as.character(1 : 6))
    store_id <- data_set[["ID"]]

    # When
    data_set_transformed <- set_col_as_date(copy(data_set), cols = "ID", verbose = verbose)

    # Then
    expect_true(is.character(data_set_transformed[["ID"]]))
    expect_equal(store_id, data_set_transformed[["ID"]])
})


test_that("set_col_as_date: work even if format not used by parse_date_time", {
    # Given
    data_set <- data.table(time = c("10:01:55", "09:35:60"))

    # When
    data_set_transformed <- set_col_as_date(data_set, cols = "time", format = "%H:%M:%S", verbose = verbose)
    # Then
    expect_true(is.POSIXct(data_set_transformed[["time"]]))
})

test_that("set_col_as_date: with s time stamps", {
    # Given
    data_set <- data.table(time_stamp_s = c(1483225200, 1485990000, 1488495600))

    # When
    data_set_transformed <- set_col_as_date(data_set, cols = "time_stamp_s", format = "s", verbose = verbose)
    # Then
    expect_true(is.POSIXct(data_set_transformed[["time_stamp_s"]]))
})

test_that("set_col_as_date: with ms time stamps", {
    # Given
    data_set <- data.table(time_stamp_ms = c(1483225200000, 1485990000000, 1488495600000))

    # When
    data_set_transformed <- set_col_as_date(data_set, cols = "time_stamp_ms", format = "ms", verbose = verbose)
    # Then
    expect_true(is.POSIXct(data_set_transformed[["time_stamp_ms"]]))
})


## is.format
# ------------
test_that("Private function: is.format: control errors: 1st level error not a list not a character", {
    # Given
    wrong_format <- 1

    # When and Then
    expect_error(is.format(wrong_format),
    ": format should either be list of formats or a character.")
})


test_that("Private function: is.format: control errors: 2nd level error a list but not of character", {
    # Given
    wrong_format <- list(1)

    # When and Then
    expect_error(is.format(wrong_format),
    ": format should either be list of character or a character.")
})


## parse_date_cols
# ----------------
test_that("Private function: parse_date_cols: with cols == NULL, return names of format list", {
    # Given
    cols <- NULL
    format <- list(a = "something", b = "someotherthing")

    # When
    cols_parsed <- parse_date_cols(cols = cols, format = format)

    # Then
    expect_identical(names(format), cols_parsed)
})

test_that("Private function: parse_date_cols: with cols and format not null return cols", {
    # Given
    cols <- c("a", "b")
    format <- list(a = "something", b = "someotherthing")

    # When
    cols_parsed <- parse_date_cols(cols = cols, format = format)

    # Then
    expect_identical(cols, cols_parsed)
})

test_that("Private function: parse_date_cols: throw error if cols is not NULL and format has not same length as cols
          and one of the columns ar in format", {
    # Given
    cols <- c("a", "b")
    format <- list(a = "something", c = "someotherthing", d = "yetanoherthing")

    # When and Then
    expect_error(parse_date_cols(cols = cols, format = format),
    "you provide cols and format but I'm not able to match them, please feed format as named list.")
})

## set_col_as_factor
#----------------
test_that("set_col_as_factor: Behave with default n_levels", {
    # Given
    data_set <- data.table(col = c("A", "B", "C"))

    # When
    result <- set_col_as_factor(data_set, cols = "col", verbose = verbose)

    # Then
    expect_true(is.factor(result[["col"]]))
})

test_that("set_col_as_factor: behave with n_levels = -1", {
    # Given
    data_set <- data.table(col = c("A", "B", "C"))

    # When
    result <- set_col_as_factor(data_set, cols = "col", n_levels = - 1, verbose = verbose)

    # Then
    expect_true(is.factor(result[["col"]]))
})

test_that("set_col_as_factor: column is unchanged if number of values greatter than n_levels", {
    # Given
    data_set <- data.table(col = c("A", "B", "C"))

    # When
    result <- set_col_as_factor(data_set, cols = "col", n_levels = 2, verbose = verbose)

    # Then
    expect_false(is.factor(result[["col"]]))
    expect_true(is.character(result[["col"]]))
})

test_that("set_col_as_factor:", {
    # Given
    data_set <- data.table(col = c("A", "B", "C"))

    # When and Then
    expect_error(set_col_as_factor(data_set, cols = "col", n_levels = "a", verbose = verbose),
    ": n_levels should be an integer.") #N_levels not integer
})