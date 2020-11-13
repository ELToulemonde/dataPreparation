context("test_generic_functions.R")
requireNamespace("data.table")
verbose <- TRUE
Sys.setlocale("LC_TIME", "C")
## findNFirstNonNull
#-------------------
test_that("find_n_first_non_null: given 1 to 50, and asking for 5 value should return 1 to 5", {
    # Given
    numerical_values <- 1 : 50
    n_wanted_values <- 5
    expected_results <- 1 : 5

    # When
    result <- find_n_first_non_null(numerical_values, n_wanted_values)

    # Then
    expect_identical(result, expected_results)
})

test_that("find_n_first_non_null: Given 1 to 50, asking for 10 element should return 1 to 10", {
    # Given
    numerical_values <- 1 : 50
    n_wanted_values <- 10
    expected_results <- 1 : 10

    # When
    result <- find_n_first_non_null(numerical_values, n_wanted_values)

    # Then
    expect_identical(result, expected_results)
})

test_that("find_n_first_non_null: Given a NA and then 1 to 50, asking for 10 element should return 1 to 10", {
    # Given
    numerical_values <- c(NA, 1 : 50)
    n_wanted_values <- 10
    expected_results <- 1 : 10

    # When
    result <- find_n_first_non_null(numerical_values, n_wanted_values)

    # Then
    expect_identical(result, expected_results)
})

test_that("find_n_first_non_null: Given a empty string and then the alphabet,
          when asking for 3 letters, should return first 3 letters", {
    # Given
    letters_values <- c("", LETTERS)
    n_wanted_values <- 3
    expected_results <- c("A", "B", "C")

    # When
    result <- find_n_first_non_null(letters_values, n_wanted_values)

    # Then
    expect_identical(result, expected_results)
})


test_that("find_n_first_non_null: Given two not NA values, and asking for more, should return result of len two", {
    # Given
    two_values_and_to_nas <- c("A", "B", NA, NA)
    n_wanted_values <- 3
    expected_results <- 2

    # When
    result <- find_n_first_non_null(two_values_and_to_nas, n_wanted_values)

    # Then
    expect_equal(length(result), expected_results)
})

## checkAndReturnDataTable
#-------------------------
test_that("check_and_return_datatable: Given a data.table, Should return a data.table", {
    # Given
    data("adult")
    setDT(adult)

    # When
    result <- check_and_return_datatable(adult)

    # Then
    expect_true(is.data.table(result))
})

test_that("check_and_return_datatable: Given a data.frame, Should return a data.table", {
    # Given
    data("adult")
    adult <- as.data.frame(adult)

    # When
    result <- check_and_return_datatable(adult)

    # Then
    expect_true(is.data.table(result))
})

test_that("check_and_return_datatable: Given a matrix, Should return a data.table", {
    # Given
    data("adult")
    adult <- as.matrix(adult)

    # When
    result <- check_and_return_datatable(adult)

    # Then
    expect_true(is.data.table(result))
})

test_that("check_and_return_datatable: Given a data.table with not unique names,
          Should return a data.table with unique names", {
    # Given
    data("adult")
    names(adult) <- rep("some_str", ncol(adult))

    # When
    expect_warning(result <- check_and_return_datatable(adult))

    # Then
    expect_equal(length(names(result)), length(unique(names(result))))
})


test_that("check_and_return_datatable: control errors: data set type", {
    # Given
    wrong_data_set <- "a"

    # When and Then
    expect_error(check_and_return_datatable(wrong_data_set),
    "should be a data.table, a data.frame or a matrix.")
})

test_that("check_and_return_datatable: control errors: a data set should not be empty", {
    # Given
    empty_data_set <- data.table()

    # When and Then
    expect_error(check_and_return_datatable(empty_data_set),
    "should have at least have 1 line")
})

test_that("check_and_return_datatable: control errors: a string is not a valide data set", {
    # Given
    one_col_data_set <- data.frame(row.names = c(1, 2))

    # When and Then
    expect_error(check_and_return_datatable(one_col_data_set),
    "should have at least have 1 column")
})

## is.verbose
# -----------
test_that("is.verbose: control input", {
    # Given
    wrong_verbose <- "a"

    # When and Then
    expect_error(is.verbose(wrong_verbose),
    "verbose should be logical")
})

test_that("is.verbose: control input error", {
    # Given
    correct_verbose <- TRUE

    # When and Then
    expect_null(is.verbose(correct_verbose))
})

## is.verbose_levels
# ------------------
test_that("is.verbose_levels: control input wrong level", {
    # Given
    wrong_verbose_level <- "a"

    # When and Then
    expect_error(is.verbose_level(wrong_verbose_level),
    "or an integer lower than")
})

test_that("is.verbose_levels: too high level", {
    # Given
    too_high_level <- 3
    max_level <- 2

    # When and Then
    expect_error(is.verbose_level(too_high_level, max_level = max_level),
    " or an integer lower than ")
})

test_that("is.verbose_levels: correct level", {
    # Given
    correct_lvel <- 1
    max_level <- 2

    # When and Then
    expect_null(is.verbose_level(correct_lvel, max_level = max_level))
})

## is.share
# ---------
test_that("is.share: wrong type", {
    # Given
    wrong_share <- "a"

    # When and Then
    expect_error(is.share(wrong_share),
    "should be a numeric between 0 and 1")
})

test_that("is.share: out of range share", {
    # Given
    out_of_range_share <- 2

    # When and Then
    expect_error(is.share(out_of_range_share),
    "should be a numeric between 0 and 1")
})

test_that("is.share: correct value", {
    # Given
    correct_share <- 1

    # When and Then
    expect_null(is.share(correct_share))
})

## is.col
#--------
test_that("is.col: col is in data set", {
    # Given
    data_set <- data.table(a = "1")

    # When and Then
    expect_null(is.col(data_set, cols = "a"))
})

test_that("is.col: 2 cols are in data set", {
    # Given
    data_set <- data.table(a = "1", b = 2)

    # When and Then
    expect_null(is.col(data_set, cols = c("a", "b")))
})

test_that("is.col: one of 2 cols is not in dataset", {
    # Given
    data_set <- data.table(a = "1")

    # When and Then
    expect_error(is.col(data_set, cols = c("a", "b")), ". should be column of data_set")
})

test_that("is.col: col is not in dataset", {
    # Given
    data_set <- data.table(a = "1")

    # When and Then
    expect_error(is.col(data_set, cols = "b"), ". should be column of data_set")
})

test_that("is.col: error data set is not correct", {
    # Given
    wrong_data_set <- 1

    # When and Then
    expect_error(is.col(wrong_data_set, cols = "b"), "is.col: data_set should be a data.table, data.frame or matrix")
})

## real_cols
# ----------
test_that("real_cols: select cols that are indeed in data_set", {
    # Given
    data_set <- data.table(col_1 = 1)

    # When
    col_selected <- real_cols(data_set, cols = c("col_1", "col_2"))

    # Then
    expect_equal(col_selected, c("col_1"))
})
test_that("real_cols: auto mode give all column names", {
    # Given
    data_set <- data.table(col_1 = 1, col_2 = 2)

    # When
    col_selected <- real_cols(data_set, cols = "auto")

    # Then
    expect_equal(col_selected, names(data_set))
})

test_that("real_cols: auto mode and specified type give all coresponding cols", {
    # Given
    data_set <- data.table(col_1 = 1, col_2 = "2")

    # When
    col_selected <- real_cols(data_set, cols = "auto", types = "numeric")

    # Then
    expect_equal(col_selected, c("col_1"))
})

test_that("real_cols: with char 0 col list throw null answer", {
    # Given
    data_set <- data.table(col_1 = 1)

    # When
    selected_cols <- real_cols(adult, cols = character(0))

    # Then
    expect_null(selected_cols)
})

test_that("real_cols: with NULL col list throw null answer", {
    # Given
    data_set <- data.table(col_1 = 1)

    # When
    selected_cols <- real_cols(adult, cols = NULL)

    # Then
    expect_null(selected_cols)
})

test_that("real_cols: is able to filter on numeric", {
    # Given
    date_set <- data.frame(date_col = as.Date(c("2015-01-01", "2016-01-01", "2015-09-01", "2015-03-01", "2015-01-31")),
    id = 1 : 5)

    # When
    cols_selected <- real_cols(date_set, cols = c("date_col", "id"), types = c("numeric"))

    # Then
    expect_identical(cols_selected, c("id"))
})

test_that("real_cols: is able to filter on dates", {
    # Given
    date_set <- data.frame(date_col = as.Date(c("2015-01-01", "2016-01-01", "2015-09-01", "2015-03-01", "2015-01-31")),
    id = 1 : 5)

    # When
    cols_selected <- real_cols(date_set, cols = c("date_col", "id"), types = c("date"))

    # Then
    expect_identical(cols_selected, c("date_col"))
})

## getPossibleSeparators
#------------------------
test_that("getPossibleSeparators:", {
    # Given

    # When
    possible_separator <- get_possible_separators()

    # Then
    expect_true(is.vector(possible_separator))
})


## printl
#--------
test_that("printl:", {
    # Given + When and Then
    expect_output(printl("printl", " is a private function ", " easier to use than print"),
    "printl is a private function  easier to use than print"
    )
})

## controlNumberOfRows
#--------------------
test_that("control_nb_rows: asking for less than number of rows is ok", {
    # Given
    data_set <- data.table(col1 = c(1, 2, 3))
    acceptable_n_rows <- 1

    # When
    n_row_controled <- control_nb_rows(data_set, acceptable_n_rows)

    # Then
    expect_equal(n_row_controled, acceptable_n_rows)
})

test_that("control_nb_rows: asking for more than number of rows is not ok, warn + reduce to number of rows", {
    # Given
    data_set <- data.table(col1 = c(1, 2, 3))
    not_acceptable_n_rows <- 10

    # When and Then
    expect_warning(n_row_controled <- control_nb_rows(data_set, not_acceptable_n_rows),
    "You want to check more rows than there are in data_set, I set nb_rows to")
    expect_equal(n_row_controled, nrow(data_set))
})

test_that("control_nb_rows: asking for 0 rows is not ok, warn + reduce to min(number of rows, 30)", {
    # Given
    data_set <- data.table(col1 = c(1, 2, 3))
    not_acceptable_n_rows <- 0

    # When and Then
    expect_warning(n_row_controled <- control_nb_rows(data_set, not_acceptable_n_rows),
    "You want to check at least a few rows than there are in data_set, I set nb_rows to")
    expect_equal(n_row_controled, min(nrow(data_set), 30))
})

test_that("conrtol_nb_rows: number of row should be numeric", {
    # Given
    data_set <- data.table(col1 = c(1, 2, 3))
    wrong_number_of_row <- "a"

    # When and Then
    expect_error(control_nb_rows(data_set, nb_rows = wrong_number_of_row), " should be a numeric.")
})

## is.agg_function
# -----------------
test_that("is.agg_function: thow error when functions doesn't contains only character", {
    # Given
    functions <- list(sum, "sum")

    # When and Then
    expect_error(is.agg_function(functions), "functions should be a list of names")
})

test_that("is.agg_function: thow warning when function is not found", {
    # Given
    functions <- list("sum", "a")

    # When and Then
    expect_warning(is.agg_function(functions), " doesn't exist, it won't be used.")
})

test_that("is.agg_function: thow warning when a function is not a function. And remove it from the list", {
    # Given
    b <- 1
    attach(list(b = b)) # A bit ugly, but i don't know how to do it another way.
    functions <- list("sum", "b")

    # When and Then
    expect_warning(result <- is.agg_function(functions),
    " is not a function, it won't be used.")
    expect_equal(list("sum"), result)

    # Clean up
    detach(list(b = b))
})

test_that("is.agg_function: thow warning when a function is not an aggregation function. And remove it from the list", {
    # Given
    functions <- list("sum", "sqrt")

    # When and Then
    expect_warning(result <- is.agg_function(functions),
    " sqrt is not an aggregation function")
    expect_equal(list("sum"), result)
})

## function.maker
# ---------------
test_that("function.maker: ", {
    # Given
    a_function <- function(x) {
      sum(x, na.rm = TRUE)
    }

    # When
    built_function <- function.maker(a_function, type = "numeric")

    # Then
    expect_true(is.function(built_function))
})

test_that("function.maker: ", {
    # Given
    a_numeric <- 1

    # When
    built_function <- function.maker(a_numeric, type = "numeric")

    # Then
    expect_true(is.function(built_function))
})

test_that("function.maker: ", {
    # Given
    a_character <- "a"

    # When
    built_function <- function.maker(a_character, type = "character")

    # Then
    expect_true(is.function(built_function))
})

test_that("function.maker: ", {
    # Given
    a_logical <- TRUE

    # When
    built_function <- function.maker(a_logical, type = "logical")

    # Then
    expect_true(is.function(built_function))
})

test_that("function.maker: warning not handling na", {
    expect_warning(function.maker(max, type = "numeric"))
    expect_warning(function.maker(max, type = "character"))
    given_function <- function(...) {
      sum(...) / length(list(...)) > 0.5
    }
    expect_warning(function.maker(given_function, type = "logical"))
})

test_that("function.maker: stop not aggregation function", {
    expect_error(function.maker(sqrt, type = "numeric"))
    expect_error(function.maker(function(x)paste0(x, "aaa"), type = "character"))
    given_function <- function(x) {
      ! x
    }
    expect_error(function.maker(given_function, type = "logical"))
})

test_that("function.maker: error wrong type", {
    expect_error(function.maker("a", type = "numeric"))
    expect_error(function.maker(1, type = "character"))
    expect_error(function.maker("a", type = "logical"))
    data_set <- data.table()
    expect_error(function.maker(data_set, type = "logical"),
                 ": is in a shape that isn't handled, please provide constant or aggregation function.")
})


## make_new_col_name
# -------------------
test_that("make_new_col_name: when col already exist add a 1 at the end of the name", {
    # Given
    a_col_list <- c("a", "b")
    a_col <- "a"
    expected_col_name <- "a1"

    # When
    col_name <- make_new_col_name(a_col, a_col_list)

    # Then
    expect_equal(col_name, expected_col_name)
})

test_that("make_new_col_name: when col and col1 already exist, add a 2 at the end of the name", {
    # Given
    a_col_list <- c("a", "a1")
    a_col <- "a"
    expected_col_name <- "a2"

    # When
    col_name <- make_new_col_name(a_col, a_col_list)

    # Then
    expect_equal(col_name, expected_col_name)
})

test_that("make_new_col_name: when col doesn't exist yet, keep name", {
    # Given
    a_col_list <- c("a", "b")
    a_col <- "c"
    expected_col_name <- a_col

    # When
    col_name <- make_new_col_name(a_col, a_col_list)

    # Then
    expect_equal(col_name, expected_col_name)
})

test_that("make_new_col_name: throw error col should be a character", {
    # Given
    a_col_list <- c("a", "b")
    a_col <- 1

    # When and Then
    expect_error(make_new_col_name(a_col, a_col_list),
    "new_col and col_names should be character.")
})


## build_name_separator
# --------------------
test_that("build_name_separator: without name_separator in args return  default value", {
    # Given
    args <- list()

    # When
    name_separator <- build_name_separator(args)

    # Then
    expect_equal(name_separator, ".")
})

test_that("build_name_separator: with name_separator in args return name separator", {
    # Given
    args <- list(name_separator = ",")

    # When
    name_separator <- build_name_separator(args)

    # Then
    expect_equal(name_separator, ",")
})

test_that("build_name_separator: with name_separator in args but not a string throw error", {
    # Given
    args <- list(name_separator = 1)

    # When
    expect_error(build_name_separator(args),
    "name_separator should be a character.")
})

test_that("build_name_separator: with name_separator in args with multiple values throw error", {
    # Given
    args <- list(name_separator = c(".", ";"))

    # When
    expect_error(build_name_separator(args),
    "name_separator should be a character.")
})


## build_factor_date_type
# -----------------------
test_that("build_factor_date_type: without factor_date_type as arguments throw default value", {
    # Given
    args <- list()

    # When
    factor_date_type <- build_factor_date_type(args)

    # Then
    expect_equal(factor_date_type, "yearmonth")
})

test_that("build_factor_date_type: with factor_date_type as arguments return value", {
    # Given
    args <- list(factor_date_type = "a")

    # When
    factor_date_type <- build_factor_date_type(args)

    # Then
    expect_equal(factor_date_type, "a")
})

test_that("build_factor_date_type: throw error when factor_date_type is not character", {
    # Given
    args <- list(factor_date_type = 1)

    # When and Then
    expect_error(build_factor_date_type(args),
    "factor_date_type should be a character.")
})


test_that("build_factor_date_type: throw error when factor_date_type is of length greater than 1", {
    # Given
    args <- list(factor_date_type = 1)

    # When and Then
    expect_error(build_factor_date_type(args),
    "factor_date_type should be a character.")
})
