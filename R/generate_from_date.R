###################################################################################################
#####################################  generate_factor_from_date ####################################
###################################################################################################
#' Generate factor from dates
#'
#'Taking Date or POSIXct colums, and building factor columns from them.
#' @param data_set Matrix, data.frame or data.table
#' @param cols List of date column(s) name(s) of data_set to transform into factor. To transform all
#' dates, set it to "auto". (characters, default to "auto")
#' @param type "year", "yearquarter", "yearmonth", "quarter" or "month", way to aggregate a date,
#' (character, default to "yearmonth")
#' @param drop Should \code{cols} be dropped after generation (logical, default to FALSE)
#' @param verbose Should the function log (logical, default to TRUE)
#' @param ... Other arguments such as \code{name_separator} to separate words in new columns names
#' (character, default to ".")
#' @return \code{data_set} with new columns. \code{data_set} is edited by \strong{reference}.
#' @examples
#' # Load set, and find dates
#' data(tiny_messy_adult)
#' tiny_messy_adult <- find_and_transform_dates(tiny_messy_adult, verbose = FALSE)
#'
#'# Generate new columns
#' # Generate year month columns
#' tiny_messy_adult <- generate_factor_from_date(tiny_messy_adult, cols = c("date1", "date2", "num1"))
#' head(tiny_messy_adult[, .(date1.yearmonth, date2.yearmonth)])
#'
#'
#' # Generate quarter columns
#' tiny_messy_adult <- generate_factor_from_date(tiny_messy_adult, 
#'                                               cols = c("date1", "date2"), type = "quarter")
#' head(tiny_messy_adult[, .(date1.quarter, date2.quarter)])
#' @export
#' @import data.table
generate_factor_from_date <- function(data_set, cols = "auto", type = "yearmonth", drop = FALSE, verbose = TRUE, ...) {
    # Working environment
    function_name <- "generate_factor_from_date"

    # Sanity check
    data_set <- check_and_return_datatable(data_set)
    cols <- real_cols(data_set = data_set, cols = cols, function_name = function_name, types = "date")
    is.verbose(verbose)

    # Initialization
    name_separator <- build_name_separator(list(...))
    if (verbose) {
        printl(function_name, ": I will create a factor column from each date column.")
        pb <- init_progress_bar(function_name, cols)
        start_time <- proc.time()
    }

    # Computation
    for (col in cols) {
        new_col <- paste0(col, name_separator, type)
        new_col <- make_new_col_name(new_col, names(data_set))
        set(data_set, NULL, new_col, build_date_factor(data_set[[col]], type = type))
        if (isTRUE(drop)) {
            set(data_set, NULL, col, NULL)
        }

        if (verbose) {
            add_a_tick_to_progress_bar(pb)
        }
    }
    if (verbose) {
        printl(function_name, ": It took me ", round((proc.time() - start_time)[[3]], 2),
        "s to transform ", length(cols), " column(s).")
    }

    # Wrap-up
    return(data_set)
}


###################################################################################################
####################################### build_date_factor ##############################################
###################################################################################################
# Code inspired by Ben Gorman in mltools package https://github.com/ben519/mltools
#' Date Factor
#'
#' Map a vector of dates to a factor at one of these levels {"yearmonth", "yearquarter", "quarter", "month"}
#' @details
#' The resulting vector is an ordered factor of the specified \code{type} (e.g. yearmonth)
#'
#' @param data_set A vector of date values
#' @param type One of {"year", "yearquarter", "yearmonth", "quarter", "month"}
#' @examples
#' library(data.table)
#' data_set <- as.Date(c("2014-01-01", "2015-01-01", "2015-06-01"))
#' build_date_factor(data_set, type = "yearmonth")
#' build_date_factor(data_set, type = "yearquarter")
#' build_date_factor(data_set, type = "yearquarter")
#'
#' @export
#' @import data.table
build_date_factor <- function(data_set, type = "yearmonth") {
    # Working environment
    function_name <- "build_date_factor"

    # Sanity check
    if (! type %in% c("year", "yearquarter", "yearmonth", "quarter", "month"))
    stop(paste0(function_name, ": type must be one of 'year', 'yearquarter', 'yearmonth', 'quarter' or 'month'."))
    if (! is.date(data_set)) {
        stop(paste0(function_name, ": data_set should contain dates."))
    }

    # Initialization
    # set formatting function
    if (type == "yearmonth") {
        format_func <- function(x)format(x, "%Y %b")
    } else if (type == "yearquarter") {
        format_func <- function(x) paste0(year(x), " Q", quarter(x))
    }else if (type == "year") {
        format_func <- year
    }else if (type == "quarter") {
        format_func <- function(x) paste0("Q", quarter(x))
    }else if (type == "month") {
        format_func <- function(x)format(x, "%b")
    }
    # Computation
    result <- factor(format_func(data_set))

    # Wrap-up
    return(result)
}


###################################################################################################
############################## generate_date_diffs #########################################################
###################################################################################################
#' Date difference
#'
#'Perform the differences between all dates of the data_set set and optionally with a static date.
#' @param data_set Matrix, data.frame or data.table
#' @param cols List of date column(s) name(s) of data_set to commute difference on. To transform all
#' dates, set it to "auto". (character, default to "auto")
#' @param analysis_date Static date (Date or POSIXct, optional)
#' @param units Unit of difference between too dates (string, default to 'years')
#' @param drop Should \code{cols} be dropped after generation (logical, default to FALSE)
#' @param verbose should the function log (logical, default to TRUE)
#' @param ... Other arguments such as \code{name_separator} to separate words in new columns names
#' (character, default to ".")
#' @details
#' \code{units} is the same as \code{\link{difftime}} units, but with one more possibility: years.
#' @return data_set (as a \code{\link{data.table}}) with more columns.
#' A numeric column has been added for every couple of Dates. The result is in years.
#' @examples
#' # First build a useful data_set set
#' require(data.table)
#' data_set <- data.table(ID = seq_len(100),
#'                   date1 = seq(from = as.Date("2010-01-01"),
#'                               to = as.Date("2015-01-01"),
#'                               length.out = 100),
#'                   date2 = seq(from = as.Date("1910-01-01"),
#'                               to = as.Date("2000-01-01"),
#'                               length.out = 100)
#'                   )
#'
#' # Now let's compute
#' data_set <- generate_date_diffs(data_set, cols = "auto", analysis_date = as.Date("2016-11-14"))
#' @import data.table
#' @importFrom lubridate is.Date
#' @export
generate_date_diffs <- function(data_set, cols = "auto", analysis_date = NULL, units = "years",
drop = FALSE, verbose = TRUE, ...) {
    # Working environment
    function_name <- "generate_date_diffs"

    # Sanity check
    data_set <- check_and_return_datatable(data_set)
    if (! is.null(analysis_date) & ! is.date(analysis_date)) {
        stop(paste0(function_name, ": analysis_date must be a Date"))
    }
    cols <- real_cols(data_set = data_set, cols = cols, function_name = function_name, types = "date")

    # Initialization
    name_separator <- build_name_separator(list(...))
    if (is.Date(analysis_date)) {
        analysis_date <- as.POSIXct(format(analysis_date, "%Y-%m-%d"))
    }
    n_transformed <- 0
    if (verbose) {
        printl(function_name, ": I will generate difference between dates.")
        start_time <- proc.time()
        pb <- init_progress_bar(function_name, cols)
    }
    # Computation
    # Unify format
    data_set <- date_format_unifier(data_set = data_set, format = "POSIXct")
    # Compute date difference
    for (col_i in cols) {
        cols_j <- cols[- (1 : which(col_i == cols))]
        for (col_j in cols_j) {
            new_col <- paste(col_i, "Minus", col_j, sep = name_separator)
            new_col <- make_new_col_name(new_col, names(data_set))
            set(data_set, NULL, new_col, extended_diff_time(data_set[[col_i]], data_set[[col_j]], units = units))
            n_transformed <- n_transformed + 1
        }
        if (! is.null(analysis_date)) {
            new_col <- paste(col_i, "Minus", "analysis_date", sep = name_separator)
            new_col <- make_new_col_name(new_col, names(data_set))
            set(data_set, NULL, new_col, extended_diff_time(data_set[[col_i]], analysis_date, units = units))
            n_transformed <- n_transformed + 1
        }
        if (isTRUE(drop)) {
            set(data_set, NULL, col_i, NULL)
        }
        if (verbose) {
            add_a_tick_to_progress_bar(pb)
        }
    }
    if (verbose) {
        printl(function_name, ": It took me ", round((proc.time() - start_time)[[3]], 2),
        "s to create ", n_transformed, " column(s).")
    }
    # Wrap-up
    return(data_set)
}


###################################################################################################
############################## Unify dates types #################################################
###################################################################################################

# @return a numeric
# extension of difftime to handle years
extended_diff_time <- function(col1, col2, units = "days") {
    if (units %in% c("auto", "secs", "mins", "hours", "days", "weeks")) {
        return(as.numeric(difftime(col1, col2, units = units)))
    }
    if (units == "years") {
      # To-do: check number of days in years instead?
      return(as.numeric(difftime(col1, col2, units = "days")) / 365.25)
    }
    else {
        stop("Sorry this unit hasn't been implemented yet")
    }
}