#######################################################################################
############################ Set col as numeric ######################################
#######################################################################################
#' Set columns as numeric
#'
#'Set as numeric a character column (or a list of columns) from a data.table.
#' @param data_set Matrix, data.frame or data.table
#' @param cols List of column(s) name(s) of data_set to transform into numerics
#' @param strip_string should I change "," to "." in the string? (logical, default to FALSE)
#' If set to TRUE, computation will be a bit longer
#' @param verbose Should the function log (logical, default to TRUE)
#' @return  data_set (as a \code{\link[data.table]{data.table}}), with specified columns set as numeric.
#' @examples
#' # Build a fake data.table
#' data_set <- data.frame(charCol1 = c("1", "2", "3"),
#' 						 charCol2 = c("4", "5", "6"))
#'
#' # Set charCol1 and charCol2 as numeric
#' data_set <- set_col_as_numeric(data_set, cols = c("charCol1", "charCol2"))
#'
#' # Using strip string when spaces or wrong decimal separator is used
#' data_set <- data.frame(charCol1 = c("1", "2", "3"),
#'                       charCol2 = c("4, 1", "5, 2", "6, 3"))
#'
#' # Set charCol1 and charCol2 as numeric
#' set_col_as_numeric(data_set, cols = c("charCol1", "charCol2"))
#' # generate mistakes
#' set_col_as_numeric(data_set, cols = c("charCol1", "charCol2"), strip_string = TRUE)
#' # Doesn't generate any mistake (but is a bit slower)
#' @import data.table
#' @export
set_col_as_numeric <- function(data_set, cols, strip_string = FALSE, verbose = TRUE) {
    # Working environment
    function_name <- "set_col_as_numeric"

    # Sanity check
    data_set <- check_and_return_datatable(data_set)
    cols <- real_cols(data_set = data_set, cols = cols, function_name = function_name, types = c("character"))
    is.verbose(verbose)

    # Initialization
    if (verbose & length(cols) > 0) {
        printl(function_name, ": I will set some columns as numeric")
        pb <- init_progress_bar(function_name, cols)
    }

    # Computation
    for (col in cols) {
        if (verbose) {
            printl(function_name, ": I am doing the column ", col, ".")
        }
        n_na_init <- sum(is.na(data_set[[col]]))
        if (strip_string) {
            set(data_set, NULL, col, as.numeric_strip(data_set[[col]]))
        }
        else {
            set(data_set, NULL, col, as.numeric(data_set[[col]]))
        }
        if (verbose) {
            printl(function_name, ": ", sum(is.na(data_set[[col]])) - n_na_init,
            " NA have been created due to transformation to numeric.")
        }
        if (verbose) {
            add_a_tick_to_progress_bar(pb)
        }
    }

    # Wrap-up
    return(data_set)
}


#######################################################################################
############################ Set col as character ####################################
#######################################################################################
#' Set columns as character
#'
#' Set as character a column (or a list of columns) from a data.table.
#' @param data_set Matrix, data.frame or data.table
#' @param cols List of column(s) name(s) of data_set to transform into characters. To transform
#' all columns, set it to "auto". (characters, default to "auto")
#' @param verbose Should the function log (logical, default to TRUE)
#' @return  data_set (as a \code{\link[data.table]{data.table}}), with specified columns set as character.
#' @examples
#' # Build a fake data.frame
#' data_set <- data.frame(numCol = c(1, 2, 3), factorCol = as.factor(c("a", "b", "c")))
#'
#' # Set numCol and factorCol as character
#' data_set <- set_col_as_character(data_set, cols = c("numCol", "factorCol"))
#' @import data.table
#' @export
set_col_as_character <- function(data_set, cols = "auto", verbose = TRUE) {
    # Working environment
    function_name <- "set_col_as_character"

    # Sanity check
    data_set <- check_and_return_datatable(data_set)
    is.verbose(verbose)
    cols <- real_cols(data_set = data_set, cols = cols, function_name = function_name)

    # Initialization
    if (verbose & length(cols) > 0) {
        printl(function_name, ": I will set some columns as character")
        pb <- init_progress_bar(function_name, cols)
    }

    # Computation
    for (col in cols) {
        if (verbose) {
            printl(function_name, ": I am doing the column ", col, ".")
        }
        if ((is.character(data_set[[col]])) & verbose) {
            printl(function_name, ": ", col, " is a character, i do nothing.")
        }
        if (! (is.character(data_set[[col]]))) {
            set(data_set, NULL, col, as.character(data_set[[col]]))
        }
        if (verbose) {
            add_a_tick_to_progress_bar(pb)
        }
    }
    return(data_set)
}

#######################################################################################
############################ Set col as Date #########################################
#######################################################################################
#' Set columns as POSIXct
#'
#' Set as POSIXct a character column (or a list of columns) from a data.table.
#' @param data_set Matrix, data.frame or data.table
#' @param cols List of column(s) name(s) of data_set to transform into dates
#' @param format Date's format (function will be faster if the format is provided)
#' (character or list of character, default to NULL).\cr
#' For timestamps, format need to be provided ("s" or "ms" or second or millisecond timestamps)
#' @param verbose Should the function log (logical, default to TRUE)
#' @details
#' set_col_as_date is way faster when format is provided. If you want to identify dates and format
#' automatically, have a look to \code{\link{identify_dates}}. \cr
#' If input column is a factor, it will be returned as a POSIXct column. \cr
#' If \code{cols} is kept to default (NULL) set_col_as_date won't do anything.
#' @return \code{data_set} (as a \code{\link[data.table]{data.table}}), with specified columns set as Date.
#' If the transformation generated only NA, the column is set back to its original value.
#' @examples
#' # Lets build a data_set set
#' data_set <- data.frame(ID = seq_len(5),
#'                   date1 = c("2015-01-01", "2016-01-01", "2015-09-01", "2015-03-01", "2015-01-31"),
#'                   date2 = c("2015_01_01", "2016_01_01", "2015_09_01", "2015_03_01", "2015_01_31")
#'                   )
#'
#' # Using set_col_as_date for date2
#' data_transformed <- set_col_as_date(data_set, cols = "date2", format = "%Y_%m_%d")
#'
#'# Control the results
#' lapply(data_transformed, class)
#'
#'# With multiple formats:
#' data_transformed <- set_col_as_date(data_set, format = list(date1 = "%Y-%m-%d", date2 = "%Y_%m_%d"))
#' lapply(data_transformed, class)
#'
#'# It also works with timestamps
#' data_set <- data.frame(time_stamp = c(1483225200, 1485990000, 1488495600))
#' set_col_as_date(data_set, cols = "time_stamp", format = "s")
#' @import data.table
#' @importFrom lubridate parse_date_time
#' @importFrom stringr str_replace_all
#' @export
set_col_as_date <- function(data_set, cols = NULL, format = NULL, verbose = TRUE) {
    # Working environment
    function_name <- "set_col_as_date"

    # Sanity check
    data_set <- check_and_return_datatable(data_set)
    is.verbose(verbose)
    is.format(format, function_name)
    cols <- parse_date_cols(cols, format, function_name)
    cols <- real_cols(data_set = data_set, cols = cols, function_name = function_name,
                      types = c("character", "factor", "numeric", "integer"))

    # Initialization
    if (verbose & length(cols) > 0) {
        start_time <- proc.time()
        printl(function_name, ": I will set some columns as Date.")
        pb <- init_progress_bar(function_name, cols)
        n_transformed <- length(cols)
    }

    # Computation
    for (col in cols) {
        if (verbose) {
            printl(function_name, ": I am doing the column ", col, ".")
        }
        # Creating data sample to transform
        if (is.factor(data_set[[col]])) {
            data_sample <- levels(data_set[[col]])[data_set[[col]]]
        }
        else {
            data_sample <- data_set[[col]]
        }
        n_na_init <- sum(is.na(data_sample))

        # Get format:
        if (is.character(format) || is.null(format)) {
            col_format <- format
        }
        if (is.list(format)) {
            # Try if list is names
            col_format <- format[[col]]
            if (is.null(col_format)) {
                # If it is not named : get it by index
                col_format <- format[col == cols]
            }
        }
        if (is.character(data_sample)) {
            # If format is NULL, we let R determine the format
            if (is.null(col_format)) {
                # If format is not given, search for it.
                formats_tmp <- identify_dates(data_set[, c(col), with = FALSE], n_test = min(30, nrow(data_set)))
                if (length(formats_tmp) == 0) {
                    printl(function_name, ": ", col,
                           " doesn't seem to be a date, if it really is please provide format.")
                    next
                }
                else {
                    col_format <- formats_tmp[[col]]
                }
            }
            # If it isn't NULL
            if (! is.null(col_format)) {
                # it is faster if it's a format accepted by parse_date_time, so we check that
                format_pdt <- str_replace_all(col_format, "[[:punct:]]", "")
                if (format_pdt %in% parse_date_time_formats()) {
                    result <- parse_date_time(data_sample, orders = format_pdt)
                }
                else {
                    result <- as.POSIXct(data_sample, format = col_format)
                }
            }
        }
        else if (is.numeric(data_sample) & ! is.null(col_format)) {
            if (col_format == "s") {
                result <- as.POSIXct(data_set[[col]], origin = "1970-01-01 00:00:00")
            }
            if (col_format == "ms") {
                result <- as.POSIXct(data_set[[col]] / 1000, origin = "1970-01-01 00:00:00")
            }
        }
        else {
            warning(paste0(function_name, ": I can't handle ", col, ", please see documentation."))
            if (verbose) {
              n_transformed <- n_transformed - 1
            }
            next
        }

        # Deal with NA
        n_na_end <- sum(is.na(result))
        if (n_na_end - n_na_init > 0) {
            if (n_na_end == nrow(data_set) & n_na_init < nrow(data_set)) {
                # If we generated only NA and format wasn't provide, we shouldn't have changer
                # it so we set it bakck to char
                if (verbose) {
                  printl(function_name, ":", " Since i generated only NAs i set ", col, " as it was before.")
                }
                result <- data_sample
            }
            else {
                if (verbose) {
                    printl(function_name, ":", n_na_end - n_na_init,
                           " NA have been created due to transformation to Date.")
                }
            }
        }
        # Assign result
        set(data_set, NULL, col, result)

        # Set log
        if (verbose) {
          add_a_tick_to_progress_bar(pb)
        }
    }
    # Wrap-up
    if (verbose) {
        printl(function_name, ": it took me: ", round((proc.time() - start_time)[[3]], 2),
        "s to transform ", n_transformed, " column(s) to Dates.")
    }
    return(data_set)
}

# Control input format
is.format <- function(format, function_name = "is.format") {
    if (! is.null(format)) {
        if (! (is.character(format) || is.list(format))) {
            stop(paste0(function_name, ": format should either be list of formats or a character."))
        }
        else {
            if (is.list(format) & ! all(sapply(format, is.character))) {
                stop(paste0(function_name, ": format should either be list of character or a character."))
            }
        }
    }
}

# Match cols and format, this function is due to will to keep consitency between previous version
parse_date_cols <- function(cols, format, function_name = "parse_date_cols") {
    if (! is.null(cols)) {
        if (is.list(format)) {
            if (length(format) != length(cols) & any(! cols %in% format)) {
                stop(paste0(function_name,
                            ": you provide cols and format but I'm not able to match them, ",
                            "please feed format as named list."))
            }
            return(cols)
        }
    }
    else {
        cols <- names(format)
        return(cols)
    }
    return(cols)
}
############################################################################################################
########################################## charToFactorOrLogical ##########################################
############################################################################################################
#' Set columns as factor
#'
#'Set columns as factor and control number of unique element, to avoid having too large factors.
#' @param data_set Matrix, data.frame or data.table
#' @param cols List of column(s) name(s) of data_set to transform into factor. To transform all columns
#'  set it to "auto", (characters, default to auto).
#' @param n_levels Max number of levels for factor (integer, default to 53)
#' set it to -1 to disable control.
#' @param verbose Should the function log (logical, default to TRUE)
#' @details
#' Control number of levels will help you to distinguish true categorical columns from just characters
#' that should be handled in another way.
#' @return \code{data_set}(as a \code{\link[data.table]{data.table}}), with specified columns set as factor or logical.
#' @examples
#' # Load messy_adult
#' data(tiny_messy_adult)
#'
#'# we wil change education
#' tiny_messy_adult <- set_col_as_factor(tiny_messy_adult, cols = "education")
#'
#'sapply(tiny_messy_adult[, .(education)], class)
#' # education is now a factor
#' @export
set_col_as_factor <- function(data_set, cols = "auto", n_levels = 53, verbose = TRUE) {
    # Working environment
    function_name <- "set_col_as_factor"

    # Sanity check
    data_set <- check_and_return_datatable(data_set)
    if (! is.numeric(n_levels)) {
      stop(paste0(function_name, ": n_levels should be an integer."))
    }
    cols <- real_cols(data_set = data_set, cols = cols, function_name = function_name)
    is.verbose(verbose)

    # Initialization
    if (verbose) {
        printl(function_name, ": I will set some columns to factor.")
        pb <- init_progress_bar(function_name, cols)
        n_transformed <- 0
        start_time <- proc.time()
    }

    # Computation
    for (col in cols) {
        if (verbose) {
          printl(function_name, ": I am doing the column ", col, ".")
          }
        if (n_levels != - 1) {
            if (! fast_is_there_less_than(data_set[[col]], n_levels)) {
                if (verbose) {
                  printl(function_name, ": ", col, " has more than ",
                         n_levels, " values, i don't transform it.")
                }
                next
            }
        }
        set(data_set, NULL, col, as.factor(data_set[[col]]))
        if (verbose) {
            n_transformed <- n_transformed + 1
            add_a_tick_to_progress_bar(pb)
        }
    }

    # Wrapp up
    if (verbose) {
        printl(function_name, ": it took me: ", round((proc.time() - start_time)[[3]], 2),
        "s to transform ", n_transformed, " column(s) to factor.")
    }
    return(data_set)
}
