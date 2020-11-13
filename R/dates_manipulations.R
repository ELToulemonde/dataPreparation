###################################################################################################
########################## find_and_transform_dates #################################################
###################################################################################################
#' Identify date columns
#'
#'Find and transform dates that are hidden in a character column. \cr
#' It use a bunch of default formats, and you can also add your own formats.
#' @param cols List of column(s) name(s) of data_set to look into. To check all all columns, set it
#'  to "auto". (characters, default to "auto")
#' @param data_set Matrix, data.frame or data.table
#' @param formats List of additional Date formats to check (see \code{\link{strptime}})
#' @param n_test Number of non-null rows on which to test (numeric, default to 30)
#' @param ambiguities How ambiguities should be treated (see details in ambiguities section)
#' (character, default to IGNORE)
#' @param verbose Should the algorithm talk? (Logical, default to TRUE)
#' @details
#' This function is using \code{\link{identify_dates}} to find formats. Please see it's documentation.
#' In case \code{\link{identify_dates}} doesn't find wanted formats you can either provide format
#'  in param \code{formats}
#' or use \code{\link{set_col_as_date}} to force transformation.
#' @section Ambiguity:
#' Ambiguities are often present in dates. For example, in date: 2017/01/01, there is no way to know
#'  if format is YYYY/MM/DD or YYYY/DD/MM. \cr
#' Some times ambiguity can be solved by a human. For example
#'  17/12/31, a human might guess that it is YY/MM/DD, but there is no sure way to know.  \cr
#' To be safe, find_and_transform_dates doesn't try to guess ambiguities. \cr
#' To answer ambiguities problem, param \code{ambiguities} is now available. It can take one of the following values
#' \itemize{
#' \item \code{IGNORE} function will then take the first format which match (fast, but can make some mistakes)
#' \item \code{WARN} function will try all format and tell you - via prints - that there are
#' multiple matches (and won't perform date transformation)
#' \item \code{SOLVE} function will try to solve ambiguity by going through more lines, so will be slower.
#' If it is able to solve it, it will transform the column, if not it will print the various acceptable formats.
#' }
#' If there are some columns that have no chance to be a match think of removing them from \code{cols}
#' to save some computation time.
#' @return data_set set (as a data.table) with identified dates transformed by \strong{reference}.
#' @examples
#' # Load exemple set
#' data(messy_adult)
#' head(messy_adult)
#' # using the find_and_transform_dates
#' find_and_transform_dates(messy_adult, n_test = 5)
#' head(messy_adult)
#'
#'# Example with ambiguities
#' \dontrun{
#' require(data.table)
#' data(messy_adult) # reload data
#' # Add an ambiguity by sorting date1
#' messy_adult$date1 = sort(messy_adult$date1, na.last = TRUE)
#' # Try all three methods:
#' result_1 = find_and_transform_dates(copy(messy_adult))
#' result_2 = find_and_transform_dates(copy(messy_adult), ambiguities = "WARN")
#' result_3 = find_and_transform_dates(copy(messy_adult), ambiguities = "SOLVE")
#' }
#' # "##NOT RUN:" mean that this example hasn't been run on CRAN since its long. But you can run it!
#' @export

find_and_transform_dates <- function(data_set, cols = "auto", formats = NULL,
                                     n_test = 30, ambiguities = "IGNORE", verbose = TRUE) {
    # Working environement
    function_name <- "find_and_transform_dates"

    # Sanity check
    data_set <- check_and_return_datatable(data_set)
    is.verbose(verbose)
    is_ambiguities(ambiguities, function_name)
    cols <- real_cols(data_set = data_set, cols = cols, function_name = function_name)
    # initialization
    start_time <- proc.time()

    # Computation
    # First we find dates
    formats_found <- identify_dates(data_set, cols = cols, formats = formats, n_test = n_test,
    ambiguities = ambiguities, verbose = verbose)
    if (verbose) {
        printl(function_name, ": It took me ", round((proc.time() - start_time)[[3]], 2), "s to identify formats")
    }
    # Now we transform (if there is something to transform)
    if (length(formats_found) < 1) {
        if (verbose) {
            printl(function_name, ": There are no dates to transform.
                   (If i missed something please provide the date format in inputs or
                   consider using set_col_as_date to transform it).")
        }
        return(data_set)
    }
    start_time <- proc.time()
    data_set <- set_col_as_date(data_set, format = formats_found, verbose = FALSE)
    if (verbose) {
        printl(function_name, ": It took me ", round((proc.time() - start_time)[[3]], 2), "s to transform ",
        length(formats_found), " columns to a Date format.")
    }
    return(data_set)
}


is_ambiguities <- function(ambiguities, function_name) {
    if (! is.character(ambiguities) || ! ambiguities %in% c("IGNORE", "WARN", "SOLVE")) {
        stop(paste0(function_name, ": ambiguities should be either IGNORE, WARN or SOLVE."))
    }
}

###################################################################################
############################## identify_dates #####################################
###################################################################################
#' Identify date columns
#'
#'Function to identify dates columns and give there format. It use a bunch of default formats.
#'But you can also add your own formats.
#' @param data_set Matrix, data.frame or data.table
#' @param cols List of column(s) name(s) of data_set to look into. To check all all columns, set it
#'  to "auto". (characters, default to "auto")
#' @param formats List of additional Date formats to check (see \code{\link{strptime}})
#' @param n_test Number of non-null rows on which to test (numeric, default to 30)
#' @param ambiguities How ambiguities should be treated (see details in ambiguities section)
#' (character, default to IGNORE)
#' @param verbose Should the algorithm talk? (Logical, default to TRUE)
#' @details
#' This function is looking for perfect transformation.
#' If there are some mistakes in data_set, consider setting them to NA before. \cr
#' In the unlikely case where you have numeric higher than \code{as.numeric(as.POSIXct("1990-01-01"))}
#' they will be considered as timestamps and you might have some issues. On the other side,
#' if you have timestamps before 1990-01-01, they won't be found, but you can use
#' \code{\link{set_col_as_date}} to force transformation.
#' @section Ambiguity:
#' Ambiguities are often present in dates. For example, in date: 2017/01/01, there is no way to know
#'  if format is YYYY/MM/DD or YYYY/DD/MM. \cr
#' Some times ambiguity can be solved by a human. For example
#'  17/12/31, a human might guess that it is YY/MM/DD, but there is no sure way to know.  \cr
#' To be safe, find_and_transform_dates doesn't try to guess ambiguities. \cr
#' To answer ambiguities problem, param \code{ambiguities} is now available. It can take one of the following values
#' \itemize{
#' \item \code{IGNORE} function will then take the first format which match (fast, but can make some mistakes)
#' \item \code{WARN} function will try all format and tell you - via prints - that there are multiple matches
#'  (and won't perform date transformation)
#' \item \code{SOLVE} function will try to solve ambiguity by going through more lines, so will be slower.
#' If it is able to solve it, it will transform the column, if not it will print the various acceptable formats.
#' }
#' @return
#' A named list with names being col names of \code{data_set} and values being formats.
#' @examples
#' # Load exemple set
#' data(messy_adult)
#' head(messy_adult)
#' # using the find_and_transform_dates
#' identify_dates(messy_adult, n_test = 5)
#' @export
identify_dates <- function(data_set, cols = "auto", formats = NULL, n_test = 30,
                           ambiguities = "IGNORE", verbose = TRUE) {
    # Working environement
    function_name <- "identify_dates"

    # Sanity check
    data_set <- check_and_return_datatable(data_set)
    n_test <- control_nb_rows(data_set = data_set, nb_rows = n_test,
                              function_name = function_name, variable_name = "n_test")
    is.verbose(verbose)
    cols <- real_cols(data_set = data_set, cols = cols, function_name = function_name)
    # Initialization
    formats_found <- list()
    format <- NULL # Initialize format to NULL to avoid considering it by mistake as the function format
    # Computation
    if (verbose) {
        pb <- init_progress_bar(function_name, names(data_set))
    }
    for (col in cols) {
        # We search dates only in characters
        if (is.character(data_set[[col]]) ||
            is.numeric(data_set[[col]]) ||
            (is.factor(data_set[[col]]) & is.character(levels(data_set[[col]])))) {
            # Look for the good format
            format <- identify_dates_formats(data_set = data_set[[col]], formats = formats,
                                             n_test = n_test, ambiguities = ambiguities)
            # There is an ambiguity (second check is redondant but for code understanding)
            if (length(format) > 1 & ambiguities != "IGNORE") {
                if (ambiguities == "SOLVE") {
                    if (verbose) {
                        printl(function_name, ": column ", col, " seems to have an ambiguity, I try to solve it.")
                    }
                    n_prev <- 30 # Keep track of previous n to make sure that while loop stops.
                    n <- 30 * 10
                    while (length(format) > 1 & n_prev != n) {
                        n <- max(n_prev * 10, nrow(data_set))
                        format <- identify_dates_formats(data_set = data_set[[col]],
                                                         formats = format, n_test = n,
                                                         ambiguities = ambiguities)
                    }
                }
                if (length(format) > 1 & verbose) { # If ambiguity wasn't solved
                    printl(function_name, ": column ", col,
                           " seems to be a date but there is an ambiguity in the format. ",
                           " The following formats seems to work: ", paste(format, collapse = ", "))
                    format <- NULL
                }
            }
        }

        # If a format has been found we note it
        if (length(format) == 1) {
            formats_found[[col]] <- format
        }
        if (verbose) {
            add_a_tick_to_progress_bar(pb)
        }
    }
    # Wrap-up
    return(formats_found)
}

# To-do
# Handle month as french or english character
# Better output format?
# Compute expected string length if format is good?


############################################################################################################
########################################## identify_dates_formats ################################################
############################################################################################################
#
#
identify_dates_formats <- function(data_set, formats = NULL, n_test = 30, ambiguities = "IGNORE") {
    # Working environement
    function_name <- "identify_dates_formats"
    # Sanity check
    if (! (is.character(data_set) ||
        is.numeric(data_set) ||
        (is.factor(data_set) & is.character(levels(data_set))))) {
        stop(paste0(function_name, ": data_set should be some characters, numerics or factor of character."))
    }

    # Initalization
    date_sep <- get_possible_separators()
    # Get a few lines that aren't NA, NULL nor ""
    if (is.factor(data_set)) { # If it's a factor, we take levels to convert
        data_sample <- find_n_first_non_null(levels(data_set), n_test)
    }
    else {
        data_sample <- unique(find_n_first_non_null(data_set, n_test))  # Take unique to avoid useless computations
    }

    # Computation
    temp_format <- NULL
    if (length(data_sample) > 0) {
        # Identify potentially used separator by checking it split date in at last two elements
        if (is.character(data_sample)) {
            date_sep_tmp <- date_sep[sapply(date_sep, function(x)length(grep(x, data_sample))) > 0]
            if (length(date_sep_tmp) == 0) {
                return(NULL) # No separator means no dates
            }

            # Check formats with "date_hours" only if there are more than 10 characters
            date_hours <- max(sapply(data_sample, nchar), na.rm = TRUE) > 10

            # Build list of all formats to check
            defaultDateFormats <- get_all_possible_dates_formats(date_sep_tmp, date_hours = date_hours)
            formats_tmp <- unique(c(defaultDateFormats, formats))

            #  Check formats
            for (format in formats_tmp) {
                converted <- as.POSIXct(data_sample, format = format)
                un_converted <- format(converted, format = format)
                if (control_date_conversion(un_converted, data_sample)) {
                    temp_format <- c(temp_format, format)
                    if (ambiguities == "IGNORE") {
                      # If don't care about possible ambiguities: return found format
                      break
                    }
                }
            }
        }
        if (is.numeric(data_sample)) {
            temp_format <- identify_time_stamps_formats(data_set = data_sample)
        }
    }
    return(temp_format)
}


# identify time_stamps_formats
identify_time_stamps_formats <- function(data_set) {
    # Working environement
    function_name <- "identify_time_stamps_formats"
    # Sanity check
    if (! is.numeric(data_set)) {
        stop(paste0(function_name, ": data_set should be some numerics."))
    }

    # Expect timestamp in seconds
    date <- as.POSIXct(data_set, origin = "1970-01-01 00:00:00")
    year <- as.numeric(format(date, "%Y"))

    if (all(year > 1990) & all(year < 2100)) {
        return("s")
    }

    # Expect timestamp in milliseconds
    date <- as.POSIXct(data_set / 1000, origin = "1970-01-01 00:00:00")
    year <- as.numeric(format(date, "%Y"))

    if (all(year > 1990) & all(year < 2100)) {
        return("ms")
    }
    # Not a date
    return(NULL)
}

# Control that date is the same (with more checks like: without 0 or tolower? or boths?)
# To-do find a cleaner way to test all comparaisons
control_date_conversion <- function(un_converted, original) {
    without_0 <- gsub("(?<=^|(?![:])[[:punct:]])0", "", un_converted, perl = TRUE)
    tolowers <- tolower(un_converted)
    tolowers_without_0 <- tolower(without_0)
    without_first_0 <- sub("^0", "", un_converted)
    tolowers_without_first_0 <- tolower(without_first_0)
    un_con_list <- list(un_converted, without_0, tolowers, tolowers_without_0,
                        without_first_0, tolowers_without_first_0)

    original_with_spaces_after_sep <- gsub("(?<=[[:punct:]]) ", "", original, perl = TRUE)
    original_trimws <- trimws(original)
    original_with_spaces_after_sep_trimws <- gsub("(?<=[[:punct:]]) ", "", original_trimws, perl = TRUE)
    original_list <- list(original, original_with_spaces_after_sep,
                          original_with_spaces_after_sep_trimws, original_trimws)
    return(any(un_con_list %in% original_list))
}

#######################################################################################
############################## Unify dates types #####################################
#######################################################################################
#' Unify dates format
#'
#' Unify every column in a date format to the same date format.
#' @param data_set Matrix, data.frame or data.table
#' @param format Desired target format: Date, POSIXct or POSIXlt, (character, default to Date)
#' @details
#' This function only handle Date, POSIXct and POSIXlt dates.  \cr
#' POSIXct format is a bit slower than Date but can keep hours-min.
#' @return The same data_set set but with dates column with the desired format.
#' @examples
#' # build a data.table
#' require(data.table)
#' data_set <- data.table( column1 = as.Date("2016-01-01"), column2 = as.POSIXct("2017-01-01") )
#'
#' # Use the function
#' data_set = date_format_unifier(data_set, format = "Date")
#'
#' # Control result
#' sapply(data_set, class)
#' # return Date for both columns
#' @import data.table
#' @export
date_format_unifier <- function(data_set, format = "Date") {
    # Working environement
    function_name <- "date_format_unifier"
    # Sanity check
    data_set <- check_and_return_datatable(data_set)
    if (! any(format %in% c("Date", "POSIXct", "POSIXlt"))) {
        stop(paste0(function_name, ": only format: Date, POSXIct, POSIXlt are implemented. You gave: ", format, "."))
    }

    # Initialization
    date_cols <- names(data_set)[sapply(data_set, is.date)]
    format_function <- paste0("as.", format)

    # Computation
    for (col in date_cols) {
        # Only change dates that don't have the right format
        if (! format %in% class(data_set[[col]])) {
            set(data_set, NULL, col, get(format_function)(data_set[[col]]))
        }
    }

    # Wrap-up
    return(data_set)
}

#######################################################################################
############################## Is in a date format  ##################################
#######################################################################################
# Check if an object is in a date format
# Are handeled: Date, POSIXct, POSIXlt
# Excension of is.Date, is.POSIXct...
#' @importFrom lubridate is.POSIXct is.POSIXlt is.POSIXt
is.date <- function(x) {
    return(is.POSIXct(x) ||
        is.Date(x) ||
        is.POSIXlt(x) ||
        is.POSIXt(x))
}


########################################################################################
########################################## get_all_possible_dates_formats ####################
########################################################################################
# Ensemble of formats
get_all_possible_dates_formats <- function(date_sep = c(",", "/", "-", "_", ":"), date_hours = TRUE) {
    # Initialization
    hours_format <- c("%H:%M:%S", "%H:%M", "%H")
    base_year <- c("%Y", "%y")
    base_month <- c("%b", "%B", "%m")
    base_day <- c("%d")

    # Computation
    # Build list of possible formats
    date_formats <- NULL
    for (separator_1 in date_sep) {
        for (separator_2 in date_sep) {
            for (year in base_year) {
                for (month in base_month) {
                    for (day in base_day) {
                        tempList <- c(
                        paste(paste(year, month, sep = separator_1), day, sep = separator_2),
                        paste(paste(year, day, sep = separator_1), month, sep = separator_2),
                        paste(paste(day, month, sep = separator_1), year, sep = separator_2),
                        paste(paste(month, day, sep = separator_1), year, sep = separator_2)
                        )

                        date_formats <- c(date_formats, tempList)
                    }
                }
            }
        }
    }

    # Complete the list with the same formats but with a time format at the and separed by
    # a ' ' or a "T" and optionaly with a "Z" at the end
    formats <- c(date_formats, hours_format)
    if (date_hours) {
        for (datesFormat in date_formats) {
            for (hoursFormat in hours_format) {
                formats <- c(formats, paste(datesFormat, hoursFormat))
            }
        }
    }

    # Add optional time zone at the end.
    # formats <- c(formats, paste(formats, "%z"))

    # Wrap-up
    return(formats)
}


#######################################################################################
############################## Format for parse_date_time ############################
#######################################################################################
# Code is commented and result hard written so that it's way faster. You should keep it that way
#' @importFrom lubridate parse_date_time
#' @importFrom stringr str_replace_all
parse_date_time_formats <- function() {
    # Get complete liste of format
    # listOfFastFormat <- get_all_possible_dates_formats()
    # result <- NULL
    # for (format in listOfFastFormat) {
    #   temp <- parse_date_time(format(Sys.Date(), format), orders = format)== Sys.Date()
    #   if (is.na(temp)) {
    #     temp <- FALSE
    #   }
    #   if (temp) {
    #     result <- c(result, format)
    #   }
    # }
    # result <- sapply(result, function(x)str_replace_all(x, "[[:punct:]]", ""))
    # result <- unique(result)
    #
    result <- c("Ybd", "Ydb", "dbY", "bdY", "YBd", "YdB", "dBY", "BdY", "Ymd",
    "Ydm", "dmY", "mdY", "ybd", "ydb", "dby", "bdy", "yBd", "ydB",
    "dBy", "Bdy", "ymd", "ydm", "dmy", "mdy", "Ybd HMS", "Ybd HM",
    "Ybd H", "Ydb HMS", "Ydb HM", "Ydb H", "dbY HMS", "dbY HM", "dbY H",
    "bdY HMS", "bdY HM", "bdY H", "YBd HMS", "YBd HM", "YBd H", "YdB HMS",
    "YdB HM", "YdB H", "dBY HMS", "dBY HM", "dBY H", "BdY HMS", "BdY HM",
    "BdY H", "Ymd HMS", "Ymd HM", "Ymd H", "Ydm HMS", "Ydm HM", "Ydm H",
    "dmY HMS", "dmY HM", "dmY H", "mdY HMS", "mdY HM", "mdY H", "ybd HMS",
    "ybd HM", "ybd H", "ydb HMS", "ydb HM", "ydb H", "dby HMS", "dby HM",
    "dby H", "bdy HMS", "bdy HM", "bdy H", "yBd HMS", "yBd HM", "yBd H",
    "ydB HMS", "ydB HM", "ydB H", "dBy HMS", "dBy HM", "dBy H", "Bdy HMS",
    "Bdy HM", "Bdy H", "ymd HMS", "ymd HM", "ymd H", "ydm HMS", "ydm HM",
    "ydm H", "dmy HMS", "dmy HM", "dmy H", "mdy HMS", "mdy HM", "mdy H"
    )

    # Wrap-up
    return(result)
}
