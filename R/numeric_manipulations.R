###################################################################################################
############################## find_and_transform_numerics  #########################################
###################################################################################################
#' Identify numeric columns in a data_set set
#'
#'Function to find and transform characters that are in fact numeric.
#' @param data_set Matrix, data.frame or data.table
#' @param cols List of column(s) name(s) of data_set to look into. To check all all columns, set it
#'  to "auto". (characters, default to "auto")
#' @param n_test Number of non-null rows on which to test (numeric, default to 30)
#' @param verbose Should the algorithm talk? (logical, default to TRUE)
#' @details
#' This function is looking for perfect transformation.
#' If there are some mistakes in data_set, consider setting them to NA before. \cr
#' If there are some columns that have no chance to be a match think of removing them from \code{cols}
#' to save some computation time.
#' @section Warning:
#' All these changes will happen \strong{by reference}.
#' @return The data_set set (as a data.table) with identified numeric transformed.
#' @examples
#' # Let's build a data_set set
#' data_set <- data.frame(ID = seq_len(5),
#'                   col1 = c("1.2", "1.3", "1.2", "1", "6"),
#'                   col2 = c("1,2", "1,3", "1,2", "1", "6")
#'                   )
#'
#'# using the find_and_transform_numerics
#' find_and_transform_numerics(data_set, n_test = 5)
#' @import data.table
#' @export
find_and_transform_numerics <- function(data_set, cols = "auto", n_test = 30, verbose = TRUE) {
    # Working environment
    function_name <- "find_and_transform_numerics"

    # Sanity check
    data_set <- check_and_return_datatable(data_set)
    is.verbose(verbose)
    cols <- real_cols(data_set = data_set, cols = cols, function_name = function_name)
    # Initialization
    start_time <- proc.time()

    # Computation
    # identify
    numerics <- identify_numerics(data_set, cols = cols, n_test = n_test, verbose = verbose)
    if (verbose) {
        printl(function_name, ": It took me ", round((proc.time() - start_time)[[3]], 2),
        "s to identify ", length(numerics$dont_strip) + length(numerics$strip),
        " numerics column(s), i will set them as numerics")
    }

    # Format
    if (is.null(numerics$strip) & is.null(numerics$dont_strip)) {
        if (verbose) {
            printl(function_name,
            ": There are no numerics to transform.",
            "(If i missed something consider using set_col_as_numeric to transform it)")
        }
        return(data_set)
    }
    start_time <- proc.time()
    if (length(numerics$dont_strip) > 0 || length(numerics$strip) > 0) {
        data_set <- set_col_as_numeric(data_set, cols = numerics$dont_strip, strip_string = FALSE, verbose = FALSE)
        data_set <- set_col_as_numeric(data_set, cols = numerics$strip, strip_string = TRUE, verbose = FALSE)

        if (verbose) {
            printl(function_name, ": It took me ", round((proc.time() - start_time)[[3]], 2),
            "s to transform ", length(numerics$dont_strip) + length(numerics$strip),
            " column(s) to a numeric format.")
        }
    }


    # Wrap-up
    return(data_set)
}

###################################################################################################
############################## identify_numerics  #################################################
###################################################################################################
#' @import data.table
identify_numerics <- function(data_set, cols = "auto", n_test = 30, verbose = TRUE, ...) {
    # Working environment
    function_name <- "identify_numerics"

    # Sanity check
    data_set <- check_and_return_datatable(data_set)
    n_test <- control_nb_rows(data_set = data_set, nb_rows = n_test, function_name = function_name,
    variable_name = "n_test")
    is.verbose(verbose)
    cols <- real_cols(data_set = data_set, cols = cols, function_name = function_name, types = "character")
    # Initialization
    numerics_cols_dont_strip <- NULL
    numerics_cols_strip <- NULL
    if (verbose) {
        pb <- init_progress_bar(function_name, names(data_set))
    }

    # Computation
    for (col in cols) {
        # Something is performed only if col is in a character format

        # Get a few lines that aren't NA, NULL nor ""
        data_sample <- find_n_first_non_null(data_set[[col]], n_test)

        # We check only columns that contains something (not NA, NULL, "")
        if (length(data_sample) > 0) {
            format <- identify_numerics_formats(data_set = data_sample)
            if (format == NUMERIC_COL_NOT_TO_STRIP) {
                numerics_cols_dont_strip <- c(numerics_cols_dont_strip, col)
            }
            if (format == NUMERIC_COL_TO_STRIP) {
                numerics_cols_strip <- c(numerics_cols_strip, col)
            }
        }
        if (verbose) {
            add_a_tick_to_progress_bar(pb)
        }
    }

    # Wrap-up
    return(list(dont_strip = numerics_cols_dont_strip, strip = numerics_cols_strip))
}


#######################################################################################
############################## Identify numeric format  ##############################
#######################################################################################
NUMERIC_COL_NOT_TO_STRIP <- "not_strip"
NUMERIC_COL_TO_STRIP <- "strip"
identify_numerics_formats <- function(data_set) {
    if (! is.character(data_set)) {
        stop("identify_numerics_formats: data_set should be some characters")
    }

    # Check convertions either strip or don't strip
    options(warn = - 1) # Localy disable warning (we are trying to transform stuff if there is a mistake we skip it)
    data_set_converted <- as.numeric(data_set)
    data_set_converted_stripped <- as.numeric_strip(data_set)
    options(warn = 0)
    # Wrap-up
    if (identical(is.na(data_set_converted), is.na(data_set))) {
        return(NUMERIC_COL_NOT_TO_STRIP)
    }
    if (identical(is.na(data_set_converted_stripped), is.na(data_set))) {
        return(NUMERIC_COL_TO_STRIP)
    }
    return("Not a numeric")
}

#######################################################################################
############################## As numerical strip  ###################################
#######################################################################################
as.numeric_strip <- function(x) {
    return(as.numeric(gsub(",", ".", x)))
}
