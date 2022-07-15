###################################################################################################
############################## findNFirstNonNull #################################################
###################################################################################################
# Description
# Dichotomic search of non-null elements
# Search from 1 to n then n+1 to 2*n then 2*n+1 to 4*n...
find_n_first_non_null <- function(object, n) {
    # Working environment

    # Initialization
    if (length(object) <= n) {
        return(object[! is.null(object) & ! is.na(object) & object != ""])
    }
    max_power <- round(log(length(object)) / log(10) + 1)
    result <- NULL

    # Computation
    for (mult in 1 : max_power) {
        indexes <- (10 ^ (mult - 1)) : min(10 ^ mult - 1, nrow(object))
        object_sample <- object[indexes]

        object_sample <- object_sample[! is.null(object_sample) &
            ! is.na(object_sample) &
            object_sample != ""]

        if (length(object_sample) > 0) {
            result <- c(result, object_sample[1 : min(n - length(result), length(object_sample))])
        }

        # If we found enough elements, we stop
        if (length(result) == n) {
            return(result)
        }
    }
    # Wrap-up
    return(result)
}

###################################################################################################
########################################## checkAndReturnDataTable ###############################
###################################################################################################
# Description: Verify that the provided data_set set is indeed a data_set set
# @param data_set an object. We will check that it is a data_set set
# @param name name of the object passed in order to make an understandable log
#' @import data.table
check_and_return_datatable <- function(data_set, data_set_name = "data_set") {
    if (! (is.data.table(data_set) ||
        is.data.frame(data_set) ||
        is.matrix(data_set))) {
        stop(paste(data_set_name, "should be a data.table, a data.frame or a matrix."))
    }
    if (nrow(data_set) < 1) {
        stop(paste(data_set_name, "should have at least have 1 line."))
    }
    if (ncol(data_set) < 1) {
        stop(paste(data_set_name, "should have at least have 1 column."))
    }

    if (! is.data.table(data_set)) {
        if (is.data.frame(data_set)) {
            setDT(data_set)
        }
        else {
            data_set <- as.data.table(data_set)
        }
    }
    # Control col names
    if (length(unique(names(data_set))) < length(names(data_set))) {
        warning(paste0(data_set_name, ": has column names in double : you should take care of it.",
            "I changed them to be unique."))
        setDT(data_set, check.names = TRUE)
    }
    # Since we are using set in all the package, we make sure that data_set is overallocate,
    # see data.table doc, among it
    # see: https://github.com/Rdatatable/data.table/issues/755
    data_set <- alloc.col(data_set)
    # Wrap-up
    return(data_set)
}


###################################################################################################
######################################### Control verbose ########################################
###################################################################################################

is.verbose <- function(verbose, function_name = "is.verbose") {
    if (! is.logical(verbose)) {
        stop(function_name, " verbose should be logical (TRUE or FALSE).")
    }
}
is.verbose_level <- function(verbose, function_name = "is.verbose", max_level = 2) {
    if (! is.logical(verbose) & ! verbose %in% 1 : max_level) {
        stop(function_name, " verbose should be logical (TRUE or FALSE) or an integer lower than ", max_level, ".")
    }
}

is.share <- function(object, object_name = "variable", function_name = "is.share") {
    if (! is.numeric(object)) {
        stop(function_name, ": ", object_name, " should be a numeric between 0 and 1.")
    }
    if (object < 0 || object > 1) {
        stop(function_name, ": ", object_name, " should be a numeric between 0 and 1.")
    }
}

###################################################################################################
######################################### check if col is in dataset #############################
###################################################################################################
# Check if a column or a list of column is in a data.table
is.col <- function(data_set, cols = NULL, function_name = "is.col", data_set_name = "data_set") {
    # Sanity check
    if (! (is.data.table(data_set) ||
        is.data.frame(data_set) ||
        is.matrix(data_set))) {
        stop("is.col: data_set should be a data.table, data.frame or matrix")
    }

    # Initialization
    # Arguments for log

    # Computation
    for (col in cols) {
        if (! col %in% names(data_set)) {
            stop(paste(function_name, ":", col, " should be column of", data_set_name))
        }
    }
}

# Reduce list of cols to only cols in data_set set
# Reduce columns that aren't of the wanted type
# Handle "auto" value
# @param data_set Matrix, data.frame or data.table (with only numeric, integer, factor, logical, character columns).
# @param cols list of column(s) name(s) of data_set to control. (Default to auto, all cols that match types)
# @param function_name name of the function where it's called from. For LOG. (Character, default to "real_cols")
# @param types types of wanted columns.

real_cols <- function(data_set, cols, function_name = "real_cols", types = NULL, verbose = TRUE) {
    # If NULL cols
    if (is.null(cols) || length(cols) == 0) { # length cols is for the case where cols == character(0)
        return(NULL)
    }

    # If auto cols
    if (all(cols == "auto")) {
        cols <- names(data_set)
    }
    else {
        cols <- reduce_cols_to_existing_ones(data_set = data_set, cols = cols, function_name = function_name,
            verbose = verbose)
        verbose <- FALSE
    }

    # Filter cols that aren't of the right type
    if (! is.null(types)) {
        cols <- reduce_cols_to_correct_type(cols = cols, data_set = data_set, function_name = function_name,
            types = types, verbose = verbose)
    }

    # Wrap-up
    return(cols)
}

reduce_cols_to_existing_ones <- function(data_set, cols, function_name, verbose) {
    error_list <- ! cols %in% names(data_set)
    if (sum(error_list) > 0) {
        if (verbose) {
            printl(function_name, ": ", paste0(cols[error_list], collapse = ", "),
            " aren\'t columns of the table, i do nothing for those variables")
        }
        cols <- cols[! error_list] # Reduce list of col
    }
    return(cols)
}


reduce_cols_to_correct_type <- function(cols, data_set, function_name, types, verbose) {

    if (all(types == "date")) {
        col_is_of_wrong_type <- ! cols %in% names(data_set)[sapply(data_set, is.date)]
    }
    else if (all(types == "numeric")) {
        col_is_of_wrong_type <- ! cols %in% names(data_set)[sapply(data_set, is.numeric)]
    }
    else {
        col_is_of_wrong_type <- ! cols %in% names(data_set)[sapply(data_set, class) %in% types]
    }

    if (sum(col_is_of_wrong_type) > 0) {
        if (verbose) {
            printl(function_name, ": ", print(cols[col_is_of_wrong_type], collapse = ", "),
            " aren\'t columns of types ", paste(types, collapse = " or "), " i do nothing for those variables.")
        }
        cols <- cols[! col_is_of_wrong_type]
    }

    return(cols)
}


###################################################################################################
########################################## getPossibleSeparators #################################
###################################################################################################
# Separators are used in multiple functions, so i put them here!
get_possible_separators <- function() {
    separator_list <- c(",", "/", "-", "_", ":", " ")
    return(separator_list)
}


###################################################################################################
############################## Print list ########################################################
###################################################################################################
# To stop using print(paste())
printl <- function(...) {
    args <- list(...)
    print(paste(args, collapse = ""))
}

# Super progress bar
# Using progress and storing some info
# Use when you build a progress bar for names
#' @importFrom progress progress_bar
init_progress_bar <- function(function_name, cols_names) {
    if (length(cols_names) == 0) {
        # No prossessing to do, so no progress bar
        return(NULL)
    }
    pb <- progress_bar$new(
    format = paste0("   ", function_name, " [:bar] :percent in :elapsed \r"),
    total = length(cols_names), clear = FALSE, width = 60)
    return(pb)
}
add_a_tick_to_progress_bar <- function(pb) {
    pb$tick()
}

###################################################################################################
############################## Control number of rows ############################################
###################################################################################################
# Control that you don't want to check an absurd number of rows
#
# @param data_set matrix, data.frame or data.table
# @param nb_rows number of rows you want to check (integer)
# @param function_name to log the name of the function which is calling this one (character)
# @param variable_name name of the variable you are controling (character)
# @return nb_rows might have been changed to be conform
# @details
# If nb_rows is greater than the numer of line of data_set, it will be set to nrow(data_set).
# If nb_rows is loawer than 1, it will be set to min(30, nrow(data_set)).
# If nb_rows is a float it will be rounded
control_nb_rows <- function(data_set, nb_rows, function_name = "", variable_name = "nb_rows") {
    # Sanity check
    if (! is.numeric(nb_rows)) {
        stop(paste0(function_name, ": ", variable_name, " should be a numeric."))
    }
    data_set <- check_and_return_datatable(data_set)

    # Initialization
    nb_rows <- round(nb_rows)

    # Computation
    if (nb_rows > nrow(data_set)) {
        nb_rows <- nrow(data_set)
        warning(paste0(function_name, ": You want to check more rows than there are in data_set, I set ",
        variable_name, " to ", nb_rows, "."))
    }
    if (nb_rows < 1) {
        nb_rows <- min(30, nrow(data_set))
        warning(paste0(function_name,
        ": You want to check at least a few rows than there are in data_set, I set ",
        variable_name, " to ", nb_rows, "."))
    }

    # Wrap-up
    return(nb_rows)
}


###################################################################################################
############################## Return true aggregation functions #################################
###################################################################################################
is.agg_function <- function(functions, function_name = "is.agg_function") {
    for (fun in functions) {
        # Check it
        # check type
        if (! is.character(fun)) {
            stop(paste0(function_name, ": functions should be a list of names (as character) of functions."))
        }
        if (! exists(x = fun)) {
            warning(paste0(function_name, ": ", fun, " doesn't exist, it won't be used."))
            functions <- functions[functions != fun]
        }
        else {
            if (! is.function(get(fun))) {
                warning(paste0(function_name, ": ", fun, " is not a function, it won't be used."))
                functions <- functions[functions != fun]
            }
            else {
                # check aggregation
                if (length(get(fun)(1 : 3)) != 1) {
                    warning(paste0(function_name, ": ", fun, " is not an aggregation function, it won't be used.",
                    " An aggregation function is a function that for multiple input return only one, exemple: sum."))
                    functions <- functions[functions != fun]
                }
            }
        }
    }
    # Wrap-up
    return(functions)
}


# Make if a function
#
# Taking a constant and transforming it into a function. If it's a function don't do anything. \cr
# Control that tehe function is giving the wanted type of input
# @param object object to be transformed and control
# @param function_name for log, from where do you call it (character, default to "function.maker")
# @param object_name for log, object name when called (character)
# @param type what type of output is expected for built function (character, numeric or logical)
# @return A function
function.maker <- function(object, type, function_name = "function.maker", object_name = "object") {
    built_function <- NULL

    # If it's a constant
    if (any(class(object) %in% c("numeric", "integer", "factor", "logical", "date", "character"))) {
        built_function <- function(...) object
    }
    # If it is a function
    if (is.function(object)) {
        built_function <- object
    }
    # Control of function
    if (! is.null(built_function)) {
        # On numeric functions
        if (type == "numeric") {
            if (length(built_function(1 : 3)) == 1) {
                if (! is.numeric(built_function(1 : 3))) {
                    stop(paste0(function_name, ": ", object_name, " should be or should return a numeric."))
                }
                if (is.na(built_function(c(1, NA)))) {
                    warning(paste0(function_name, ": ", object_name, " is not handling NAs, it won't do anything."))
                }
                return(built_function)
            }
        }
        # On logical functions
        if (type == "logical") {
            if (length(built_function(c(TRUE, FALSE))) == 1) {
                if (! is.logical(built_function(c(TRUE, FALSE)))) {
                    stop(paste0(function_name, ": ", object_name, " should be or should return a logical."))
                }
                if (is.na(built_function(c(TRUE, NA)))) {
                    warning(paste0(function_name, ": ", object_name, " is not handling NAs, it won't do anything."))
                }
                return(built_function)
            }
        }
        # On character functions
        if (type == "character") {
            if (length(built_function(c("a", "b"))) == 1) {
                if (! is.character(built_function(c("a", "b")))) {
                    stop(paste0(function_name, ": ", object_name, " should be or should return a character."))
                }
                if (is.na(built_function(c("a", NA)))) {
                    warning(paste0(function_name, ": ", object_name, " is not handling NAs, it won't do anything."))
                }
                return(built_function)
            }
        }
    }
    # Wrap-up, if we reached here, we can't handle what was provided
    stop(paste0(object_name, ": is in a shape that isn't handled, please provide constant or aggregation function."))
}


#################################################################################################################
#### make new col name #########################################################################################
#################################################################################################################
make_new_col_name <- function(new_col, col_names) {
    # Working environement
    function_name <- "make_new_col_name"

    # Sanit check
    if (! is.character(new_col) || ! is.character(col_names)) {
        stop(paste0(function_name, ": new_col and col_names should be character."))
    }

    # Initialization
    new_col <- gsub("[[:punct:]]", ".", new_col) # replace special characters
    if (! new_col %in% col_names) {
        return(new_col)
    }
    i <- 1
    while (paste0(new_col, i) %in% col_names) {
        i <- i + 1
    }
    return(paste0(new_col, i))
}


#################################################################################################################
############################ build name separator ##############################################################
#################################################################################################################
build_name_separator <- function(args) {
    name_separator <- "."
    if (! is.null(args[["name_separator"]])) {
        if (is.character(args[["name_separator"]]) & length(args[["name_separator"]]) == 1) {
            name_separator <- args[["name_separator"]]
        }
        else {
            stop("name_separator should be a character.")
        }
    }
    return(name_separator)
}


#################################################################################################################
############################ build factor_date_type ############################################################
#################################################################################################################
build_factor_date_type <- function(args) {
    factor_date_type <- "yearmonth"
    if (! is.null(args[["factor_date_type"]])) {
        if (is.character(args[["factor_date_type"]]) & length(args[["factor_date_type"]]) == 1) {
            factor_date_type <- args[["factor_date_type"]]
        }
        else {
            stop("factor_date_type should be a character.")
        }
    }
    return(factor_date_type)
}
