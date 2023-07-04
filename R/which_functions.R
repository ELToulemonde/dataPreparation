#######################################################################################
############################## which are constant ####################################
#######################################################################################
#' Identify constant columns
#'
#'Find all the columns that are constant.
#' @param data_set Matrix, data.frame or data.table
#' @param keep_cols List of columns not to drop (list of character, default to NULL)
#' @param verbose Should the algorithm talk (logical, default to TRUE)
#' @return List of column's indexes that are constant in the data_set set.
#' @details
#' Algorithm is performing exponential search: it check constancy on row 1 to 10,
#' if it's not constant it stops, if it's constant then on 11 to 100 ... \cr
#' If you have a lot of columns than aren't constant, this function is way faster than a simple
#' \code{length(unique())}! The larger the data_set set is, the more interesting it is to use this function.
#' @examples
#' # Let's load our data_set
#' data(tiny_messy_adult)
#'
#' # Let's try our function
#' which_are_constant(tiny_messy_adult)
#' # Indeed it return constant the name of the constant column.
#' @import data.table
#' @export
which_are_constant <- function(data_set, keep_cols = NULL, verbose = TRUE) {
    # Working environment
    function_name <- "which_are_constant"

    # Sanity check
    data_set <- check_and_return_datatable(data_set)
    is.verbose(verbose)
    keep_cols <- real_cols(data_set = data_set, cols = keep_cols, function_name = function_name)

    # Initialization
    constant_cols <- NULL
    if (verbose) {
        start_time <- proc.time()
        pb <- init_progress_bar(function_name, names(data_set))
    }
    cols <- names(data_set)[! names(data_set) %in% keep_cols]

    # Computation
    if (nrow(data_set) > 1) { # We check for constant only if there are at least two lines
        for (col in cols) {
            if (fast_is_there_less_than(data_set[[col]], 1)) {
                if (verbose) {
                  printl(function_name, ": ", col, " is constant.")
                }
                constant_cols <- c(constant_cols, col)
            }
            if (verbose) {
              add_a_tick_to_progress_bar(pb)
            }
        }
    }

    # Wrap-up
    constant_cols_indexes <- which(names(data_set) %in% constant_cols)
    if (verbose) {
        printl(function_name, ": it took me ", round((proc.time() - start_time)[[3]], 2),
        "s to identify ", length(constant_cols), " constant column(s)")
    }

    return(constant_cols_indexes)
}


#######################################################################################
############################## which are in double ###################################
#######################################################################################
#' Identify double columns
#'
#'Find all the columns that are in double.
#' @param data_set Matrix, data.frame or data.table
#' @param keep_cols List of columns not to drop (list of character, default to NULL)
#' @param verbose Should the algorithm talk (logical, default to TRUE)
#' @return
#' A list of index of columns that have an exact duplicate in the data_set set.
#' Ex: if column i and column j (with j > i) are equal it will return j.
#' @details
#' This function is performing search by looking to every couple of columns. First it compares the
#' first 10 lines of both columns. If they are not equal then the columns aren't identical, else
#' it compares lines 11 to 100; then 101 to 1000... So this function is fast with data_set set
#' with a large number of lines and a lot of columns that aren't equals. \cr
#' If \code{verbose} is TRUE, the column logged will be the one returned.
#' @examples
#' # First let's build a matrix with 3 columns and a lot of lines, with 1's everywhere
#' M <- matrix(1, nrow = 1e6, ncol = 3)
#'
#' # Now let's check which columns are equals
#' which_are_in_double(M)
#' # It return 2 and 3: you should only keep column 1.
#'
#' # Let's change the column 2, line 1 to 0. And check again
#' M[1, 2] <- 0
#' which_are_in_double(M)
#' # It only returns 3
#'
#' # What about NA? NA vs not NA => not equal
#' M[1, 2] <- NA
#' which_are_in_double(M)
#' # It only returns 3
#'
#' # What about NA?  Na vs NA => yep it's the same
#' M[1, 1] <- NA
#' which_are_in_double(M)
#' # It only returns 2
#' @export
which_are_in_double <- function(data_set, keep_cols = NULL, verbose = TRUE) {
    # Working environment
    function_name <- "which_are_in_double"

    # Sanity check
    data_set <- check_and_return_datatable(data_set)
    is.verbose(verbose)
    keep_cols <- real_cols(data_set = data_set, cols = keep_cols, function_name = function_name)

    # Initialization

    # Computation
    double_cols <- bi_col_test(data_set = data_set, keep_cols = keep_cols, verbose = verbose,
    test_function = "fast_is_equal", function_name = function_name,
    test_log = " is exactly equal to ")

    # Wrap-up
    return(double_cols)
}


#######################################################################################
############################## Fast find bijections ##################################
#######################################################################################
#' Identify bijections
#'
#'Find all the columns that are bijections of another column.
#' @param data_set Matrix, data.frame or data.table
#' @param keep_cols List of columns not to drop (list of character, default to NULL)
#' @param verbose Should the algorithm talk (logical, default to TRUE)
#' @return A list of index of columns that have an exact bijection in the data_set set.
#' @details
#' Bijection, meaning that there is another column containing the exact same information (but maybe
#'  coded differently) for example col1: Men/Women, col2 M/W. \cr
#' This function is performing search by looking to every couple of columns.
#' It computes numbers of unique elements in each column, and number of unique tuples of values. \cr
#' Computation is made by exponential search, so that the function is faster. \cr
#' If \code{verbose} is TRUE, the column logged will be the one returned. \cr
#' Ex: if column i and column j (with j > i) are bijections it will return j, expect if j is a
#' character then it return i.
#' @examples
#' # First let's get a data set
#' data("adult")
#'
#' # Now let's check which columns are equals
#' which_are_in_double(adult)
#' # It doesn't give any result.
#'
#' # Let's look of bijections
#' which_are_bijection(adult)
#' # Return education_num index because education_num and education which
#' # contain the same info
#' @export
which_are_bijection <- function(data_set, keep_cols = NULL, verbose = TRUE) {
    # Working environment
    function_name <- "which_are_bijection"

    # Sanity check
    data_set <- check_and_return_datatable(data_set = data_set)
    is.verbose(verbose)
    keep_cols <- real_cols(data_set = data_set, cols = keep_cols, function_name = function_name)

    # Initialization

    # Computation # to-do clean it
    bijection_cols <- bi_col_test(data_set = data_set, keep_cols = keep_cols, verbose = verbose,
    test_function = "fast_is_bijection", function_name = function_name,
    test_log = " is a bijection of ")

    # Wrapp up
    return(bijection_cols)
}


#######################################################################################
############################## Fast are included #####################################
#######################################################################################
#' Identify columns that are included in others
#'
#'Find all the columns that don't contain more information than another column. For example if
#' you have a column with an amount and another with the same amount but rounded, the second
#' column is included in the first.
#' @param data_set Matrix, data.frame or data.table
#' @param keep_cols List of columns not to drop (list of character, default to NULL)
#' @param verbose Should the algorithm talk (logical, default to TRUE)
#' @details
#' This function is performing exponential search and is looking to every couple of columns. \cr
#' Be very careful while using this function: \cr
#' - if there is an id column, it will say everything is included in the id column; \cr
#' - the order of columns will influence the result.\cr
#' \cr
#'For example if
#' you have a column with an amount and another with the same amount but rounded, the second
#' column is included in the first.\cr
#' \cr
#' And last but not least, with some machine learning algorithm it's not always smart to drop
#' columns even if they don't give more info: the extreme example is the id example.
#' @return A list of index of columns that have an exact duplicate in the \code{data_set}.
#' @examples
#' # Load toy data set
#' require(data.table)
#' data(tiny_messy_adult)
#'
#'# Check for included columns
#' which_are_included(tiny_messy_adult)
#'
#'# Return columns that are also constant, double and bijection
#' # Let's add a truly just included column
#' tiny_messy_adult$are50OrMore <- tiny_messy_adult$age > 50
#' which_are_included(tiny_messy_adult[, .(age, are50OrMore)])
#'
#'# As one can, see this column that doesn't have additional info than age is spotted.
#'
#'# But you should be careful, if there is a column id, every column will be dropped:
#' tiny_messy_adult$id = seq_len(nrow(tiny_messy_adult)) # build id
#' which_are_included(tiny_messy_adult)
#' @export
which_are_included <- function(data_set, keep_cols = NULL, verbose = TRUE) {
    # Working environment
    function_name <- "which_are_included"

    # Sanity check
    data_set <- check_and_return_datatable(data_set = data_set)
    is.verbose(verbose)
    keep_cols <- real_cols(data_set = data_set, cols = keep_cols, function_name = function_name)

    # Initialization
    included_cols <- NULL
    keep_cols_index <- which(names(data_set) %in% keep_cols)
    if (verbose) {
        pb <- init_progress_bar(function_name, names(data_set))
    }
    n_unique_vals_by_columns <- sapply(data_set, uniqueN)
    number_of_columns <- length(n_unique_vals_by_columns)
    cols_ordered_by_nunique_values <- order(n_unique_vals_by_columns)
    candidates_included_cols <- cols_ordered_by_nunique_values[- number_of_columns]

    # Computation
    for (candidate_included_col in candidates_included_cols) {
        candidate_included_col_index <- which(cols_ordered_by_nunique_values == candidate_included_col)
        candidates_containing_cols <- cols_ordered_by_nunique_values[(candidate_included_col_index + 1) :
                                                                       number_of_columns]
        for (candidate_containing_col in candidates_containing_cols) {
            n_couples <- uniqueN(data_set[, c(candidate_included_col, candidate_containing_col), with = FALSE])
            if (n_couples == n_unique_vals_by_columns[candidate_containing_col] &
                  ! candidate_included_col %in% keep_cols_index) {
                included_cols <- c(included_cols, candidate_included_col)
                if (verbose) {
                    printl(function_name, ": ", names(data_set)[candidate_included_col],
                           " is included in column ", names(data_set)[candidate_containing_col], ".")
                }
                break # Break loop since col_i will be dropped.
            }
            else if (n_couples == n_unique_vals_by_columns[candidate_included_col] &
                     ! candidate_containing_col %in% keep_cols_index) {
                # This is when candidate_included_col and candidate_containing_col are bijections
                # and candidate_included_col is in keep_cols
                included_cols <- c(included_cols, candidate_containing_col)
                if (verbose) {
                    printl(function_name, ": ", names(data_set)[candidate_containing_col],
                           " is included in column ", names(data_set)[candidate_included_col], ".")
                }
            }
        }
        if (verbose) {
            add_a_tick_to_progress_bar(pb)
        }
    }
    # Wrapp up
    if (! is.null(included_cols)) {
        included_cols <- sort(unique(included_cols))
    }
    return(included_cols)
}

###################################################################################################
############################## Bi test function  #################################################
###################################################################################################
# Internal function to compute test on couple of columns
# @param data_set Matrix, data.frame or data.table
# @param keep_cols List of columns not to drop (list of character, default to NULL)
# @param verbose Should the algorithm talk (logical, default to TRUE)
# @param test_function function to perform test
# @param function_name function from which this function is called (for verbose)
# @param test_log what is this test result (for verbose)
bi_col_test <- function(data_set, keep_cols = NULL, verbose = TRUE, test_function,
function_name = "bi_col_test", test_log = " is a match of ") {

    # Sanity check
    data_set <- check_and_return_datatable(data_set = data_set)
    is.verbose(verbose)
    keep_cols <- real_cols(data_set = data_set, cols = keep_cols, function_name = function_name)

    # Initialization
    drop_list <- NULL
    if (verbose) {
        start_time <- proc.time()
        pb <- init_progress_bar(function_name, names(data_set))
    }

    # Computation
    for (compared_col in names(data_set)[- ncol(data_set)]) {
        cols_to_be_compared <- names(data_set)[(which(compared_col == names(data_set)) + 1) : ncol(data_set)]
        # Drop cols that are already found
        cols_to_be_compared <- cols_to_be_compared[! cols_to_be_compared %in% drop_list]
        for (comparaison_col in cols_to_be_compared) {
            # If one of the couple is dropable
            if (get(test_function)(data_set[[compared_col]], data_set[[comparaison_col]])) {
                # If test is positive check which one is to drop
                if (! comparaison_col %in% keep_cols & ! is.character(data_set[[comparaison_col]])) {
                    drop_list <- c(drop_list, comparaison_col)
                }
                else { # Drop i
                    drop_list <- c(drop_list, compared_col)
                    if (verbose) {
                        printl(function_name, ": ", compared_col, test_log, comparaison_col,
                               ". I put it in drop list.")
                    }
                    break # Break loop since i will be dropped.
                }
            }
        }
        if (verbose) {
          add_a_tick_to_progress_bar(pb)
        }
    }
    # Wrap-up
    if (verbose) {
        printl(function_name, ": it took me ", round((proc.time() - start_time)[[3]], 2),
        "s to identify ", length(drop_list), " column(s) to drop.")
    }
    return(sort(unique(which(names(data_set) %in% drop_list))))
}
