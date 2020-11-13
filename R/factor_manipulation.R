#' Unfactor factor with too many values
#'
#'To unfactorize all columns that have more than a given amount of various values. This
#'  function will be usefull after using some reading functions that put every string as factor.
#' @param data_set Matrix, data.frame or data.table
#' @param cols List of column(s) name(s) of data_set to look into. To check all all columns, set it
#'  to "auto". (characters, default to "auto")
#' @param n_unfactor Number of max element in a factor (numeric, default to 53)
#' @param verbose Should the algorithm talk? (logical, default to TRUE)
#' @details
#' If a factor has (strictly) more than \code{n_unfactor} values it is unfactored. \cr
#' It is recommended to use \code{\link{find_and_transform_numerics}} and
#'  \code{\link{find_and_transform_dates}} after this function.\cr
#' If \code{n_unfactor} is set to -1, nothing will be performed. \cr
#' If there are a lot of column that have been transformed, you might want to look at the
#' documentation of your data reader in order to stop transforming everything into a factor.
#' @return Same data_set (as a data.table) with less factor columns.
#' @examples
#' # Let's build a data_set
#' data_set <- data.frame(true_factor = factor(rep(c(1,2), 13)),
#'                       false_factor = factor(LETTERS))
#'
#' # Let's un factorize all factor that have more than 5 different values
#' data_set <- un_factor(data_set, n_unfactor = 5)
#' sapply(data_set, class)
#' # Let's un factorize all factor that have more than 5 different values
#' data_set <- un_factor(data_set, n_unfactor = 0)
#' sapply(data_set, class)
#'
#'@import data.table
#' @export
un_factor <- function(data_set, cols = "auto", n_unfactor = 53, verbose = TRUE) {
    # Working environement
    function_name <- "un_factor"

    # Sanity check
    if (! is.numeric(n_unfactor)) {
        stop(paste0(function_name, ": n_unfactor should be a numeric, you provided a ", class(n_unfactor)))
    }
    else {
        # To be safe
        n_unfactor <- round(n_unfactor)
    }
    if (n_unfactor == - 1) {
        return(data_set)
    }
    is.verbose(verbose)
    data_set <- check_and_return_datatable(data_set = data_set)

    # Initialization
    cols <- real_cols(data_set, cols = cols, function_name = function_name, types = "factor")
    if (verbose) {
        pb <- init_progress_bar(function_name, cols)
        printl(function_name, ": I will identify variable that are factor but shouldn't be.")
        count <- 0
        start_time <- proc.time()
    }

    # Computation
    for (col in cols) {
        if (length(levels(data_set[[col]])) > n_unfactor) {
            if (verbose) {
                printl(function_name, ": I unfactor ", col, ".")
                count <- count + 1
            }
            set(data_set, NULL, col, levels(data_set[[col]])[data_set[[col]]])
        }

        if (verbose) {
            add_a_tick_to_progress_bar(pb)
        }
    }
    if (verbose) {
        printl(function_name, ": It took me ", round((proc.time() - start_time)[[3]], 2),
        "s to unfactor ", count, " column(s).")
    }

    # Wrap-up
    return(data_set)
}