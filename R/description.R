#' Describe data set
#'
#'Generate extensive description of a data set.
#' @param data_set Matrix, data.frame or data.table
#' @param level Level of description (0: generic, 1: column by column)
#' (numeric, default to 1)
#' @param path_to_write Path where the report should be written (character, default to NULL)
#' @param verbose Should the algorithm talk? (Logical, default to TRUE)
#' @examples
#' # Load exemple set
#' data(tiny_messy_adult)
#'
#'# Describe it
#' description(tiny_messy_adult)
#' @import data.table
#' @export
description <- function(data_set, level = 1, path_to_write = NULL, verbose = TRUE) {
    # Working environment
    function_name <- "description"

    # Sanity check
    store_class <- class(data_set)
    data_set <- check_and_return_datatable(data_set)
    is.verbose(verbose)
    # If path to write is set, check if it's writable
    if (! level %in% c(0, 1)) {
        stop(paste0(function_name, ": level should be either 0 or 1."))
    }

    # Initialization
    if (! is.null(path_to_write)) {
        log_file <- path_to_write
        sink(log_file)
    }
    # Computation
    # Level 0: general description
    cat("##################################\n# Level 0: General description ##\n##################################\n")
    printl("data_set is a ", paste0(store_class, collapse = "-"))
    printl("data_set contains ", nrow(data_set), " rows and ", ncol(data_set), " cols.")
    printl("Columns are of the following classes:")
    print(table(sapply(data_set, function(x)paste0(class(x), collapse = "-"))))

    # Level 1: Uni variate description
    if (level >= 1) {
        cat("#####################################\n# Level 1: uni-variate description ##
#####################################\n")
        card <- data_set[, lapply(.SD, uniqueN)]
        for (col in names(data_set)) {
            # Unique or distinct values
            if (card[[col]] == 1) {
                printl(col, " only has 1 value.")
            }
            if (card[[col]] == nrow(data_set)) {
                printl(col, " has all unique values.")
            }
            # Numerical and date cols
            if (is.numeric(data_set[[col]]) || is.date(data_set[[col]])) {
                if (is.numeric(data_set[[col]])) {
                    printl("Summary for numeric variable ", col)
                }
                if (is.date(data_set[[col]])) {
                    printl("Summary for date variable ", col)
                }
                print(summary(data_set[[col]]))
            }
            # Factor cols
            if (is.factor(data_set[[col]]) || is.logical(data_set[[col]])) {
                if (is.factor(data_set[[col]])) {
                    printl("Table of occurrence for factor variable ", col)
                }
                if (is.logical(data_set[[col]])) {
                    printl("Table of occurrence for logical variable ", col)
                }
                print(table(data_set[[col]]))
            }
            # character
            if (is.character(data_set[[col]])) {
                printl("character variable ", col, " takes ", uniqueN(data_set[[col]]), " different values.")
            }
        }
    }

    # Wrap-up
    if (! is.null(path_to_write)) {
        sink()
    }
}
