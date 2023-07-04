#' Standard deviation outlier filtering
#'
#' Remove outliers based on standard deviation thresholds. \cr
#' Only values within \code{mean - sd * n_sigmas} and \code{mean + sd * n_sigmas} are kept.
#' @param data_set Matrix, data.frame or data.table
#' @param cols List of numeric column(s) name(s) of data_set to transform. To transform all
#' numeric columns, set it to "auto".  (character, default to "auto")
#' @param n_sigmas number of times standard deviation is accepted (integer, default to 3)
#' @param verbose Should the algorithm talk? (logical, default to TRUE)
#' @details Filtering is made column by column, meaning that extreme values from first element
#' of \code{cols} are removed, then extreme values from second element of \code{cols} are removed,
#' ... \cr
#' So if filtering is performed on too many column, there ia high risk that a lot of rows will be dropped.
#' @return Same dataset with less rows, edited by \strong{reference}. \cr
#' If you don't want to edit by reference please provide set \code{data_set = copy(data_set)}.
#' @examples
#' # Given
#' library(data.table)
#' col_vals <- runif(1000)
#' col_mean <- mean(col_vals)
#' col_sd <- sd(col_vals)
#' extreme_val <- col_mean + 6 * col_sd
#' data_set <- data.table(num_col = c(col_vals, extreme_val))
#'
#'# When
#' data_set <- remove_sd_outlier(data_set, cols = "auto", n_sigmas = 3, verbose = TRUE)
#'
#'# Then extreme value is no longer in set
#' extreme_val %in% data_set[["num_col"]] # Is false
#' @export
remove_sd_outlier <- function(data_set, cols = "auto", n_sigmas = 3, verbose = TRUE) {
    # Environment
    function_name <- "remove_sd_outlier"

    # Sanity check
    data_set <- check_and_return_datatable(data_set = data_set)
    cols <- real_cols(data_set = data_set, cols = cols, function_name = function_name, types = "numeric")

    # Initialization
    if (verbose) {
        printl(function_name, ": I start to filter categorical rare events")
        pb <- init_progress_bar(function_name, names(data_set))
        start_time <- proc.time()
    }
    initial_nrow <- nrow(data_set)

    # Computation
    for (col in cols) {
        tmp_nrow <- nrow(data_set)
        col_mean <- data_set[, mean(get(col))]
        col_sd <- data_set[, sd(get(col))]
        data_set <- data_set[(get(col) <= col_mean + n_sigmas * col_sd) &
        (get(col) >= col_mean - n_sigmas * col_sd), ]
        if (verbose) {
            printl(function_name, ": dropped ", tmp_nrow - nrow(data_set), " row(s) that are rare event on ", col, ".")
            add_a_tick_to_progress_bar(pb)
        }
    }

    if (verbose) {
        printl(function_name, ": ", initial_nrow - nrow(data_set), " have been dropped. It took ",
        round((proc.time() - start_time)[[3]], 2), " seconds. ")
    }


    # Wrap-up
    return(data_set)
}

#' Filter rare categories
#'
#' Filter rows that have a rare occurrences
#' @param data_set Matrix, data.frame or data.table
#' @param cols List of column(s) name(s) of data_set to transform. To transform all
#' columns, set it to "auto".  (character, default to "auto")
#' @param threshold share of occurrences under which row should be removed (numeric, default to 0.01)
#' @param verbose Should the algorithm talk? (logical, default to TRUE)
#' @details Filtering is made column by column, meaning that extreme values from first element
#' of \code{cols} are removed, then extreme values from second element of \code{cols} are removed,
#' ... \cr
#' So if filtering is performed on too many column, there ia high risk that a lot of rows will be dropped.
#' @return Same dataset with less rows, edited by \strong{reference}. \cr
#' If you don't want to edit by reference please provide set \code{data_set = copy(data_set)}.
#' @examples
#' # Given a set with rare "C"
#' library(data.table)
#' data_set <- data.table(cat_col = c(sample(c("A", "B"), 1000, replace=TRUE), "C"))
#'
#'# When calling function
#' data_set <- remove_rare_categorical(data_set, cols = "cat_col",
#'                                    threshold = 0.01, verbose = TRUE)
#'
#' # Then there are no "C"
#' unique(data_set[["cat_col"]])
#' @import data.table
#' @export
remove_rare_categorical <- function(data_set, cols = "auto", threshold = 0.01, verbose = TRUE) {
    # Environment
    function_name <- "remove_rare_categorical"

    # Sanity check
    data_set <- check_and_return_datatable(data_set = data_set)
    cols <- real_cols(data_set = data_set, cols = cols, function_name = function_name)

    # Initialization
    if (verbose) {
        printl(function_name, ": I start to filter categorical rare events")
        pb <- init_progress_bar(function_name, names(data_set))
        start_time <- proc.time()
    }
    initial_nrow <- nrow(data_set)

    # Computation
    for (col in cols) {
        col_val_occ <- data_set[, .N, by = col]
        acceptable_cat <- col_val_occ[get("N") >= initial_nrow * threshold, get(col)]

        tmp_nrow <- nrow(data_set)
        data_set <- data_set[get(col) %in% acceptable_cat]

        if (verbose) {
            printl(function_name, ": dropped ", tmp_nrow - nrow(data_set), " row(s) that are rare event on ", col, ".")
            add_a_tick_to_progress_bar(pb)
        }
    }

    if (verbose) {
        printl(function_name, ": ", initial_nrow - nrow(data_set), " have been dropped. It took ",
        round((proc.time() - start_time)[[3]], 2), " seconds. ")
    }

    # Wrap-up
    return(data_set)
}

#' Percentile outlier filtering
#'
#' Remove outliers based on percentiles. \cr
#' Only values within \code{n}th and \code{100 - n}th percentiles are kept.
#' @param data_set Matrix, data.frame or data.table
#' @param cols List of numeric column(s) name(s) of data_set to transform. To transform all
#' numeric columns, set it to "auto".  (character, default to "auto")
#' @param percentile percentiles to filter (numeric, default to 1)
#' @param verbose Should the algorithm talk? (logical, default to TRUE)
#' @details Filtering is made column by column, meaning that extreme values from first element
#' of \code{cols} are removed, then extreme values from second element of \code{cols} are removed,
#' ... \cr
#' So if filtering is performed on too many column, there ia high risk that a lot of rows will be dropped.
#' @return Same dataset with less rows, edited by \strong{reference}. \cr
#' If you don't want to edit by reference please provide set \code{data_set = copy(data_set)}.
#' @examples
#' # Given
#' library(data.table)
#' data_set <- data.table(num_col = seq_len(100))
#'
#'# When
#' data_set <- remove_percentile_outlier(data_set, cols = "auto", percentile = 1, verbose = TRUE)
#'
#'# Then extreme value is no longer in set
#' 1 %in% data_set[["num_col"]] # Is false
#' 2 %in% data_set[["num_col"]] # Is true
#' @importFrom stats quantile
#' @export
remove_percentile_outlier <- function(data_set, cols = "auto", percentile = 1, verbose = TRUE) {
    # Environment
    function_name <- "remove_percentile_outlier"

    # Sanity check
    data_set <- check_and_return_datatable(data_set = data_set)
    cols <- real_cols(data_set = data_set, cols = cols, function_name = function_name, types = "numeric")

    # Initialization
    if (verbose) {
        printl(function_name, ": I start to filter categorical rare events")
        pb <- init_progress_bar(function_name, names(data_set))
        start_time <- proc.time()
    }
    initial_nrow <- nrow(data_set)

    # Computation
    for (col in cols) {
        tmp_nrow <- nrow(data_set)
        percentiles <- quantile(data_set[[col]],
        c(percentile / 100, (100 - percentile) / 100), na.rm = TRUE)
        data_set <- data_set[(get(col) >= percentiles[1]) & (get(col) <= percentiles[2]), ]
        if (verbose) {
            printl(function_name, ": dropped ", tmp_nrow - nrow(data_set), " row(s) that are rare event on ", col, ".")
            add_a_tick_to_progress_bar(pb)
        }
    }

    if (verbose) {
        printl(function_name, ": ", initial_nrow - nrow(data_set), " have been dropped. It took ",
        round((proc.time() - start_time)[[3]], 2), " seconds. ")
    }


    # Wrap-up
    return(data_set)
}
