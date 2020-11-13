#' Compute bins
#'
#'Compute bins for discretization of numeric variable (either equal_width or equal_fred).
#' @param data_set Matrix, data.frame or data.table
#' @param cols List of numeric column(s) name(s) of data_set to transform. To transform all
#' characters, set it to "auto".  (character, default to "auto")
#' @param n_bins Number of group to compute (numeric, default to 10)
#' @param type Type of discretization ("equal_width" or "equal_freq")
#' @param verbose Should the algorithm talk? (Logical, default to TRUE)
#' @return A list where each element name is a column name of data set and each element contains
#' bins to discretize this column.
#' @details
#' Using equal freq first bin will start at -Inf and last bin will end at +Inf.
#' @examples
#' # Load data
#' data(messy_adult)
#' head(messy_adult)
#'
#'# Compute bins
#' bins <- build_bins(messy_adult, cols = "auto", n_bins = 5, type = "equal_freq")
#' print(bins)
#' @export
build_bins <- function(data_set, cols = "auto", n_bins = 10, type = "equal_width", verbose = TRUE) {
    # Working environement
    function_name <- "fast_discretization"

    # Sanity check
    data_set <- check_and_return_datatable(data_set)
    is.verbose(verbose)
    cols <- real_cols(data_set = data_set, cols = cols, function_name = function_name, types = "numeric")
    if (! type %in% c("equal_width", "equal_freq")) {
        stop(paste0(function_name, ": type should either be 'equal_width' or 'equal_freq'."))
    }
    n_bins <- round(n_bins) # Just to be safe

    # Initialization
    if (verbose) {
        pb <- init_progress_bar(function_name, cols)
        printl(function_name, ": I will build splits for ", length(cols), " numeric columns using, ",
        type, " method.")
        start_time <- proc.time()
    }
    n_transformed <- length(cols)
    splits_list <- list()
    # Computation
    for (col in cols) {
        # Compute splits
        if (type == "equal_width") {
            splits <- equal_width_splits(data_set[[col]], n_bins = n_bins, col = col, verbose = verbose)
        }
        if (type == "equal_freq") {
            splits <- equal_freq_splits(data_set[[col]], n_bins = n_bins, col = col, verbose = verbose)
        }
        # Constant columns
        if (length(splits) == 1) {
            printl(function_name, ": column ", col, " seems to be constant, I do nothing.")
            n_transformed <- n_transformed - 1
            next
        }
        else {
            splits_list[[col]] <- splits
        }
        # Update progress bar
        if (verbose) {
            add_a_tick_to_progress_bar(pb)
        }
    }
    if (verbose) {
        printl(function_name, ": it took me: ", round((proc.time() - start_time)[[3]], 2),
        "s to build splits for ", n_transformed, " numeric columns.")
    }
    # Wrap-up
    return(splits_list)
}

#' Discretization
#'
#'Discretization of numeric variable (either equal_width or equal_fred).
#' @param data_set Matrix, data.frame or data.table
#' @param bins Result of funcion \code{\link{build_bins}}, (list, default to NULL). \cr
#' To perform the same discretization on train and test, it is recommended to compute \code{\link{build_bins}}
#' before. If it is kept to NULL, build_bins will be called.\cr
#' \code{bins} could also be carefully hand written.
#' @param verbose Should the algorithm talk? (Logical, default to TRUE)
#' @return Same dataset discretized by \strong{reference}. \cr
#' If you don't want to edit by reference please provide set \code{data_set = copy(data_set)}.
#' @details NAs will be putted in an NA category.
#' @examples
#' # Load data
#' data(messy_adult)
#' head(messy_adult)
#'
#'# Compute bins
#' bins <- build_bins(messy_adult, cols = "auto", n_bins = 5, type = "equal_freq")
#'
#'# Discretize
#' messy_adult <- fast_discretization(messy_adult, bins = bins)
#'
#'# Control
#' head(messy_adult)
#'
#' # Example with hand written bins
#' data("adult")
#' adult <-  fast_discretization(adult, bins = list(age = c(0, 40, +Inf)))
#' print(table(adult$age))
#' @export
fast_discretization <- function(data_set, bins = NULL, verbose = TRUE) {
    # Working environement
    function_name <- "fast_discretization"

    # Sanity check
    data_set <- check_and_return_datatable(data_set)
    is.verbose(verbose)
    if (is.null(bins)) {
        bins <- build_bins(data_set, verbose = verbose)
    }
    cols <- names(bins)
    cols <- real_cols(data_set, cols = cols, function_name = function_name, types = "numeric")

    # Initialization
    if (verbose) {
        pb <- init_progress_bar(function_name, cols)
        printl(function_name, ": I will discretize ", length(cols), " numeric columns using, bins.")
        start_time <- proc.time()
    }
    # Computation
    for (col in cols) {
        # Compute splits
        splits <- bins[[col]]
        split_names <- build_splits_names(splits)

        # Update column
        set(data_set, NULL, col, as.factor(split_names[sapply(data_set[[col]], function(x) find_splits(x, splits))]))
        # Update progress bar
        if (verbose) {
            add_a_tick_to_progress_bar(pb)
        }
    }
    if (verbose) {
        printl(function_name, ": it took me: ", round((proc.time() - start_time)[[3]], 2),
        "s to transform ", length(cols), " numeric columns into, binarised columns.")
    }
    # Wrap-up
    return(data_set)
}


find_splits <- function(x, splits) {
    if (is.na(x)) {
        return(NA)
    }
    res <- which(splits[- length(splits)] <= x & x < splits[- 1])
    if (length(res) == 0) {
        res <- length(splits) - 1
    }
    return(res)
}

# equal_freq_splits
# ------------------
# @param data_set a numeric verctor
# @param n_bins interger number of wanted bins
# @return a vector of limits (lim1, ..., lim(n_bins +1))
equal_width_splits <- function(data_set, n_bins, col = "data_set", verbose = TRUE) {
    # Working environement
    function_name <- "equal_width_splits"

    # Sanity check
    is.possible_to_split(function_name = function_name, data_set = data_set, n_bins = n_bins)
    is.verbose(verbose)

    # Computation
    min_val <- min(data_set, na.rm = TRUE)
    max_val <- max(data_set, na.rm = TRUE)
    splits <- unique(seq(from = min_val, to = max_val, length.out = n_bins + 1)) # +1 to have exactly n_bins intervals

    if (length(splits) < n_bins & verbose) {
        printl(function_name, ": ", col, " can't provide ", n_bins,
        " equal width bins; instead you will have ", length(splits) - 1, " bins.")
    }
    # Wrap-up
    return(splits)
}

# equal_freq_splits
# ------------------
# @param data_set a numeric verctor
# @param n_bins interger number of wanted bins
# @return a vector of limits (-Inf, lim1, ..., +Inf)
equal_freq_splits <- function(data_set, n_bins, col = "data_set", verbose = TRUE) {
    # Working environement
    function_name <- "equal_freq_splits"

    # Sanity check
    is.possible_to_split(function_name = function_name, data_set = data_set, n_bins = n_bins)
    is.verbose(verbose)

    # Computation
    # Order
    data_set <- data_set[! is.na(data_set)]
    order_set <- order(data_set, decreasing = FALSE)

    # Number of value in each
    wanted_index <- round(seq(from = 1, to = length(data_set), length.out = n_bins + 1))

    splits <- data_set[order_set[wanted_index]]
    splits[1] <- - Inf
    splits[n_bins + 1] <- + Inf

    # To be safe
    splits <- unique(splits)
    if (length(splits) < n_bins & verbose) {
        printl(function_name, ": ", col, " can't provide ", n_bins,
        " equal freq bins; instead you will have ", length(splits) - 1, " bins.")
    }
    # Wrap-up
    return(splits)
}

# build_splits_names
# -------------------
build_splits_names <- function(splits) {
    split_names <- paste0("[", splits[- length(splits)], ", ", splits[- 1], "[")

    split_names[length(split_names)] <- gsub("\\[$", "]", split_names[length(split_names)])
    split_names[1] <- gsub("\\[-Inf", "]-Inf", split_names[1])
    split_names[length(split_names)] <- gsub("+Inf\\]$", "+Inf[", split_names[length(split_names)])

    return(split_names)
}

# is.possible_to_split
# -------------------
is.possible_to_split <- function(data_set, n_bins, function_name = "is.possible_to_split") {
    if (! is.numeric(data_set) || ! is.numeric(n_bins)) {
        stop(paste0(function_name, ": data_set should be a vector of numerics and n_bins a numeric."))
    }
}