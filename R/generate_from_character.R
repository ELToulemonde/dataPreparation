#' Recode character
#'
#'Recode character into 3 new columns: \cr
#' \itemize{
#' \item was the value not NA, "NA", "",
#' \item how often this value occurs,
#' \item the order of the value (ex: M/F => 2/1 because F comes before M in alphabet).
#' }
#' @param data_set Matrix, data.frame or data.table
#' @param cols List of character column(s) name(s) of data_set to transform. To transform all
#' characters, set it to "auto". (character, default to "auto")
#' @param verbose Should the function log (logical, default to TRUE)
#' @param drop Should \code{cols} be dropped after generation (logical, default to FALSE)
#' @param ... Other arguments such as \code{name_separator} to separate words in new columns names
#' (character, default to ".")
#' @return \code{data_set} with new columns. \code{data_set} is edited by \strong{reference}.
#' @examples
#' # Load data set
#' data(tiny_messy_adult)
#' tiny_messy_adult <- un_factor(tiny_messy_adult, verbose = FALSE) # un factor ugly factors
#'
#'# transform column "mail"
#' tiny_messy_adult <- generate_from_character(tiny_messy_adult, cols = "mail")
#' head(tiny_messy_adult)
#'
#'# To transform all characters columns:
#' tiny_messy_adult <- generate_from_character(tiny_messy_adult, cols = "auto")
#' @import data.table
#' @export
generate_from_character <- function(data_set, cols = "auto", verbose = TRUE, drop = FALSE, ...) {
    # Working environment
    function_name <- "generate_from_character"

    # Sanity check
    data_set <- check_and_return_datatable(data_set)
    cols <- real_cols(data_set = data_set, cols = cols, function_name = function_name, types = "character")
    is.verbose(verbose)

    # Initialization
    start_time <- proc.time()
    args <- list(...)
    name_separator <- build_name_separator(args)

    # Computation
    for (col in cols) {
        # has value
        new_col <- paste0(col, name_separator, "notnull")
        new_col <- make_new_col_name(new_col, names(data_set))
        set(data_set, NULL, new_col, ! data_set[[col]] %in% c(NA, "NA", ""))

        # recode with nb of occurrence of value
        new_col <- paste0(col, name_separator, "num")
        new_col <- make_new_col_name(new_col, names(data_set))
        data_set[, c(new_col) := .N, by = col]

        # recode with order of value
        new_col <- paste0(col, name_separator, "order")
        new_col <- make_new_col_name(new_col, names(data_set))
        col_order <- names(data_set) # store order to reset it
        col_tmp <- paste0(col, '_tmp')
        data_set[[col_tmp]] <- data_set[[col]]
        set(data_set, which(is.na(data_set[[col]])), col_tmp, "NA")
        unique_values <- sort(unique(data_set[[col_tmp]]))
        unique_values <- data.table(col = unique_values, new_col = seq_len(length(unique_values)))
        data_set <- merge(data_set, unique_values, by.x = col, by.y = "col", all.x = TRUE, sort=FALSE)
        set(data_set, NULL, col_tmp, NULL)
        setnames(data_set, "new_col", new_col) # set correct name
        setcolorder(data_set, c(col_order, new_col)) # reset col order

        # if asked drop col
        if (isTRUE(drop)) {
            set(data_set, NULL, col, NULL)
        }
    }
    if (verbose) {
        printl(function_name, ": it took me: ", round((proc.time() - start_time)[[3]], 2),
        "s to transform ", length(cols), " character columns into, ", 3 * length(cols), " new columns.")
    }
    # Wrap-up
    return(data_set)
}
