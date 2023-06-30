###################################################################################################
############################## prepare_set  #######################################################
###################################################################################################
#' Preparation pipeline
#'
#'Full pipeline for preparing your data_set set.
#' @param data_set Matrix, data.frame or data.table
#' @param final_form "data.table" or "numerical_matrix" (default to data.table)
#' @param verbose Should the algorithm talk? (logical, default to TRUE)
#' @param ... Additional parameters to tune pipeline (see details)
#' @details
#' Additional arguments are available to tune pipeline:
#' \itemize{
#'   \item \code{key} Name of a column of data_set according to which data_set should be aggregated
#'      (character)
#'   \item \code{analysis_date} A date at which the data_set should be aggregated
#'      (differences between every date and analysis_date will be computed) (Date)
#'   \item \code{n_unfactor} Number of max value in a facotr, set it to -1 to disable
#'   \code{\link{un_factor}} function.  (numeric, default to 53)
#'   \item \code{digits} The number of digits after comma (optional, numeric, if set will perform
#'      \code{\link{fast_round}})
#'   \item \code{dateFormats} List of format of Dates in data_set (list of characters)
#'   \item \code{name_separator} character to separate parts of new column names (character, default to ".")
#'   \item \code{functions}  Aggregation functions for numeric columns, see \code{\link{aggregate_by_key}}
#'    (list of functions names (character))
#'   \item \code{factor_date_type} Aggregation level to factorize date (see
#'      \code{\link{generate_factor_from_date}}) (character, default to "yearmonth")
#'   \item \code{target_col} A target column to perform target encoding, see \code{\link{target_encode}}
#'   (character)
#'   \item \code{target_encoding_functions} Functions to perform target encoding, see
#'   \code{\link{build_target_encoding}},
#'   if \code{target_col} is not given will not do anything, (list, default to \code{"mean"})
#' }
#' @return A data.table or a numerical matrix (according to \code{final_form}). \cr
#' It will perform the following steps:
#' \itemize{
#'   \item Correct set: unfactor factor with many values, id dates and numeric that are hiden in character
#'   \item Transform set: compute differences between every date, transform dates into factors, generate
#'      features from character..., if \code{key} is provided, will perform aggregate according to this \code{key}
#'   \item Filter set: filter constant, in double or bijection variables. If `digits` is provided,
#'      will round numeric
#'   \item Handle NA: will perform \code{\link{fast_handle_na}})
#'   \item Shape set: will put the result in asked shape (\code{final_form}) with acceptable columns format.
#' }
#' @examples
#' # Load ugly set
#' \dontrun{
#' data(tiny_messy_adult)
#'
#'# Have a look to set
#' head(tiny_messy_adult)
#'
#'# Compute full pipeline
#' clean_adult <- prepare_set(tiny_messy_adult)
#'
#'# With a reference date
#' adult_agg <- prepare_set(tiny_messy_adult, analysis_date = as.Date("2017-01-01"))
#'
#'# Add aggregation by country
#' adult_agg <- prepare_set(tiny_messy_adult, analysis_date = as.Date("2017-01-01"), key = "country")
#'
#'# With some new aggregation functions
#' power <- function(x) {sum(x^2)}
#' adult_agg <- prepare_set(tiny_messy_adult, analysis_date = as.Date("2017-01-01"), key = "country",
#'                         functions = c("min", "max", "mean", "power"))
#' }
#' # "##NOT RUN:" mean that this example hasn't been run on CRAN since its long. But you can run it!
#' @import data.table
#' @export
prepare_set <- function(data_set, final_form = "data.table", verbose = TRUE, ...) {
    # Environement
    function_name <- "prepare_set"

    # Sanity check
    data_set <- check_and_return_datatable(data_set)
    is.verbose(verbose)

    # Initialization
    args <- list(...)

    # Computation
    ## 1. Correct data_set set
    if (verbose) {
        printl(function_name, ": step one: correcting mistakes.")
    }
    # 1.0 Filter useless vars
    data_set <- fast_filter_variables(data_set, keep_cols = args[["key"]], verbose = verbose)

    # 1.1 Unfactor
    n_unfactor <- 53
    if (! is.null(args[["n_unfactor"]])) {
        n_unfactor <- args[["n_unfactor"]]
    }
    data_set <- un_factor(data_set, n_unfactor = n_unfactor, verbose = verbose)

    # 1.2 Id variables
    data_set <- find_and_transform_numerics(data_set, verbose = verbose)
    data_set <- find_and_transform_dates(data_set, formats = args[["dateFormats"]], verbose = verbose)

    ## 2. Transform data_set set
    if (verbose) {
        printl(function_name, ": step two: transforming data_set.")
    }

    # 2.1 Generate from dates
    date_cols <- real_cols(data_set, cols = "auto", function_name, types = "date")
    if (! is.null(args[["key"]])) { # don't transform key
        date_cols <- date_cols[date_cols != args[["key"]]]
    }
    # 2.1.1 Compute differences between dates
    result <- generate_date_diffs(data_set, cols = date_cols, analysis_date = args[["analysis_date"]],
    name_separator = args[["name_separator"]], verbose = verbose)

    # 2.1.2 Build factor from dates month
    factor_date_type <- build_factor_date_type(args)
    result <- generate_factor_from_date(result, cols = date_cols, type = factor_date_type, drop = TRUE,
    name_separator = args[["name_separator"]], verbose = verbose)

    # 2.2 Generate features from character
    character_cols <- real_cols(data_set, cols = "auto", function_name, types = "character")
    if (! is.null(args[["key"]])) { # don't transform key
        character_cols <- character_cols[character_cols != args[["key"]]]
    }
    result <- generate_from_character(result, cols = character_cols,
                                      drop = TRUE, name_separator = args[["name_separator"]], verbose = verbose)

    # 2.3 Perform target encoding
    if (! is.null(args[["target_col"]])) {
        target_col <- args[["target_col"]]
        target_encoding_functions <- args[["target_encoding_functions"]]
        if (is.null(args[["target_encoding_functions"]])) {
            target_encoding_functions <- c("mean")
        }
        cols_to_encode <- names(result)[sapply(result, is.factor)]
        target_encoding <- build_target_encoding(data_set = result,
        cols_to_encode = cols_to_encode,
        target_col = target_col,
        functions = target_encoding_functions,
        verbose = verbose)
        result <- target_encode(data_set = result,
        target_encoding = target_encoding,
        verbose = verbose)
    }

    # 2.4 Aggregate by key
    if (! is.null(args[["key"]])) {
        key <- args[["key"]]
        result <- aggregate_by_key(result, key, verbose = verbose, functions = args[["functions"]], ...)
    }

    ## 3 Filter
    if (verbose) {
        printl(function_name, ": step three: filtering data_set.")
    }
    # 3.1 Get ride of useless variables
    result <- fast_filter_variables(data_set = result, keep_cols = args[["key"]], verbose = verbose,
    data_set_name = "result")

    # 3.2 Round
    if (! is.null(args[["digits"]])) {
        result <- fast_round(result, digits = args[["digits"]], verbose = verbose)
    }

    ## 4 Handle NA
    if (verbose) {
        printl(function_name, ": step four: handling NA.")
    }
    result <- fast_handle_na(result, verbose = verbose)

    ## 5 Shape set
    if (verbose) {
        printl(function_name, ": step five: shaping result.")
    }
    result <- shape_set(result, final_form = final_form, verbose = verbose)

    # Wrap-up
    return(result)
}
