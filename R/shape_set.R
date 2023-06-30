#' Final preparation before ML algorithm
#'
#' Prepare a data.table by:
#' \itemize{
#'  \item transforming numeric variables into factors whenever they take less than \code{thresh} unique
#'    variables
#'  \item transforming characters using \code{\link{generate_from_character}}
#'  \item transforming logical into binary integers
#'  \item dropping constant columns
#'  \item Sending the data.table to \code{\link{set_as_numeric_matrix}} (when \code{final_form == "numerical_matrix"})
#'    will then allow you to get a numerical matrix usable by most Machine Learning Algorithms.
#' }
#' @param data_set Matrix, data.frame or data.table
#' @param final_form "data.table" or "numerical_matrix" (default to data.table)
#' @param thresh  Threshold such that  a numerical column is transformed into
#'   a factor whenever its number of unique modalities is smaller or equal to
#' \code{thresh} (numeric, default to 10)
#' @param verbose Should the algorithm talk? (logical, default to TRUE)
#' @section Warning:
#' All these changes will happen \strong{by reference}.
#' @export
shape_set <- function(data_set, final_form = "data.table", thresh = 10, verbose = TRUE) {
    # Working environment
    function_name <- "shape_set"

    # Sanity check
    data_set <- check_and_return_datatable(data_set)
    is.verbose(verbose)

    #  Initialization
    col_class <- sapply(data_set, class)
    # Safety for classes with multiple values: ex: POSIXct, POSIXt
    col_class_init <- sapply(col_class, function(x) x[[1]])
    # Computation
    carac_cols <- names(col_class)[col_class %in% c("character")]
    data_set <- set_col_as_factor(data_set, cols = carac_cols, n_levels = - 1, verbose = verbose)

    # NUMERIC INTO FACTORS (IF # OF MODALITIES <= THRESH)
    num_cols <- names(col_class)[col_class %in% c("numeric", "integer")]
    if (length(num_cols) > 0) {
        if (verbose) {
            printl(function_name, ": Transforming numerical variables into factors when length(unique(col)) <= ",
                   thresh, ".")
        }
        data_set <- set_col_as_factor(data_set, cols = num_cols, n_levels = thresh, verbose = FALSE)
    }
    col_class <- sapply(data_set, class)

    # LOGICAL INTO BINARY
    logical_col <- names(col_class)[which(col_class %in% c("logical"))]
    if (length(logical_col) > 0) {
        if (verbose) {
          printl(function_name, ": Transforming logical into binaries.\n")
        }
        for (col in logical_col) set(data_set, NULL, col, as.integer(data_set[[col]] * 1))
    }

    # Distribution des types de colonnes
    if (verbose) {
      col_class_end <- sapply(data_set, class)
      col_class_end <- sapply(col_class_end, function(x) x[[1]])
      printl(function_name, ": Previous distribution of column types:")
      print(table(col_class_init))
      printl(function_name, ": Current distribution of column types:")
      print(table(col_class_end))
    }

    # Wrap-up
    if (final_form == "numerical_matrix") {
        data_set <- set_as_numeric_matrix(data_set)
    }
    return(data_set)
}


#' Numeric matrix preparation for Machine Learning.
#'
#' Prepare a numeric matrix from a data.table. This matrix is suitable for
#' machine learning purposes, since factors are binarized. It may be sparsed,
#' include an intercept, and drop a reference column for each factor if
#' required (when using \code{lm()}, for instance)
#'
#'@param data_set data.table
#' @param intercept Should a constant column be added? (logical, default to FALSE)
#' @param all_cols For each factor, should we create all possible
#' dummies, or should we drop a reference dummy? (logical, default to FALSE)
#' @param sparse Should the resulting matrix be of a (sparse) Matrix
#' class? (logical, default to FALSE)
#' @export
#' @importFrom stats as.formula model.matrix contrasts
#' @importFrom Matrix sparse.model.matrix
#' @import data.table
set_as_numeric_matrix <- function(data_set, intercept = FALSE, all_cols = FALSE,
sparse = FALSE) {

    # SANITY CHECKS
    if (! is.data.table(data_set)) stop("set_as_numeric_matrix: data_set is not a data.table")
    # Checking that columns are of the proper type. Column type modification are
    # not done here, because we would either need to duplicate the data_set in memory
    # (not memory efficient), or we would need to modify them by reference.
    # Modification by reference may be done through a separate function. It seems
    # preferable to keep functions that modify data_set by reference separated from
    # functions that generate new data_set.
    if (any(! sapply(data_set, function(x) is.numeric(x) |
        is.logical(x) |
        is.factor(x)))) {
        stop(paste0("set_as_numeric_matrix: some columns are not numerical/logical/factor.",
             " Consider using shape_set() to prepare the data_set."))
    }

    # FORMULA
    if (intercept)fm <- as.formula("~ .") else fm <- as.formula("~ . - 1")

    # CONTRASTS
    # Getting full contrast (identity) matrices when allCols == TRUE (required to
    # build a full model matrix)
    factor_cols <- which(sapply(data_set, is.factor))
    if (all_cols & length(factor_cols) > 0) {
        ct <- lapply(data_set[, factor_cols, with = FALSE], contrasts, contrasts = FALSE)
    } else {
        ct <- NULL
    }

    # MATRIX
    if (sparse) {
        output_matrix <- sparse.model.matrix(fm, data = data_set, contrasts.arg = ct)
    } else {
        output_matrix <- model.matrix(fm, data = data_set, contrasts.arg = ct)
    }

    # RETURN
    return(output_matrix)
}