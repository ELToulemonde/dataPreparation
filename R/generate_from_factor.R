#' Recode factor
#'
#'Recode factors into 3 new columns:
#' \itemize{
#' \item was the value not NA, "NA", "",
#' \item how often this value occures,
#' \item the order of the value (ex: M/F => 2/1 because F comes before M in alphabet).
#' }
#' @param data_set Matrix, data.frame or data.table
#' @param cols list of character column(s) name(s) of data_set to transform. To transform all
#' factors, set it to "auto". (character, default to "auto")
#' @param verbose Should the function log (logical, default to TRUE)
#' @param drop Should \code{cols} be dropped after generation (logical, default to FALSE)
#' @param ... Other arguments such as \code{name_separator} to separate words in new columns names
#' (character, default to ".")
#' @return \code{data_set} with new columns. \code{data_set} is edited by \strong{reference}.
#' @examples
#' # Load data set
#' data(tiny_messy_adult)
#'
#'# transform column "type_employer"
#' tiny_messy_adult <- generate_from_factor(tiny_messy_adult, cols = "type_employer")
#' head(tiny_messy_adult)
#'
#'# To transform all factor columns:
#' tiny_messy_adult <- generate_from_factor(tiny_messy_adult, cols = "auto")
#' @import data.table
#' @export
generate_from_factor <- function(data_set, cols = "auto", verbose = TRUE, drop = FALSE, ...) {
    # Working environment
    function_name <- "generate_from_factor"

    # Sanity check
    data_set <- check_and_return_datatable(data_set)
    cols <- real_cols(data_set = data_set, cols = cols, function_name = function_name, types = "factor")
    is.verbose(verbose)

    # Initialization
    start_time <- proc.time()
    name_separator <- build_name_separator(list(...))

    # Computation
    for (col in cols) {
        # has value
        new_col <- paste0(col, name_separator, "notnull")
        new_col <- make_new_col_name(new_col, names(data_set))
        set(data_set, NULL, new_col, ! levels(data_set[[col]])[col] %in% c(NA, "NA", ""))

        # recode with nb of occurence of value
        new_col <- paste0(col, name_separator, "num")
        new_col <- make_new_col_name(new_col, names(data_set))
        data_set[, c(new_col) := .N, by = col]

        # recode with order of value
        new_col <- paste0(col, name_separator, "order")
        new_col <- make_new_col_name(new_col, names(data_set))
        col_levels <- levels(data_set[[col]])
        col_levels_order <- order(col_levels)
        set(data_set, NULL, new_col, col_levels_order[col])
        # if asked drop col
        if (isTRUE(drop)) {
            set(data_set, NULL, col, NULL)
        }
    }
    if (verbose) {
        printl(function_name, ": it took me: ", round((proc.time() - start_time)[[3]], 2),
        "s to transform ", length(cols), " factor columns into, ", 3 * length(cols), " new columns.")
    }
    # Wrap-up
    return(data_set)
}


# one_hot_encoder
# ----------------
#' One hot encoder
#'
#'Transform factor column into 0/1 columns with one column per values of the column.
#' @param data_set Matrix, data.frame or data.table
#' @param encoding Result of funcion \code{\link{build_encoding}}, (list, default to NULL). \cr
#' To perform the same encoding on train and test, it is recommended to compute \code{\link{build_encoding}}
#' before. If it is kept to NULL, build_encoding will be called.
#' @param type What class of columns is expected? "integer" (0L/1L), "numeric" (0/1), or "logical" (TRUE/FALSE),
#' (character, default to "integer")
#' @param drop Should \code{cols} be dropped after generation (logical, default to FALSE)
#' @param verbose Should the function log (logical, default to TRUE)
#' @return \code{data_set} edited by \strong{reference} with new columns.
#' @details If you don't want to edit your data set consider sending \code{copy(data_set)} as an input.\cr
#' Please \strong{be carefull} using this function, it will generate as many columns as there different values
#' in your column and might use a lot of RAM. To be safe, you can use parameter
#'  \code{min_frequency} in \code{\link{build_encoding}}.
#' @examples
#' data(tiny_messy_adult)
#'
#'# Compute encoding
#' encoding <- build_encoding(tiny_messy_adult, cols = c("marital", "occupation"), verbose = TRUE)
#'
#'# Apply it
#' tiny_messy_adult <- one_hot_encoder(tiny_messy_adult, encoding = encoding, drop = TRUE)
#'
#'# Apply same encoding to adult
#' data(adult)
#' adult <- one_hot_encoder(adult, encoding = encoding, drop = TRUE)
#'
#'# To have encoding as logical (TRUE/FALSE), pass it in type argument
#' data(adult)
#' adult <- one_hot_encoder(adult, encoding = encoding, type = "logical", drop = TRUE)
#' @export
#' @import data.table
one_hot_encoder <- function(data_set, encoding = NULL, type = "integer", verbose = TRUE, drop = FALSE) {
    # Working environment
    function_name <- "one_hot_encoder"

    # Sanity check
    data_set <- check_and_return_datatable(data_set)
    is.verbose(verbose)
    if (! type %in% c("integer", "logical", "numeric")) {
        stop(paste0(function_name, ": type should either be 'integer', 'numeric' or 'logical.'"))
    }
    as_type <- get(paste0("as.", type)) # Build a_type function to transform result type
    # Initialization
    # Transform char into factor
    if (is.null(encoding)) {
        if (verbose) {
            printl(function_name, ": Since you didn't profvide encoding, I compute them with build_encoding.")
        }
        encoding <- build_encoding(data_set, cols = "auto", verbose = verbose)
    }
    cols <- names(encoding)
    if (verbose) {
        printl(function_name, ": I will one hot encode some columns.")
        pb <- init_progress_bar(function_name, cols)
        start_time <- proc.time()
    }

    # Computation
    for (col in cols) {
        # Log
        if (verbose) {
            printl(function_name, ": I am doing column: ", col)
        }
        # Build columns with 0 value (it save time to pre-set the columns)
        new_cols <- encoding[[col]]$new_cols
        # Set the write value
        for (i in seq_len(length(new_cols))) {
            set(data_set, NULL, new_cols[i], as_type(data_set[[col]] == encoding[[col]]$values[i]))
        }

        # drop col if asked
        if (isTRUE(drop)) {
            set(data_set, NULL, col, NULL)
        }
        # Update progress bar
        if (verbose) {
            add_a_tick_to_progress_bar(pb)
        }
    }

    if (verbose) {
        printl(function_name, ": It took me ", round((proc.time() - start_time)[[3]], 2),
        "s to transform ", length(cols), " column(s).")
    }
    # Wrap-up
    return(data_set)
}

#' Compute encoding
#'
#'Build a list of one hot encoding for each \code{cols}.
#' @param data_set Matrix, data.frame or data.table
#' @param cols List of numeric column(s) name(s) of data_set to transform. To transform all
#' characters, set it to "auto". (character, default to "auto")
#' @param verbose Should the algorithm talk? (Logical, default to TRUE)
#' @param min_frequency The minimal share of lines that a category should represent (numeric,
#' between 0 and 1, default to 0)
#' @param ... Other arguments such as \code{name_separator} to separate words in new columns names
#' (character, default to ".")
#' @details
#' To avoid creating really large sparce matrices, one can use  param \code{min_frequency} to be
#'  sure that only most representative values will be used to create a new column (and not
#'  outlayers or mistakes in data). \cr
#'  Setting \code{min_frequency} to something gretter than 0 may cause the function to be slower
#'  (especially for large data_set).
#' @return A list where each element name is a column name of data set and each element new_cols
#' and values the new columns that will be built during encoding.
#' @examples
#' # Get a data set
#' data(adult)
#' encoding <- build_encoding(adult, cols = "auto", verbose = TRUE)
#'
#'print(encoding)
#'
#'# To limit the number of generated columns, one can use min_frequency parameter:
#' build_encoding(adult, cols = "auto", verbose = TRUE, min_frequency = 0.1)
#' # Set to 0.1, it will create columns only for values that are present 10% of the time.
#' @import data.table
#' @export
build_encoding <- function(data_set, cols = "auto", verbose = TRUE, min_frequency = 0, ...) {
    # Working environment
    function_name <- "build_encoding"

    # Sanity check
    data_set <- check_and_return_datatable(data_set)
    cols <- real_cols(data_set = data_set, cols = cols, function_name = function_name, types = c("factor", "character"))
    is.verbose(verbose)
    is.share(min_frequency, object_name = "min_frequency", function_name = function_name)

    # Initialization
    if (verbose) {
        pb <- init_progress_bar(function_name, cols)
        printl(function_name, ": I will compute encoding on ", length(cols), " character and factor columns.")
        start_time <- proc.time()
    }

    # Retrive arg
    name_separator <- build_name_separator(list(...))
    encoder <- list()
    # Computation
    for (col in cols) {
        # Build columns with 0 value (it save time to pre-set the columns)
        if (is.factor(data_set[[col]])) {
            values <- levels(data_set[[col]])
        }
        else {
            values <- unique(data_set[[col]])
        }
        if (min_frequency > 0) {
            frequency <- data_set[, c("freq") := (.N / nrow(data_set)), by = col]
            to_drop <- frequency[get("freq") < min_frequency, ][[col]]
            values <- setdiff(values, to_drop)
        }
        new_cols <- paste0(col, name_separator, values)
        new_cols <- sapply(new_cols, function(x) make_new_col_name(x, names(data_set)))

        # Set the write value
        encoder[[col]] <- list(new_cols = new_cols, values = values)

        # Update progress bar
        if (verbose) {
            add_a_tick_to_progress_bar(pb)
        }
    }
    if (verbose) {
        printl(function_name, ": it took me: ", round((proc.time() - start_time)[[3]], 2),
        "s to compute encoding for ", length(cols), " character and factor columns.")
    }
    # Wrap-up
    return(encoder)
}

#' Target encode
#'
#'Target encoding is the process of replacing a categorical value with the aggregation of the target variable.
#' the target variable. \code{target_encode} is used to apply this transformations on a data set.
#' Function \code{\link{build_target_encoding}} must be used first to compute aggregations.
#' @param data_set Matrix, data.frame or data.table
#' @param target_encoding result of function \code{\link{build_target_encoding}} (list)
#' @param drop Should \code{col_to_encode} be dropped after generation (logical, default to FALSE)
#' @param verbose Should the algorithm talk? (Logical, default to TRUE)
#' @return \code{data_set} with new cols of \code{target_encoding} merged to \code{data_set}
#' using \code{target_encoding} names as merging key. \code{data_set} is edited by \strong{reference}.
#' @examples
#' # Build a data set
#' require(data.table)
#' data_set <- data.table(student = c("Marie", "Marie", "Pierre", "Louis", "Louis"),
#'                       grades = c(1, 1, 2, 3, 4))
#'
#'# Construct encoding
#' target_encoding <- build_target_encoding(data_set, cols_to_encode = "student",
#'                                          target_col = "grades", functions = c("mean", "sum"))
#'
#'# Apply them
#' target_encode(data_set, target_encoding = target_encoding)
#' @import data.table
#' @export
target_encode <- function(data_set, target_encoding, drop = FALSE, verbose = TRUE) {
    # Working environment
    function_name <- "target_encode"

    # Sanity check
    data_set <- check_and_return_datatable(data_set)
    cols_to_encode <- real_cols(data_set, cols = names(target_encoding), function_name = function_name)
    is.verbose(verbose)

    # Initialization
    if (verbose) {
        pb <- init_progress_bar(function_name, names(data_set))
        printl(function_name, ": Start to encode columns according to target.")
    }

    # Computation
    for (col in cols_to_encode) {
        target_encoding_this_col <- target_encoding[[col]]
        data_set <- merge(data_set, target_encoding_this_col, by = col, all.x = TRUE, sort = FALSE)

        if (verbose) {
            add_a_tick_to_progress_bar(pb)
        }
    }


    # drop col if asked
    if (isTRUE(drop)) {
        set(data_set, NULL, cols_to_encode, NULL)
    }

    # Wrap-up
    return(data_set)
}


#' Build target encoding
#'
#'Target encoding is the process of replacing a categorical value with the aggregation of the
#' target variable. \code{build_target_encoding} is used to compute aggregations.
#' @param data_set Matrix, data.frame or data.table
#' @param cols_to_encode columns to aggregate according to (list)
#' @param target_col column to aggregate (character)
#' @param functions functions of aggregation (list or character, default to "mean"). Functions \code{compute_probability_ratio}
#' and \code{compute_weight_of_evidence} are classically used functions
#' @param verbose Should the algorithm talk? (Logical, default to TRUE)
#' @return A \code{list} of \code{\link{data.table}} a data.table for each \code{cols_to_encode}
#' each data.table containing a line by unique value of column and \code{len(functions) + 1} columns.
#' @examples
#' # Build a data set
#' require(data.table)
#' data_set <- data.table(student = c("Marie", "Marie", "Pierre", "Louis", "Louis"),
#'                       grades = c(1, 1, 2, 3, 4))
#'
#'# Perform target_encoding construction
#' build_target_encoding(data_set, cols_to_encode = "student", target_col = "grades",
#'                       functions = c("mean", "sum"))
#' @import data.table
#' @export
build_target_encoding <- function(data_set, cols_to_encode, target_col, functions = "mean", verbose = TRUE) {
    # Working environment
    function_name <- "build_target_encoding"

    # Sanity check
    data_set <- check_and_return_datatable(data_set)
    cols_to_encode <- real_cols(data_set = data_set, cols = cols_to_encode, function_name = function_name)
    is.col(data_set, cols = c(target_col), function_name = function_name)
    if (is.character(functions)) {
      functions <- c(functions)
    }
    functions <- is.agg_function(functions, function_name)
    is.verbose(verbose)

    # Initialization
    result <- list()
    if (verbose) {
        pb <- init_progress_bar(function_name, names(data_set))
        printl(function_name, ": Start to compute encoding for target_encoding according to col: ",
        target_col, ".")
    }

    # Computation
    for (col in cols_to_encode) {
        result_this_col <- NULL
        result_tmp <- NULL
        for (fun in functions) {
            new_col_name <- paste0(target_col, "_", fun, "_by_", col)
            code <- paste0("result_tmp <- data_set[, .(", new_col_name,
            "= get(fun)(get(target_col))), by = c(col), with=TRUE]")
            try(eval(parse(text = code)))
            if (is.null(result_this_col)) {
                result_this_col <- result_tmp
            }
            else {
                set(result_this_col, NULL, new_col_name, result_tmp[[new_col_name]])
            }
        }
        result[[col]] <- result_this_col

        if (verbose) {
            add_a_tick_to_progress_bar(pb)
        }
    }

    # Wrap-up
    return(result)
}

#' Compute weight of evidence
#'
#'Weight of evidence is an aggregation function that can be used for 
#' \code{build_target_encoding}. Weight of evidence is the ln(P(most freq element) / (1 - P(most frq element))).
#' @param x A \code{list} of categorical elements
#' @details To be more generic, the library compute P(most freq element) inplace of traditional formula ln(P(1)/P(0))
#' @return Weight of evidence
#' @examples
#' # Build example list
#' example_list <- c(1, 1, 1, 2, 2, 3)
#' 
#' # Compute weight of evidence
#' compute_weight_of_evidence(example_list)
#' @export
compute_weight_of_evidence <- function(x){
  # Compute weight of evidence
  return(log(compute_probability_ratio(x)))
}

#' Compute probability ratio
#'
#' Probability ratio is an aggregation function that can be used for 
#' \code{build_target_encoding}. Probability ratio is the P(most freq element) / (1 - P(most frq element)).
#' @param x A \code{list} of categorical elements
#' @details To be more generic, the library compute P(most freq element) inplace of traditional formula P(1)/P(0)
#' @return P(most freq element) / (1 - P(most frq element))
#' @examples
#' # Build example list
#' example_list <- c(1, 1, 1, 2, 2, 3)
#' 
#' # Compute probability ratio
#' compute_probability_ratio(example_list)
#' @export
compute_probability_ratio <- function(x){
  # Get most frequent class
  most_frequent_class <- get_most_frequent_element(x)
  # Compute class probability
  probability_of_most_frequent_class <- sum(x == most_frequent_class) / length(x)
  # Compute weight of evidence
  return(sum(x == most_frequent_class) / (1 -probability_of_most_frequent_class))
}


