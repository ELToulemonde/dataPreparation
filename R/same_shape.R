#' Give same shape
#'
#'Transform \code{data_set} into the same shape as \code{reference_set}. Especially this
#' function will be useful to make your test set have the same shape as your train set.
#' @param data_set Matrix, data.frame or data.table to transform
#' @param reference_set Matrix, data.frame or data.table
#' @param verbose Should the algorithm talk? (logical, default to TRUE)
#' @return Return \code{data_set} transformed in order to make it have the same shape as
#' \code{reference_set}
#' @details
#' This function will make sure that \code{data_set} and \code{reference_set}
#' \itemize{
#'    \item have the same class
#'    \item have exactly the same columns
#'    \item have columns with exactly the same class
#'    \item have factor factor with exactly the same levels
#' }
#' You should always use this function before applying your model on a new data set to make sure
#' that everything will go smoothly. But if this function change a lot of stuff you should have a
#' look to your preparation process, there might be something wrong.
#' @examples
#' \dontrun{
#' # Build a train and a test
#' data(tiny_messy_adult)
#' data(adult)
#' train <- messy_adult
#' test <- adult # So test will have missing columns
#'
#'# Prepare them
#' train <- prepare_set(train, verbose = FALSE, key = "country")
#' test <- prepare_set(test, verbose = FALSE, key = "country")
#'
#'# Give them the same shape
#' test <- same_shape(test, train)
#' # As one can see in log, a lot of small change had to be done.
#' # This is an extreme case but you get the idea.
#' }
#' # "##NOT RUN:" mean that this example hasn't been run on CRAN since its long. But you can run it!
@import data.table
#' @export
same_shape <- function(data_set, reference_set, verbose = TRUE) {
    # Working environment
    function_name <- "same_shape"

    # Sanity check
    data_set <- check_and_return_datatable(data_set)
    is.verbose(verbose)

    # Initialization
    # Store class of reference set and transform it into data.table to make computation faster
    reference_set_class <- class(reference_set)
    reference_set <- check_and_return_datatable(reference_set, data_set_name = "reference_set")

    # Computation
    data_set <- create_missing_cols(data_set, reference_set, function_name, verbose)

    data_set <- drop_unwanted_cols(data_set, reference_set, function_name, verbose)

    data_set <- verify_and_correct_column_types(data_set, function_name, reference_set, verbose)

    data_set <- correct_levels_for_factor_columns(data_set, reference_set, function_name, verbose)

    data_set <- reorder_cols(data_set, reference_set)

    data_set <- set_correct_class(data_set, reference_set_class)

    return(data_set)
}

is_class_dataframe <- function(some_class) {
    if (length(some_class) > 1) {
        return(FALSE)
    }
    if (some_class == "data.frame") {
        return(TRUE)
    }
    return(FALSE)
}

is_class_matrix <- function(some_class) {
    if (length(some_class) > 1) { # Might be future matrix
        if (all(some_class == c("matrix", "array"))) {
            return(TRUE)
        }
        else {
            return(FALSE)
        }
    }
    if (some_class == "matrix") {
        return(TRUE)
    }
    return(FALSE)
}


verify_and_correct_column_types <- function(data_set, function_name, reference_set, verbose) {
    if (verbose) {
        printl(function_name, ": verify that every column is in the right type.")
        pb <- init_progress_bar(function_name, names(data_set))
    }
    for (col in names(data_set)) {
        trans_class <- class(data_set[[col]])
        ref_class <- class(reference_set[[col]])
        if (! all(trans_class == ref_class)) {
            transfo_function <- paste0("as.", ref_class[[1]])
            if (exists(transfo_function)) {
                if (verbose) {
                    printl(function_name, ": ", col, " class was ", trans_class, " i set it to ",
                    ref_class, ".")
                }
                set(data_set, NULL, col, get(transfo_function)(data_set[[col]]))
                # Control
                if (! all(class(data_set[[col]]) == ref_class)) {
                    warning(paste0(function_name, ": transformation didn't work. Please control that function ",
                    transfo_function, " indeed transform into ", ref_class, "."))
                }
            }
            else {
                warning(paste0(function_name, ": ", col, " class is ", trans_class, " but should be ", ref_class,
                " and i don't know how to transform it."))
            }
        }
        if (verbose) {
            add_a_tick_to_progress_bar(pb)
        }
    }
    return(data_set)
}


drop_unwanted_cols <- function(data_set, reference_set, function_name, verbose) {
    if (verbose) {
        printl(function_name, ": drop unwanted columns.")
    }
    drop_list <- names(data_set)[! names(data_set) %in% names(reference_set)]
    if (length(drop_list) > 0) {
        if (verbose) {
            printl(function_name, ": the following columns are in data_set but not in reference_set: I drop them: ")
            print(drop_list)
        }
        set(data_set, NULL, drop_list, NULL)
    }
    return(data_set)
}

create_missing_cols <- function(data_set, reference_set, function_name, verbose) {
    if (verbose) {
        printl(function_name, ": verify that every column is present.")
    }
    create_list <- names(reference_set)[! names(reference_set) %in% names(data_set)]
    if (length(create_list) > 0) {
        if (verbose) {
            printl(function_name, ": columns ", paste(create_list, collapse = ", "), " are missing, I create them.")
        }
        set(data_set, NULL, create_list, NA)
    }
    return(data_set)
}

correct_levels_for_factor_columns <- function(data_set, reference_set, function_name, verbose) {
    if (verbose) {
        printl(function_name, ": verify that every factor as the right number of levels.")
        pb <- init_progress_bar(function_name, names(data_set))
    }
    cols <- real_cols(data_set = data_set, cols = names(data_set), function_name = function_name, types = "factor",
    verbose = FALSE)
    for (col in cols) {
        transfo_levels <- levels(data_set[[col]])
        ref_levels <- levels(reference_set[[col]])
        if (! identical(transfo_levels, ref_levels)) {
            set(data_set, NULL, col, factor(data_set[[col]], levels = ref_levels))
            if (verbose) {
                printl(function_name, ": ", col, " class had different levels than in reference_set I change it.")
            }
        }
        if (verbose) {
            add_a_tick_to_progress_bar(pb)
        }
    }
    return(data_set)
}

set_correct_class <- function(data_set, reference_set_class) {
    if (! identical(reference_set_class, class(data_set))) {
        if (is_class_dataframe(reference_set_class)) {
            setDF(data_set)
        }
        if (is_class_matrix(reference_set_class)) {
            data_set <- as.matrix(data_set)
        }
    }
    return(data_set)
}

reorder_cols <- function(data_set, reference_set) {
    setcolorder(data_set, names(reference_set))
    return(data_set)
}