#######################################################################################
############################## Fast filter function ##################################
#######################################################################################
#' Filtering useless variables
#'
#' Delete columns that are constant or in double in your data_set set.
#' @param data_set Matrix, data.frame or data.table
#' @param level which columns do you want to filter (1 = constant, 2 = constant and doubles,
#' 3 = constant doubles and bijections, 4 = constant doubles bijections and included)(numeric, default to 3)
#' @param keep_cols List of columns not to drop (list of character, default to NULL)
#' @param verbose Should the algorithm talk (logical or 1 or 2, default to TRUE)
#' @param ... optional parameters to be passed to the function when called from another function
#' @details
#' \code{verbose} can be set to 2 have full details from which functions, otherwise they
#' don't log. (\code{verbose = 1} is equivalent to \code{verbose = TRUE}).
#' @return
#' The same data_set but with fewer columns. Columns that are constant, in double,
#' or bijection of another have been deleted.
#' @examples
#' # First let's build a data.frame with 3 columns: a constant column, and a column in double
#' df <- data.frame(col1 = 1, col2 = rnorm(1e6), col3 = sample(c(1, 2), 1e6, replace = TRUE))
#' df$col4 <- df$col2
#' df$col5[df$col3 == 1] = "a"
#' df$col5[df$col3 == 2] = "b" # Same info than in col1 but with a for 1 and b for 2
#' head(df)
#'
#' # Let's filter columns:
#' df <- fast_filter_variables(df)
#' head(df)
#' @import data.table
#' @export
fast_filter_variables <- function(data_set, level = 3, keep_cols = NULL, verbose = TRUE, ...) {
    # Working environment
    function_name <- "fast_filter_variables"
    args <- list(...)
    data_set_name <- get_data_set_name_from_args(args)

    # Sanity check
    data_set <- check_and_return_datatable(data_set = data_set, data_set_name = data_set_name)
    is.verbose_level(verbose, max_level = 2, function_name = function_name)
    keep_cols <- real_cols(data_set = data_set, cols = keep_cols, function_name = function_name)
    is.filtering_level(level, function_name = function_name)

    # Computation
    # Delete constant columns
    if (level >= 1) {
      if (verbose) {
        printl(function_name, ": I check for constant columns.")
      }
      constant_cols <- which_are_constant(data_set, keep_cols = keep_cols, verbose = verbose >= 2)
      if (length(constant_cols) > 0) {
          if (verbose) {
              printl(function_name, ": I delete ", length(constant_cols),
                     " constant column(s) in ", data_set_name, ".")
          }
          set(data_set, NULL, constant_cols, NULL)
      }
    }

    # Delete columns in double
    if (level >= 2) {
        if (verbose) {
          printl(function_name, ": I check for columns in double.")
        }
        double_cols <- which_are_in_double(data_set, keep_cols = keep_cols, verbose = verbose >= 2)
        if (length(double_cols) > 0) {
            if (verbose) {
                printl(function_name, ": I delete ", length(double_cols),
                       " column(s) that are in double in ", data_set_name, ".")
            }
            set(data_set, NULL, double_cols, NULL)
        }
    }


    # Delete columns that are bijections
    if (level >= 3) {
        if (verbose) {
            printl(function_name, ": I check for columns that are bijections of another column.")
        }
        bijection_cols <- which_are_bijection(data_set, keep_cols = keep_cols, verbose = verbose >= 2)
        if (length(bijection_cols) > 0) {
            if (verbose) {
                printl(function_name, ": I delete ", length(bijection_cols),
                " column(s) that are bijections of another column in ", data_set_name, ".")
            }
            set(data_set, NULL, bijection_cols, NULL)
        }
    }

    # Delete columns that are included
    if (level >= 4) {
        if (verbose) {
            printl(function_name, ": I check for columns that are included in another column.")
        }
        included_cols <- which_are_included(data_set, keep_cols = keep_cols, verbose = verbose >= 2)
        if (length(included_cols) > 0) {
            if (verbose) {
                printl(function_name, ": I delete ", length(included_cols),
                " column(s) that are bijections of another column in ", data_set_name, ".")
            }
            set(data_set, NULL, included_cols, NULL)
        }
    }

    # Wrapp up
    return(data_set)
}

get_data_set_name_from_args <- function(args) {
    data_set_name <- "data_set"
    if (length(args) > 0) {
        if (! is.null(args[["data_set_name"]])) {
            data_set_name <- args[["data_set_name"]]
        }
    }
    return(data_set_name)
}

#######################################################################################
############################ is.filtering_level ######################################
#######################################################################################

# Control that level is indeed a filtering level
# @param level which columns do you want to filter
# (1 = constant, 2 = constant and doubles, 3 = constant doubles and bijections,
# 4 = constant doubles bijections and included)(numeric, default to 3)
is.filtering_level <- function(level, function_name = "is.filtering_level") {
    if (! is.numeric(level) || level < 1 || level > 4) {
        stop(paste0(function_name, ": level should be 1, 2, 3 or 4."))
    }
}
#######################################################################################
############################## Fast round ############################################
#######################################################################################
#' Fast round
#'
#'Fast round of numeric columns in a data.table. Will only round numeric, so don't worry about characters.
#' Also, it computes it column by column so your RAM is safe too.
#' @param data_set matrix, data.frame or data.table
#' @param cols List of numeric column(s) name(s) of data_set to transform. To transform all
#' numerics columns, set it to "auto" (characters, default to "auto")
#' @param digits The number of digits after comma (numeric, default to 2)
#' @param verbose Should the algorithm talk? (logical, default to TRUE)
#' @details
#' It is performing round by \strong{reference} on data_set, column by column, only on numercial columns.
#' So that it avoid copying data_set in RAM.
#' @return The same datasets but as a data.table and with numeric rounded.
#' @examples
#' # First let's build a very large data.table with random numbers
#' require(data.table)
#' M <- as.data.table(matrix(runif (3e4), ncol = 10))
#'
#'M_rouded <- fast_round(M, 2)
#' # Lets add some character
#' M[, stringColumn := "a string"]
#'
#'# And use our function
#' M_rouded <- fast_round(M, 2)
#' # It still work :) and you don't have to worry about the string.
#' @import data.table
#' @export
fast_round <- function(data_set, cols = "auto", digits = 2, verbose = TRUE) {
  # Working environment
  function_name <- "fast_round"

  # Sanity check
  data_set <- check_and_return_datatable(data_set)
  if (! is.numeric(digits)) {
    stop(paste0(function_name, ": digits should be an integer."))
  }
  is.verbose(verbose)
  cols <- real_cols(data_set, cols = cols, function_name = function_name, types = c("numeric", "integer"))

  # Initialization
  digits <- round(digits, 0) # just to be safe

  # Computation
  if (verbose) {
      pb <- init_progress_bar(function_name, cols)
  }
  for (col in cols) {
    set(data_set, NULL, col, round(data_set[[col]], digits))
    if (verbose) {
      add_a_tick_to_progress_bar(pb)
    }
  }

  # Wrap-up
  return(data_set)
}

#######################################################################################
#################################### Handle NA #######################################
#######################################################################################
#' Handle NA values
#'
#' Handle NAs values depending on the class of the column.
#' @param data_set Matrix, data.frame or data.table
#' @param set_num NAs replacement for numeric column, (numeric or function, default to 0)
#' @param set_logical NAs replacement for logical column, (logical or function, default to FALSE)
#' @param set_char NAs replacement for character column, (character or function, default to "")
#' @param verbose Should the algorithm talk (logical, default to TRUE)
#' @details
#' To preserve RAM this function edits data_set by \strong{reference}. To keep object unchanged,
#' please use \code{\link{copy}}. \cr
#' If you provide a function, it will be applied to the full column. So this function should handle NAs. \cr
#' For factor columns, it will add NA to list of values.
#' @return data_set as a \code{\link{data.table}} with NAs replaced.
#' @examples
#' # Build a useful data_set set for example
#' require(data.table)
#' data_set <- data.table(numCol = c(1, 2, 3, NA),
#'                    charCol = c("", "a", NA, "c"),
#'                    booleanCol = c(TRUE, NA, FALSE, NA))
#'
#' # To set NAs to 0, FALSE and "" (respectively for numeric, logical, character)
#' fast_handle_na(copy(data_set))
#'
#' # In a numeric column to set NAs as "missing"
#' fast_handle_na(copy(data_set), set_char = "missing")
#'
#'# In a numeric column, to set NAs to the minimum value of the column#'
#' fast_handle_na(copy(data_set), set_num = min) # Won't work because min(c(1, NA)) = NA so put back NA
#' fast_handle_na(copy(data_set), set_num = function(x)min(x,na.rm = TRUE)) # Now we handle NAs
#'
#' # In a numeric column, to set NAs to the share of NAs values
#' rateNA <- function(x) {
#'   sum(is.na(x)) / length(x)
#' }
#' fast_handle_na(copy(data_set), set_num = rateNA)
#'
#'@import data.table
#' @export
fast_handle_na <- function(data_set, set_num = 0, set_logical = FALSE,
set_char = "", verbose = TRUE) {
    # Working environment
    function_name <- "fast_handle_na"

    # Sanity check
    data_set <- check_and_return_datatable(data_set)
    is.verbose(verbose)

    # Initialization
    # Transform into function
    num_fun <- function.maker(set_num, "numeric", function_name, "set_num")
    logical_fun <- function.maker(set_logical, "logical", function_name, "set_logical")
    char_fun <- function.maker(set_char, "character", function_name, "set_char")

    if (verbose) {
        pb <- init_progress_bar(function_name, names(data_set))
    }

    # Computation
    for (col in names(data_set)) {
        if (is.numeric(data_set[[col]])) {
            set(data_set, which(is.na(data_set[[col]])), col, num_fun(data_set[[col]]))
        }
        if (is.logical(data_set[[col]])) {
            set(data_set, which(is.na(data_set[[col]])), col, logical_fun(data_set[[col]]))
        }
        if (is.character(data_set[[col]])) {
            set(data_set, which(is.na(data_set[[col]])), col, char_fun(data_set[[col]]))
        }
        if (is.factor(data_set[[col]])) {
            if (sum(is.na(data_set[[col]])) > 0) {
                set(data_set, NULL, col, addNA(data_set[[col]]))
                # Set level to string NA, otherwise it cause mistake, especialy for randomForests
                levels(data_set[[col]])[is.na(levels(data_set[[col]]))] <- "NA"
            }
        }
        if (verbose) {
            add_a_tick_to_progress_bar(pb)
        }
    }
    # Wrap-up
    return(data_set)
}

#######################################################################################
############################## Fast is equal function ################################
#######################################################################################
#' Fast checks of equality
#'
#'Performs quick check if two objects are equal.
#' @param object1 An element, a vector, a data.frame, a data.table
#' @param object2 An element, a vector, a data.frame, a data.table
#' @details
#' This function uses exponential search trick, so it is fast for very large vectors, data.frame and data.table.
#' This function is also very robust; you can compare a lot of stuff without failing.
#' @return Logical (TRUE or FALSE) if the two objects are equals.
#' @examples
#' # Test on a character
#' fast_is_equal("a", "a")
#' fast_is_equal("a", "b")
#'
#'# Test on a vector
#' myVector <- rep(x = "a", 10000)
#' fast_is_equal(myVector, myVector)
#'
#'# Test on a data.table
#' fast_is_equal(tiny_messy_adult, messy_adult)
#' @import data.table
#' @export
fast_is_equal <- function(object1, object2) {
    # Control on class
    if (any(class(object1) != class(object2))) {
        return(FALSE)
    }
    # Control on length
    if (length(object1) != length(object2)) {
        return(FALSE)
    }
    # List handeling
    if (is.data.frame(object1) || is.list(object1)) {
        for (i in seq_len(length(object1))) {
            if (! fast_is_equal(object1[[i]], object2[[i]])) {
                return(FALSE)
            }
        }
        return(TRUE)
    }
    # Simple comparaison for factors
    if (is.factor(object1)) {
        if (! identical(levels(object1), levels(object2))) {
            return(FALSE) # To-do Limitation: les levels vides
        }
    }
    return(exponential_equality_check(object1, object2))
}

exponential_equality_check <- function(object1, object2) {
    exp_factor <- 10
    max_power <- floor(log(length(object1)) / log(exp_factor)) + 1
    for (i in 1 : max_power) {
        indexes <- (exp_factor ^ (i - 1)) : min(exp_factor ^ i - 1, length(object1))
        if (! identical(object1[indexes], object2[indexes])) {
            return(FALSE)
        }
    }
    return(TRUE)
}
#######################################################################################
############################## Fast is bijection function ############################
#######################################################################################
#' @import data.table
fast_is_bijection <- function(object1, object2) {
    # Initialization
    nrows <- length(object1)
    exp_factor <- 10
    max_power <- floor(log(nrows) / log(exp_factor)) + 1
    # Create empty object of the correct class. Genreic way (ex: for POSIXct try it)
    empty_object_of_class_1 <- get(paste0("as.", class(object1)[1]))(character())
    empty_object_of_class_2 <- get(paste0("as.", class(object2)[1]))(character())
    unique_couples <- data.table(object1 = empty_object_of_class_1, object2 = empty_object_of_class_2)

    # Computation
    for (i in 1 : max_power) {
        indexes <- (exp_factor ^ (i - 1)) : min(exp_factor ^ i - 1, nrows)
        n1 <- uniqueN(c(object1[indexes], unique_couples[["object1"]]))
        n2 <- uniqueN(c(object2[indexes], unique_couples[["object2"]]))
        if (n2 != n1) {
            return(FALSE)
        }
        # Compute unique couples
        potential_unique_couples <- rbind(data.table(object1 = object1[indexes],
                                                     object2 = object2[indexes]), unique_couples)
        unique_couples <- potential_unique_couples[, unique(.SD), .SDcols = c("object1", "object2")]
        n12 <- nrow(unique_couples)

        if (n12 != n1) {
            return(FALSE)
        }
    }
    # If every test passed, it's true
    return(TRUE)
}

#######################################################################################
############################## Fast check if has less than n elt #####################
#######################################################################################
# Check number of various values
#
# Using exponential search, it check if there are indeed less than max_n_values in object
# @param object a column of a data_set set
# @param max_n_values number of maximal acceptable values in object (numeric, default to 1)
# @retrun logical. Return TRUE if there are less or equal to max_n_values in object.
fast_is_there_less_than <- function(object, n_values = 1) {
    # Initialization
    unique_elements <- NULL
    exp_factor <- 10
    object_length <- length(object)
    max_power <- floor(log(object_length) / log(exp_factor)) + 1

    # Computation
    for (i in 1 : max_power) {
        indexes <- (exp_factor ^ (i - 1)) : min(exp_factor ^ i - 1, object_length)
        unique_elements <- unique(c(unique_elements, unique(object[indexes])))
        if (length(unique_elements) > n_values) {
            return(FALSE)
        }
    }

    # Wrap-up
    return(TRUE)
}
