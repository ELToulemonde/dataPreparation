#######################################################################################
############################### Fast filter function ##################################
#######################################################################################
#' Filtering useless variables
#'
#' Delete columns that are constant or in double in your dataSet set.
#' @param dataSet Matrix, data.frame or data.table
#' @param level which columns do you want to filter (1 = constant, 2 = constant and doubles, 3 = constant doubles and bijections, 4 = constant doubles bijections and included)(numeric, default to 3)
#' @param keep_cols List of columns not to drop (list of character, default to NULL)
#' @param verbose Should the algorithm talk (logical or 1 or 2, default to TRUE)
#' @param ... optional parameters to be passed to the function when called from another function
#' @details 
#' \code{verbose} can be set to 2 have full details from which functions, otherwise they 
#' don't log. (\code{verbose = 1} is equivalent to \code{verbose = TRUE}).
#' @return 
#' The same dataSet but with fewer columns. Columns that are constant, in double, 
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
#' df <- fastFilterVariables(df)
#' head(df)
#' @import data.table
#' @export
fastFilterVariables <- function(dataSet, level = 3, keep_cols = NULL, verbose = TRUE, ...){
  ## Working environement
  function_name <- "fastFilterVariables"
  
  ## Sanity check
  dataSet <- checkAndReturnDataTable(dataSet = dataSet, name = "DEBUG: see fastFilterVariables")
  is.verbose_level(verbose, max_level = 2, function_name = function_name)
  keep_cols <- real_cols(dataSet, keep_cols, function_name = function_name)
  is.filtering_level(level, function_name = function_name)
  
  ## Initalization
  # Arguments for log
  args <- list(...)
  dataName <- "dataSet"
  if (length(args) > 0){
    if (!is.null(args[["dataName"]])){
      dataName <- args[["dataName"]]
    }
  }
  
  ## Computation
  # Delete constant columns
  if (level >= 1){
    if (verbose){
      printl(function_name, ": I check for constant columns.")
    }
    constant_cols <- whichAreConstant(dataSet, keep_cols = keep_cols, verbose = verbose >= 2)
    if (length(constant_cols) > 0){
      if (verbose){
        printl(function_name, ": I delete ", length(constant_cols), " constant column(s) in ", dataName, ".")
      }
      dataSet[, (constant_cols) := NULL]
    }
  }
  
  # Delete columns in double
  if (level >= 2){
    if (verbose){
      printl(function_name, ": I check for columns in double.")
    }
    double_cols <- whichAreInDouble(dataSet, keep_cols = keep_cols, verbose = verbose >= 2)
    if (length(double_cols) > 0){
      if (verbose){
        printl(function_name, ": I delete ", length(double_cols), " column(s) that are in double in ", dataName, ".")
      }  
      dataSet[, (double_cols) := NULL]
    }
  }
  
  
  # Delete columns that are bijections
  if (level >= 3){
    if (verbose){
      printl(function_name, ": I check for columns that are bijections of another column.")
    }
    bijection_cols <- whichAreBijection(dataSet, keep_cols = keep_cols, verbose = verbose >= 2)
    if (length(bijection_cols) > 0){
      if (verbose){
        printl(function_name, ": I delete ", length(bijection_cols), 
               " column(s) that are bijections of another column in ", dataName, ".")
      }  
      dataSet[, (bijection_cols) := NULL]
    }
  }
  
  # Delete columns that are included
  if (level >= 4){
    if (verbose){
      printl(function_name, ": I check for columns that are included in another column.")
    }
    included_cols <- whichAreIncluded(dataSet, keep_cols = keep_cols, verbose = verbose >= 2)
    if (length(included_cols) > 0){
      if (verbose){
        printl(function_name, ": I delete ", length(included_cols), 
               " column(s) that are bijections of another column in ", dataName, ".")
      }  
      dataSet[, (included_cols) := NULL]
    }
  }
  
  ## Wrapp up
  return(dataSet)
}

#######################################################################################
############################# is.filtering_level ######################################
#######################################################################################

# Control that level is indeed a filtering level
# @param level which columns do you want to filter (1 = constant, 2 = constant and doubles, 3 = constant doubles and bijections, 4 = constant doubles bijections and included)(numeric, default to 3)
is.filtering_level <- function(level, function_name = "is.filtering_level"){
  if (! is.numeric(level) || level < 1 || level > 4){
    stop(paste0(function_name, ": level should be 1, 2, 3 or 4."))
  }
}
#######################################################################################
############################### Fast round ############################################
#######################################################################################
#' Fast round
#' 
#' Fast round of numeric columns in a data.table. Will only round numeric, so don't worry about characters. 
#' Also, it computes it column by column so your RAM is safe too.
#' @param dataSet matrix, data.frame or data.table
#' @param cols List of numeric column(s) name(s) of dataSet to transform. To transform all 
#' numerics columns, set it to "auto" (characters, default to "auto")
#' @param digits The number of digits after comma (numeric, default to 2)
#' @param verbose Should the algorithm talk? (logical, default to TRUE)
#' @details
#' It is performing round by \strong{reference} on dataSet, column by column, only on numercial columns. 
#' So that it avoid copying dataSet in RAM.
#' @return The same datasets but as a data.table and with numeric rounded.
#' @examples
#' # First let's build a very large data.table with random numbers
#' require(data.table)
#' M <- as.data.table(matrix(runif (3e4), ncol = 10))
#' 
#' M_rouded <- fastRound(M, 2)
#' # Lets add some character
#' M[, stringColumn := "a string"] 
#' 
#' # And use our function
#' M_rouded <- fastRound(M, 2)
#' # It still work :) and you don't have to worry about the string.
#' @import data.table
#' @export
fastRound <- function(dataSet, cols = "auto", digits = 2, verbose = TRUE){
  ## Working environement
  function_name <- "fastRound"
  
  ## Sanity check
  dataSet <- checkAndReturnDataTable(dataSet)
  if (!is.numeric(digits)){stop(paste0(function_name, ": digits should be an integer."))}
  is.verbose(verbose)
  cols <- real_cols(dataSet, cols = cols, function_name = function_name, types = c("numeric", "integer"))
  
  ## Initialization
  digits <- round(digits, 0) # just to be safe
  
  ## Computation
  if (verbose){
    pb <- initPB(function_name, cols)
  }
  for (col in cols){
    set(dataSet, NULL, col, round(dataSet[[col]], digits))
    if (verbose){
      setPB(pb, col)
    }
  }
  gc(verbose = FALSE)
  
  ## Wrapp-up
  return(dataSet)
}

#######################################################################################
##################################### Handle NA #######################################
#######################################################################################
#' Handle NA values
#'
#' Handle NAs values depending on the class of the column.
#' @param dataSet Matrix, data.frame or data.table
#' @param set_num NAs replacement for numeric column, (numeric or function, default to 0)
#' @param set_logical NAs replacement for logical column, (logical or function, default to FALSE)
#' @param set_char NAs replacement for character column, (character or function, default to "")
#' @param verbose Should the algorithm talk (logical, default to TRUE)
#' @details 
#' To preserve RAM this function edits dataSet by \strong{reference}. To keep object unchanged, please use \code{\link{copy}}. \cr
#' If you provide a function, it will be applied to the full column. So this function should handle NAs. \cr
#' For factor columns, it will add NA to list of values.
#' @return dataSet as a \code{\link{data.table}} with NAs replaced.
#' @examples
#' # Build a useful dataSet set for example
#' require(data.table)
#' dataSet <- data.table(numCol = c(1, 2, 3, NA),
#'                    charCol = c("", "a", NA, "c"),
#'                    booleanCol = c(TRUE, NA, FALSE, NA))
#'
#' # To set NAs to 0, FALSE and "" (respectively for numeric, logical, character)
#' fastHandleNa(copy(dataSet))
#'
#' # In a numeric column to set NAs as "missing"
#' fastHandleNa(copy(dataSet), set_char = "missing")
#' 
#' # In a numeric column, to set NAs to the minimum value of the column#'                    
#' fastHandleNa(copy(dataSet), set_num = min) # Won't work because min(c(1, NA)) = NA so put back NA
#' fastHandleNa(copy(dataSet), set_num = function(x)min(x,na.rm = TRUE)) # Now we handle NAs
#'
#' # In a numeric column, to set NAs to the share of NAs values
#' rateNA <- function(x){sum(is.na(x)) / length(x)}
#' fastHandleNa(copy(dataSet), set_num = rateNA) 
#' 
#' @import data.table
#' @export
fastHandleNa <- function(dataSet, set_num = 0, set_logical = FALSE, 
                         set_char = "", verbose = TRUE){
  ## Working environement
  function_name <- "fastHandleNa"
  
  ## Sanity check
  dataSet <- checkAndReturnDataTable(dataSet)
  is.verbose(verbose)
  
  ## Initialization
  # Transform into function
  num_fun <- function.maker(set_num, "numeric", function_name, "set_num")
  logical_fun <- function.maker(set_logical, "logical", function_name, "set_logical")
  char_fun <- function.maker(set_char, "character", function_name, "set_char")
  
  if (verbose){
    pb <- initPB(function_name, names(dataSet))
  }
  
  ## Computation
  for (col in names(dataSet)){ 
    if (is.numeric(dataSet[[col]])){
      set(dataSet, which(is.na(dataSet[[col]])), col, num_fun(dataSet[[col]]))
    }
    if (is.logical(dataSet[[col]])){
      set(dataSet, which(is.na(dataSet[[col]])), col, logical_fun(dataSet[[col]]))
    }
    if (is.character(dataSet[[col]])){
      set(dataSet, which(is.na(dataSet[[col]])), col, char_fun(dataSet[[col]]))
    }
    if (is.factor(dataSet[[col]])){
      if (sum(is.na(dataSet[[col]])) > 0 ){
        set(dataSet, NULL, col, addNA(dataSet[[col]]))
        # Set level to string NA, otherwise it cause mistake, especialy for randomForests
        levels(dataSet[[col]])[is.na(levels(dataSet[[col]]))] <- "NA" 
      }
    }
    if (verbose){
      setPB(pb, col)
    }
  }
  gc(verbose = FALSE)
  ## Wrapp-up
  return(dataSet)
}

#######################################################################################
############################### Fast is equal function ################################
#######################################################################################
#' Fast checks of equality
#' 
#' Performs quick check if two objects are equal.
#' @param object1 An element, a vector, a data.frame, a data.table
#' @param object2 An element, a vector, a data.frame, a data.table
#' @details 
#' This function uses exponential search trick, so it is fast for very large vectors, data.frame and data.table. 
#' This function is also very robust; you can compare a lot of stuff without failing.
#' @return Logical (TRUE or FALSE) if the two objects are equals.
#' @examples
#' # Test on a character
#' fastIsEqual("a", "a")
#' fastIsEqual("a", "b")
#' 
#' # Test on a vector
#' myVector <- rep(x = "a", 10000)
#' fastIsEqual(myVector, myVector)
#' 
#' # Test on a data.table
#' fastIsEqual(messy_adult, messy_adult)
#' @import data.table
#' @export
fastIsEqual <- function(object1, object2){
  # Control on class
  if (any(class(object1) != class(object2))){
    return(FALSE) 
  }
  # Control on length
  if (length(object1) != length(object2)){
    return(FALSE)
  }
  # List handeling
  if (is.data.frame(object1) || is.list(object1)){
    i <- 1
    n <- length(object1)
    result <- TRUE
    while (result & i <= n){
      result <- result & fastIsEqual(object1[[i]], object2[[i]]) 
      i <- i + 1  
    }
    return(result)
  }
  # Simple comparaison for factors
  if (is.factor(object1)){
    if (! identical(levels(object1), levels(object2))){
      return(FALSE) # To-do Limitation: les levels vides
    }
  }
  
  # Comparaison for short object
  if (length(object1) <= 3){ # (length(object1) <= 3) => (max_power == 0) =>  We check direcly for equality
    return(identical(object1, object2))
  }
  
  # Comparaison for long object
  exp_factor <- 10
  max_power <- floor(log(length(object1)) / log(exp_factor)) + 1
  for (i in 1:max_power){
    I <- (exp_factor^(i - 1)):min(exp_factor^i - 1, length(object1))
    if (! identical(object1[I], object2[I])){
      return(FALSE)
    }
  }
  
  # If every test passed, it's true
  return(TRUE)
}

#######################################################################################
############################### Fast is bijection function ############################
#######################################################################################
#' @import data.table
fastIsBijection <- function(object1, object2){
  ## Working environement
  function_name <- "fastIsBijection"
  
  ## Sanity check
  
  ## Initialization
  
  ## Computation
  # Concluded that there is bijection if and only if: 
  # number of unique elements in col1, is equal to the number of unique couples (col1, col2)
  nrows <- length(object1)
  exp_factor <- 10
  max_power <- floor(log(nrows) / log(exp_factor)) + 1
  for (i in 1:max_power){
    I <- (exp_factor ^ (i - 1)):min(exp_factor ^ i - 1,  nrows)
    n1 <- uniqueN(object1[I])
    n2 <- uniqueN(object2[I])
    if (n2 != n1){
      return(FALSE)
    }
    
    n12 <- uniqueN(data.frame(object1 = object1[I], object2 = object2[I]))
    
    if (n12 != n1){
      return(FALSE)
    }
  }  
  # If every test passed, it's true
  return(TRUE)
}

#######################################################################################
############################### Fast check if has less than n elt #####################
#######################################################################################
# Check number of various values
#
# Using exponential search, it check if there are indeed less than max_n_values in object
# @param object a column of a dataSet set
# @param max_n_values number of maximal acceptable values in object (numeric, default to 1)
# @retrun logical. Return TRUE if there are less or equal to max_n_values in object.
fastMaxNbElt <- function(object, max_n_values = 1){
  ## Initialization
  listOfUnique <- NULL
  exp_factor <- 10
  max_power <- floor(log(length(object)) / log(exp_factor)) + 1
  i <- 1
  
  ## Computation
  for (i in 1:max_power){
    I <- (exp_factor ^ (i - 1)):min(exp_factor ^ i - 1, length(object))
    listOfUnique <- unique( c( listOfUnique, unique( object[I])))
    if (length(listOfUnique) > max_n_values){
      return(FALSE)
    }
  }
  
  ## Wrapp-up
  # if every test passed, return TRUE
  return(TRUE)
}


#######################################################################################
############################### fastIsIncluded ########################################
#######################################################################################
## WARNING: this is a project function, it isn't used yet.
# Concluded that there is inclusion of object2 in object1 if and only if: 
# number of unique elements in object1, is equal to the number of unique couples (object1, object2)
# Return either:
# - NULL if there is no inclusion
# - 1: if object1 is included in object2
# - 2: if object2 is included in object1
# - "bijection": if there is a bijection between object 1 and object2.
#' @import data.table
# fastIsIncluded <- function(object1, object2){
  # ## Working environement
  # function_name <- "fastIsIncluded"
  
  # ## Sanity check
  
  # ## Initialization
  # nrows <- length(object1)
  # exp_factor <- 10
  # max_power <- floor(log(nrows) / log(exp_factor)) + 1
  # included_1 <- TRUE
  # included_2 <- TRUE  
  
  # ## Computation
  # for (i in 1:max_power){
    # I <- (exp_factor ^ (i - 1)):min(exp_factor ^ i - 1,  nrows)
    # n1 <- uniqueN(object1[I])
    # n2 <- uniqueN(object2[I])
    # n12 <- uniqueN(data.frame(object1 = object1[I], object2 = object2[I]))
    
    # if (n12 != n1 & n12 != n2){
      # return(NULL)
    # }
    # included_1 <- TRUE & (n12 == n1)
    # included_2 <- TRUE & (n12 == n2)
  # }
  # # If every test passed, it's true
  # if (included_1 & ! included_2){
    # return(2)
  # }
  # if (included_2 & ! included_1){
    # return(1)
  # }
  # return("bijection")
# }

