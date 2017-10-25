#' Automatic dataSet aggregation by key
#'
#' Automatic aggregation of a dataSet set according to a \code{key}.
#' @param dataSet Matrix, data.frame or data.table (with only numeric, integer, factor, logical, character columns)
#' @param key Name of a column of dataSet according to which the set should be aggregated (character)
#' @param verbose Should the algorithm talk? (logical, default to TRUE)
#' @param thresh Number of max values for frequencies count (numerical, default to 53)
#' @param ... Optional argument: \code{functions}:  aggregation functions for numeric columns 
#' (vector of function, optional, if not set we use: c(mean, min, max, sd))
#' @details
#' Perform aggregation depending on column type:
#' \itemize{
#'   \item If column is numeric \code{functions} are performed on the column. So 1 numeric column 
#'     give length(functions) new columns,
#'   \item If column is character or factor and have less than \code{thresh} different values, 
#'     frequency count of values is performed,
#'   \item If column is character or factor with more than \code{thresh} different values, number 
#'     of different values for each \code{key} is performed,
#'   \item If column is logical, count of number and rate of positive is performed.
#' }
#' Be careful using functions argument, given functions should be an aggregation function, 
#' meaning that for multiple values it should only return one value.
#' @return A \code{\link{data.table}} with one line per \code{key} elements and multiple  new columns.
#' @examples
#' # Get generic dataset from R
#' data("adult")
#'
#' # Aggregate it using aggregateByKey, in order to extract characteristics for each country
#' adult_aggregated <- aggregateByKey(adult, key = 'country')
#'
#' # Exmple with other functions
#' power <- function(x){sum(x^2)}
#' adult_aggregated <- aggregateByKey(adult, key = 'country', functions = c(power, sqrt))
#' 
#' # sqrt is not an aggregation function, so it wasn't used.
#' @import data.table
#' @importFrom stats sd
#' @export
aggregateByKey <- function(dataSet, key, verbose = TRUE, thresh = 53, ...){
  ## Environement
  function_name <- "aggregateByKey"                                           # For print(s)
  args <- list(...)
  
  ## Sanity check
  dataSet <- checkAndReturnDataTable(dataSet)
  if (! is.character(key)){
    stop(paste0(function_name, ": key should be a character, you provided a ", class(key), "."))
  }
  is.col(dataSet, cols = key, function_name = function_name)
  if (any(! sapply(dataSet, class) %in% c("numeric", "integer", "factor", "logical", "character"))){
    stop( paste0( function_name, ": I can only handle: numeric, integer, factor, logical, character columns. 
                  Please provide them in one of those format."))
  }
  is.verbose(verbose)
  
  ## Initialization
  # Make an nice list of functions
  functions <- c(mean = mean, min = min, max = max, sd = sd)
  if (! is.null(args[["functions"]])){
    functions <- args[["functions"]]
    # Make as vector
    if (! is.vector(functions)){
      store_name <-  as.character(match.call()$functions)
      functions <- c(functions)
      names(functions) <- store_name
    }
    
    functions <- unique(functions)
    
    # If functions have no names, give one
    if (!is.null(args[["listNames"]])){ # passed by prepareSet
      # This is a bit ugly, but since we are retriving function name depending on how function was called, we need to get the names from first function.
      listNames <- as.character(args[["listNames"]])
    }
    else{
      listNames <- as.character(match.call()$functions)
    }
    if (is.null(names(functions))){
      names(functions) <- listNames[! listNames %in% c("c", "list")]
    }
    # If some functions still have no name, put fun1, fun2...
    if (any(names(functions) == "")){
      names(functions)[names(functions) == ""] <- paste0("fun", 1:sum(names(functions) == ""))
    }
    
    functions <- true.aggFunction(functions, function_name)
  }
  name_separator <- build_name_separator(args)
  
  
  # identification of nbr of element per column
  uniqueN_byCol <- lapply(dataSet, uniqueN)
  
  ## Aggregation
  # If there are more lines than unique key: we aggregate
  if (nrow(dataSet) != uniqueN_byCol[key]){ 
    result <- dataSet[, .N, by = key]
    setnames(result, "N", "nbrLines")
    if (verbose){
      printl(function_name, ": I start to aggregate")
      pb <- initPB(function_name, names(dataSet))
      start_time <- proc.time()
    }
    for (col in colnames(dataSet)[colnames(dataSet) != key]){ # we don't aggregate the key!
      data_sample <- dataSet[, c(key, col), with = FALSE] # To save some RAM
      result_tmp <- aggregateAcolumn(data_sample, col, key, uniqueN_byCol, name_separator, functions, thresh)
      result <- merge(result, result_tmp, by = key, all.x = TRUE)
      if (verbose){
        setPB(pb, col)
      }
    }
    if (verbose){
      printl(function_name, ": ", ncol(result), " columns have been constructed. It took ", 
             round((proc.time() - start_time)[[3]], 2), " seconds. ")
    }
    
    return(result)
  }
  else{ # If there is as many unique key as lines, there is nothing to do
    return(dataSet)
  }
  }






###########################################################################
##################### aggregateAcolumn ####################################
###########################################################################
# Aggregate a column
#
# Given a dataSet set of two columns, this column will aggregate col by key. 
# For numeric, a list of functions of aggregation are performed
# For character they are transformed in either a Locigal or a table of occurence
# @param dataSet Matrix, data.frame or data.table
# @param col name of the column to aggregate
# @param key name of the key according to which aggregation should be performed
# @param numberOfUniqueEltPerCol
# @param name_separator
# @param aggregation functions for numeric columns
# @param thresh number of max distinct values for frequencies count
# @
# @export # Before exporting this function should be improved!
aggregateAcolumn <- function(dataSet, col, key, uniqueN_byCol, name_separator = ".", 
                             functions, thresh = 53, ...){
  ## Environement
  function_name <- "aggregateAcolumn"
  ## Sanity check
  dataSet <- checkAndReturnDataTable(dataSet)
  is.col(dataSet, cols = c(key, col), function_name = function_name)
  
  ## Initialization
  maxNbValuePerKey <- max(dataSet[, uniqueN(get(col)), by = key][, -key, with = FALSE])
  
  ## Computation 
  if (maxNbValuePerKey > 1 ){
    result_tmp <- unique(dataSet[, key, with = FALSE])
    ## Aggregation of numerics
    if (is.numeric(dataSet[[col]])){ 
      # To-do: if there is a constant nbr of value for each line consider make
      # them columns
      # To-do: if there is a small amount of values: factorize
      for(fct in names(functions)){
        new_col <- paste(fct, col, sep = name_separator)
        set(result_tmp, NULL, new_col, dataSet[, functions[[fct]](get(col)), by = key][, - key, with = FALSE])
        
        if (fct == "sd"){
          # Bug fixing, sd is giving NA if you only have one value while standard deviation is supposed to be 0
          set(result_tmp, which(is.na(result_tmp[[new_col]])), new_col, 0) 
        }
      }
      
      
    }
    ## aggregation of character (categorical or non-categorical)
    if (is.character(dataSet[[col]]) || is.factor(dataSet[[col]])){
      if (uniqueN_byCol[col] < thresh){ 
        result_tmp <- dcast.data.table(dataSet[, c(key, col), with = FALSE], 
                                       formula = paste(key, col, sep = "~"), 
                                       fun.aggregate = length,
                                       value.var = col
        )
        setnames(result_tmp, c(key, paste(col, colnames(result_tmp)[-1], sep = name_separator)))
      }
      if (uniqueN_byCol[col] >= thresh){
        result_tmp <- dataSet[, c(key, col), with = FALSE][, .N, by = key]
        setnames(result_tmp, c(key, paste("nbr", col, sep = name_separator)))
      }
    }
    ## Aggregation of logicals
    if (is.logical(dataSet[[col]])){
      result_tmp <- dataSet[, sum(get(col)), by = key]
      setnames(result_tmp, c(key, paste("nbr", col, sep = name_separator)))
    }
    
  }
  if (maxNbValuePerKey == 1){
    # Only one different value by key: we put the value one time by key.
    result_tmp <- dataSet[, c(key, col), with = FALSE] # drop potentially unwanted columns
    result_tmp <- result_tmp[!duplicated(result_tmp), ]
  }
  
  ## Wrapp up
  return(result_tmp)
}