#' Automatic dataSet aggregation by key
#'
#' Automatic aggregation of a dataSet set according to a \code{key}.
#' @param dataSet Matrix, data.frame or data.table (with only numeric, integer, factor, logical, character columns)
#' @param key Name of a column of dataSet according to which the set should be aggregated (character)
#' @param verbose Should the algorithm talk? (logical, default to TRUE)
#' @param thresh Number of max values for frequencies count (numerical, default to 53)
#' @param ... Optional argument: \code{functions}:  aggregation functions for numeric columns 
#' (vector of function names (character), optional, if not set we use: c("mean", "min", "max", "sd"))
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
#' adult_aggregated <- aggregateByKey(adult, key = 'country', functions = c("power", "sqrt"))
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
  is.col(dataSet, cols = key, function_name = function_name)
  is.verbose(verbose)
  if (any(! sapply(dataSet, class) %in% c("numeric", "integer", "factor", "logical", "character"))){
    stop( paste0( function_name, ": I can only handle: numeric, integer, factor, logical, character columns. 
                  Please provide them in one of those format."))
  }
  
  ## Initialization
  # Make an nice list of functions
  functions <- c("mean", "min", "max", "sd")
  if (! is.null(args[["functions"]])){
    functions <- args[["functions"]]
  }
  functions <- is.agg_function(functions, function_name)
  name_separator <- build_name_separator(args)
  
  ## Aggregation
  # If there are more lines than unique key: we aggregate
  if (nrow(dataSet) != uniqueN(dataSet[[key]])){ 
    result <- dataSet[, .N, by = key]
    setnames(result, "N", "nbrLines")
    unique_keys <- result[, key, with = FALSE]
    if (verbose){
      printl(function_name, ": I start to aggregate")
      pb <- initPB(function_name, names(dataSet))
      start_time <- proc.time()
    }
    for (col in colnames(dataSet)[colnames(dataSet) != key]){ # we don't aggregate the key!
      result_tmp <- aggregateAcolumn(dataSet = dataSet[, c(key, col), with = FALSE], 
                                     col = col, 
                                     key = key, 
                                     unique_keys = unique_keys,
                                     name_separator = name_separator, 
                                     functions = functions, 
                                     thresh = thresh)
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
# @param unique_keys Unique keys in set (to avoid recomputing it)
# @param name_separator
# @param functions functions for numeric columns
# @param thresh number of max distinct values for frequencies count
# @
# @export # Before exporting this function should be improved!
aggregateAcolumn <- function(dataSet, col, key, unique_keys, name_separator = ".", 
                             functions, thresh = 53, ...){
  ## Environement
  function_name <- "aggregateAcolumn"
  ## Sanity check
  dataSet <- checkAndReturnDataTable(dataSet)
  is.col(dataSet, cols = c(key, col), function_name = function_name)
  if (ncol(dataSet) != 2){
    stop(paste0(function_name, ": dataSet should have 2 columns. (This is a private function)."))
  }
  ## Initialization
  maxNbValuePerKey <- max(unique(dataSet)[, .N, by = key]$N)
  
  ## Computation 
  if (maxNbValuePerKey > 1){
    result_tmp <- copy(unique_keys) # copy because it's a data.table, otherwise it append it
    ## Aggregation of numerics
    if (is.numeric(dataSet[[col]])){ 
      # To-do: if there is a constant nbr of value for each line consider make
      # them columns
      # To-do: if there is a small amount of values: factorize
      code = "result_tmp = dataSet[, .("
      for (fct in functions){
        code = paste0(code, paste(fct, col, sep = name_separator), "=", fct, "(get(col)), ")
      }
      if (length(functions) > 0){
        code = substr(code, start = 1, stop = nchar(code) - 2)
      }
      code = paste0(code, "), by = key]")
      try(eval(parse(text = code)))
      if ("sd" %in% functions){
        # Bug fixing, sd is giving NA if you only have one value while standard deviation is supposed to be 0
        new_col <- paste("sd", col, sep = name_separator)
        set(result_tmp, which(is.na(result_tmp[[new_col]])), new_col, 0) 
      }
      
    }
    ## aggregation of character (categorical or non-categorical)
    if (is.character(dataSet[[col]]) || is.factor(dataSet[[col]])){
      if (fastMaxNbElt(dataSet[[col]], max_n_values = thresh)){ 
        result_tmp <- dcast.data.table(dataSet, 
                                       formula = paste(key, col, sep = "~"), 
                                       fun.aggregate = length,
                                       value.var = col
        )
        setnames(result_tmp, c(key, paste(col, colnames(result_tmp)[-1], sep = name_separator)))
      }
      else{
        result_tmp <- dataSet[, .N, by = key]
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
    result_tmp <- unique(dataSet)
  }
  
  ## Wrapp up
  return(result_tmp)
}
