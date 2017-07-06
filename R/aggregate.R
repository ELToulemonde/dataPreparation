#' Automatic dataSet aggregation by key
#'
#' Automatic aggregation of a dataSet set according to a key.\cr
#' @param dataSet Matrix, data.frame or data.table (with only numeric, integer, factor, logical, character columns).
#' @param key The name of a column of dataSet according to which the set should be aggregated (character)
#' @param verbose Should the algorithm talk? (logical, default to TRUE)
#' @param ... optional argument: \code{functions}:  aggregation functions for numeric columns (list of functions)
#' (vector of function, optional, if not set we use: c(mean, min, max, sd))
#' @details
#' Be carefull using functions agrument, the function given should be an aggregation fuction, meaning that for multiple values it should only return one value.
#' @return A \code{\link{data.table}} with one line per key elements.
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
aggregateByKey <- function(dataSet, key, verbose = TRUE, ...){
  ## Environement
  function_name <- "aggregateByKey"                                           # For print(s)
  args <- list(...)
  
  ## Sanity check
  dataSet <- checkAndReturnDataTable(dataSet)
  is.col(dataSet, cols = key, function_name = function_name)
  if (any(! sapply(dataSet, class) %in% c("numeric", "integer", "factor", "logical", "character"))){
    stop( paste0( function_name, ": I can only handle: numeric, integer, factor, logical, character columns. 
                  Please provide them in one of those format."))
  }
  
  ## Initialization
  # Make an nice list of functions
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
    if(is.null(names(functions))){
      names(functions) <- listNames[! listNames %in% c("c", "list")]
    }
    # If some functions still have no name, put fun1, fun2...
    if(any(names(functions) == "")){
      names(functions)[names(functions) == ""] <- paste0("fun", 1:sum(names(functions) == ""))
    }
    
    functions <- true.aggFunction(functions, function_name)
  }
  else{
    functions <- c(mean = mean, min = min, max = max, sd = sd)
  }
  
  if (! is.null(args[["name_separator"]])){
    name_separator <- args[["name_separator"]]
  }
  else{
    name_separator <- "."
  }
  
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
      start_time = proc.time()
    }
    for (col in colnames(dataSet)[colnames(dataSet) != key]){ # we don't aggregate the key!
      data_sample <- dataSet[, c(key, col), with = FALSE] # To save some RAM
      result_tmp <- aggregateAcolumn(data_sample, col, key, uniqueN_byCol, name_separator, functions)
      result <- merge(result, result_tmp, by = key, all.x = TRUE)
      if (verbose){
        setPB(pb, col)
      }
    }
    if (verbose){
      close(pb); rm(pb); gc(verbose = FALSE)
      printl(function_name, ": ", ncol(result), " columns have been constructed. It took ", 
             round((proc.time() - start_time)[[3]], 2), "seconds. ")
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
# @
# @export # Before exporting this function should be improved!
aggregateAcolumn <- function(dataSet, col, key, uniqueN_byCol, name_separator = ".", 
                             functions, ...){
  ## Environement
  function_name = "aggregateAcolumn"
  ## Sanity check
  dataSet <- checkAndReturnDataTable(dataSet)
  is.col(dataSet, cols = c(key, col), function_name = function_name)
  
  ## Initialization
  maxNbValuePerKey <- max(dataSet[, uniqueN(get(col)), by = key][, -key, with = FALSE])
  
  ## Computation 
  if (maxNbValuePerKey > 1 ){
    result_tmp <- unique(dataSet[, key, with = FALSE])
    ## Aggregation of numerics
    if (any(class(dataSet[[col]]) %in% c("numeric", "integer"))){ 
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
    if (any(class(dataSet[[col]]) %in% c("character", "factor"))){
      if (uniqueN_byCol[col] < 53){ # 53 is finger in the nose...
        result_tmp <- dcast.data.table(dataSet[, c(key, col), with = FALSE], 
                                      formula = paste(key, col, sep = "~"), 
                                      fun.aggregate = length,
                                      value.var = col
                                      )
        setnames(result_tmp, colnames(result_tmp)[-1], 
                 paste(col, colnames(result_tmp)[-1], sep = name_separator))
      }
      if (uniqueN_byCol[col] >= 53){
        result_tmp <- dataSet[, c(key, col), with = FALSE][, .N, by = key]
        setnames(result_tmp, c(key, paste("nbr", col, sep = name_separator)))
      }
    }
    ## Aggregation of logicals
    if (any(class(dataSet[[col]]) %in% c("logical"))){
      result_tmp <- dataSet[, sum(get(col)), by = key]
      setnames(result_tmp, c(key, paste("nbr", col, sep = name_separator)))
    }
    
  }
  if (maxNbValuePerKey == 1){
	# Only one different value by key: we put the value one time by key.
    result_tmp <- dataSet[!duplicated(dataSet), ]
  }
  
  ## Wrapp up
  return(result_tmp)
}