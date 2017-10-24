###################################################################################################
############################### findNFirstNonNull #################################################
###################################################################################################
## Description
# Dichotomic search of non-null elements
# Search from 1 to N then N+1 to 2*N then 2*N+1 to 4*N...
findNFirstNonNull <- function(dataSet, N){
  ## Working environment
  
  ## Initialization
  if (length(dataSet) <= N){
    return(dataSet[!is.null(dataSet) & !is.na(dataSet) & dataSet != ""])
  }
  maxMultiple <- round(log(length(dataSet)) / log(10) + 1)
  result <- NULL
  
  ## Computation
  for ( mult in 1:maxMultiple){
    I <- (10 ^ (mult - 1)):min(10 ^ mult - 1, nrow(dataSet))
    data_sample <- dataSet[I]
    
    data_sample <- data_sample[!is.null(data_sample) & !is.na(data_sample) & data_sample != ""]
    
    if (length(data_sample) > 0 ){
      result <- c(result, data_sample[1:min(N - length(result), length(data_sample))])
    }
    
    
    ## If we found enough elements, we stop
    if (length(result) == N){
      return(result)
    }
  }
  
  ## Wrapp-up
  return(result)
}

###################################################################################################
########################################### checkAndReturnDataTable ###############################
###################################################################################################
# Description: Verify that the provided dataSet set is indeed a dataSet set
# @param dataSet an object. We will check that it is a dataSet set
# @param name name of the object passed in order to make an understandable log
#' @import data.table
checkAndReturnDataTable <- function(dataSet, name = "dataSet"){
  if (!any( class(dataSet) %in% c("data.table", "data.frame", "matrix"))){
    stop(paste(name, "should be a data.table, a data.frame or a matrix"))
  }
  if (nrow(dataSet) < 1){
    stop(paste(name, "should have at least have 1 line"))
  }
  if (ncol(dataSet) < 1){
    stop(paste(name, "should have at least have 1 column"))
  }
  
  if (! "data.table" %in% class(dataSet)){
    if (class(dataSet) == "data.frame"){
      setDT(dataSet)
    }
    else{
      dataSet <- as.data.table(dataSet)
    }
  }
  ## Wrapp-up
  return(dataSet)
}


###################################################################################################
########################################## Control verbose ########################################
###################################################################################################

is.verbose <- function(verbose, function_name = "is.verbose"){
  if (! is.logical(verbose)){
    stop(function_name, " verbose should be logical (TRUE or FALSE).")
  }
}
is.verbose_levels <- function(verbose, function_name = "is.verbose", max_level = 2){
  if (! is.logical(verbose) & ! verbose %in% 1:max_level){
    stop(function_name, " verbose should be logical (TRUE or FALSE) or an integer lower than ", max_level, ".")
  }
}

###################################################################################################
########################################## check if col is in dataset #############################
###################################################################################################
# Check if a column or a list of column is in a data.table
is.col <- function(dataSet, cols = NULL, ...){
  ## Sanity check
  if (! any(class(dataSet) %in% c("data.table", "data.frame", "matrix"))){
    stop("is.col: dataSet should be a data.table, data.frame or matrix")
  }
  
  ## Initialization
  # Arguments for log
  args <- list(...)
  if (length(args) > 0){
    if (!is.null(args[["function_name"]])){
      function_name <- args[["function_name"]]
    }
  }
  else{
    function_name <- "is.col"
  }
  ## Computation
  for (col in cols){
    if (!col %in% colnames(dataSet)){
      stop(paste(function_name,": ", col, " should be column of dataSet"))
    }
  }
}

# Reduce list of cols to only cols in dataSet set
# Reduce columns that aren't of the wanted type
# Handle "auto" value
# @param dataSet Matrix, data.frame or data.table (with only numeric, integer, factor, logical, character columns).
# @param cols list of column(s) name(s) of dataSet to control. (Default to auto, all cols that match types)
# @param function_name name of the function where it's called from. For LOG. (Character, default to "real_cols")
# @param types types of wanted columns.
real_cols <- function(dataSet, cols, function_name = "real_cols", types = NULL){
  ## If NULL cols
  if (is.null(cols) || length(cols) == 0){ # length cols is for the case where cols == character(0)
	return(NULL)
  }
  
  ## If auto cols
  if (all(cols == "auto")){
    cols <- colnames(dataSet)
    was_auto <- TRUE
  }
  else{
    was_auto <- FALSE
  }
  
  # Filter cols that doesn't exist
  listOfIsError <- ! cols %in% colnames(dataSet)
  if (sum(listOfIsError) > 0){
    printl(function_name, ": ", print(cols[listOfIsError], collapse = ", "), 
           " aren\'t columns of the table, i do nothing for those variables")
    cols <- cols[!listOfIsError] # Reduce list of col
  }
  rm(listOfIsError)
  
  # Filter cols that aren't of the right type
  if (! is.null(types)){
    if (all(types == "date")){
      listOfIsError <- ! cols %in% colnames(dataSet)[sapply(dataSet, is.date)]
    }
    else if(all(types == "numeric")){
      listOfIsError <- ! cols %in% colnames(dataSet)[sapply(dataSet, is.numeric)]
    }
    else{
      listOfIsError <- ! cols %in% colnames(dataSet)[sapply(dataSet, class) %in% types]  
    }
    if (sum(listOfIsError) > 0){
      if (! was_auto){
        printl(function_name, ": ", print(cols[listOfIsError], collapse = ", "), 
               " aren\'t columns of types ", paste(types, collapse = " or ")," i do nothing for those variables.")  
      }
      cols <- cols[!listOfIsError] # Reduce list of col
    }
    rm(listOfIsError)
  }
  
  ## Wrapp-up
  return(cols)
}





###################################################################################################
########################################### getPossibleSeparators #################################
###################################################################################################
## Separators are used in multiple functions, so i put them here!
getPossibleSeparators <- function(){
  listOfPossibleSeparator <- c(",", "/", "-", "_", ":")
  
  return(paste(listOfPossibleSeparator, collapse = "|"))
}


###################################################################################################
############################### Print list ########################################################
###################################################################################################
# To stop using print(paste())
printl <- function(...){
  args <- list(...)
  
  toPrint <- paste(args, collapse = "")
  print(toPrint)
}

## Super progress bar
# Using progress and storing some info
# Use when you build a progress bar for colnames
#' @importFrom progress progress_bar
initPB <- function(function_name, cols_names){
  if (length(cols_names) == 0){
    # No prossessing to do, so no progress bar
    return(NULL)
  }
  pb <- progress_bar$new(
    format = paste0("   ", function_name, " [:bar] :percent in :elapsed \r"),
    total = length(cols_names), clear = FALSE, width= 60)
  return(pb)
}
setPB <- function(pb, col){
  pb$tick()
}

###################################################################################################
############################### Control number of rows ############################################
###################################################################################################
# Control that you don't want to check an absurd number of rows
#
# @param dataSet matrix, data.frame or data.table
# @param nb_rows number of rows you want to check (integer)
# @param function_name to log the name of the function which is calling this one (character)
# @param variable_name name of the variable you are controling (character)
# @return nb_rows might have been changed to be conform
# @details 
# If nb_rows is greater than the numer of line of dataSet, it will be set to nrow(dataSet). 
# If nb_rows is loawer than 1, it will be set to min(30, nrow(dataSet)). 
# If nb_rows is a float it will be rounded
control_nb_rows <- function(dataSet, nb_rows, function_name = "", variable_name = "nb_rows"){
  ## Sanity check
  if (! is.numeric(nb_rows)){
    stop(paste0(function_name,": ", variable_name," should be a numeric."))
  }
  dataSet <- checkAndReturnDataTable(dataSet)
  
  ## Initialization
  nb_rows <- round(nb_rows)
  
  ## Computation
  if (nb_rows > nrow(dataSet)){
    nb_rows <- nrow(dataSet)
    warning(paste0(function_name,": You want to check more rows than there are in dataSet, I set ", 
                   variable_name," to ", nb_rows, "."))
  }
  if (nb_rows < 1){
    nb_rows <- min(30, nrow(dataSet))
    warning(paste0(function_name, 
                   ": You want to check at least a few rows than there are in dataSet, I set ", 
                   variable_name," to ", nb_rows, "."))
  } 
  
  ## Wrapp-up
  return(nb_rows)
}


###################################################################################################
############################### Return true aggregation functions #################################
###################################################################################################
# power <- function(x){sum(x^2}
# Ex: true.aggFunction(c(power = power, sqrt = sqrt))
true.aggFunction <- function(functions, function_name = "true.aggFunction "){
  for(fun in names(functions)){
    ## Check it
    # check type
    if (!is.function(functions[[fun]])){
      warning(paste0(function_name, ": ", fun, " is not a function, it wont be used."))
      functions <- functions[names(functions) != fun]
    }
    else{
      # check aggregation
      if (length(functions[[fun]](1:3)) != 1){
        warning(paste0(function_name, ": ", fun, " is not an aggegaration function, it wont be used. An aggregation function is a function that for multiple input return only one, exemple: sum."))
        functions <- functions[names(functions) != fun]
      }
    }
  }
  # Wrapp-up
  return(functions)
}





# Make if a function
#
# Taking a constant and transforming it into a function. If it's a function don't do anything. \cr
# Control that tehe function is giving the wanted type of input
# @param object object to be transformed and control
# @param function_name for log, from where do you call it (character, default to "function.maker")
# @param object_name for log, object name when called (character)
# @param type what type of output is expected for built function (character, numeric or logical)
# @return A function
function.maker <- function(object, type, function_name = "function.maker",  object_name = "object"){
  built_function <- NULL
  
  # If it's a constant
  if (any(class(object) %in% c("numeric", "integer", "factor", "logical", "date", "character"))){
    built_function <- function(...){return(object)}
  }
  if (is.function(object)){
    built_function <- object
  }
  
  
  if (!is.null(built_function)){
    if (type == "numeric"){
      if (length(built_function(1:3)) == 1){
        if (!is.numeric(built_function(1:3))){ 
          stop(paste0(function_name, ": ", object_name, " should be or should return a numeric."))
        }
        if (is.na(built_function(c(1, NA)))){
          warning(paste0(function_name, ": ", object_name, " is not handling NAs, it won't do anything."))
        }
        return(built_function)  
      }
    }
    
    if (type == "logical"){
      if (length(built_function(c(TRUE, FALSE))) == 1){
        if (!is.logical(built_function(c(TRUE, FALSE)))){ 
          stop(paste0(function_name, ": ", object_name, " should be or should return a logical."))
        }
        if (is.na(built_function(c(TRUE, NA)))){
          warning(paste0(function_name, ": ", object_name, " is not handling NAs, it won't do anything."))
        }
        return(built_function)  
      }
    }
    if (type == "character"){
      if (length(built_function(c("a", "b"))) == 1){
        if (!is.character(built_function(c("a", "b")))){ 
          stop(paste0(function_name, ": ", object_name, " should be or should return a character."))
        }
        if (is.na(built_function(c("a", NA)))){
          warning(paste0(function_name, ": ", object_name, " is not handling NAs, it won't do anything."))
        }
        return(built_function)  
      }
    }
    stop(paste0(object_name, ": is in a shape that isn't handled, please provide constant or aggregation function."))
  }
  else{
    stop(paste0(object_name, ": is in a shape that isn't handled, please provide constant or aggregation function."))
  }
}



#################################################################################################################
##### make new col name #########################################################################################
#################################################################################################################
make_new_col_name <- function(new_col, col_names){
  ## Working environement
  function_name <- "make_new_col_name"
  
  ## Sanit check
  if (! is.character(new_col) || ! is.character(col_names)){
    stop(paste0(function_name, ": new_col and col_names should be character."))
  }  
  
  ## Initialization
  new_col <- gsub("[[:punct:]]", ".", new_col) # replace special characters
  if (! new_col %in% col_names){
    return(new_col)
  }
  i <- 1
  while (paste0(new_col, i) %in% col_names){
    i <- i + 1
  }
  return(paste0(new_col, i))
}


#################################################################################################################
############################# build name separator ##############################################################
#################################################################################################################
build_name_separator <- function(args){
  name_separator <- "."
  if (! is.null(args[["name_separator"]])){
    if (is.character(args[["name_separator"]]) & length(args[["name_separator"]]) == 1){
      name_separator <- args[["name_separator"]]
    }
    else{
      stop("name_separator should be a character.")
    }
  }
  return(name_separator)
}


#################################################################################################################
############################# build factor_date_type ############################################################
#################################################################################################################
build_factor_date_type <- function(args){
  factor_date_type <- "yearmonth"
  if (! is.null(args[["factor_date_type"]])){
    if (is.character(args[["factor_date_type"]]) & length(args[["factor_date_type"]]) == 1){
      name_separator <- args[["factor_date_type"]]
    }
    else{
      stop("factor_date_type should be a character.")
    }
  }
  return(factor_date_type)
}

