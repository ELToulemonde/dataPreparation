#######################################################################################
############################### Fast filter function ##################################
#######################################################################################
#' Filtering useless variables
#'
#' Delete columns that are constant or in double in your dataSet set.
#' @param dataSet Matrix, data.frame or data.table
#' @param verbose Should the algorithm talk (logical, default to TRUE)
#' @param ... optional parameters to be passed to the function when called from another function
#' @return 
#' The same dataSet set but with fewer columns. Columns that are constant, in double, 
#' or bijection of anotger have been deleted.
#' @import data.table
#' @export
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
fastFilterVariables <- function(dataSet, verbose = TRUE, ...){
  ## Working environement
  
  ## Sanity check
  dataSet <- checkAndReturnDataTable(dataSet = dataSet, name = "DEBUG: see fastFilterVariables")
  
  ## Initalization
  # Arguments for log
  myArgs <- list(...)
  if (length(myArgs)>0){
    if (!is.null(myArgs[["function_name"]])){
      function_name <- myArgs[["function_name"]]
    }
    else{
      function_name <- "fastFilterVariables"
    }
    if (!is.null(myArgs[["dataName"]])){
      dataName <- myArgs[["dataName"]]
    }
    else{
      dataName <- "dataSet"
    }
  }
  else{
    function_name <- "fastFilterVariables"
    dataName <- "dataSet"
  }
  
  ## Computation
  # Delete constant in double
  if (verbose){
    printl(function_name, ": I check for constant columns")
  }
  listOfConstantCols <- whichAreConstant(dataSet, verbose = verbose)
  if (length(listOfConstantCols) > 0){
    if (verbose){
      printl(function_name, ": I delete", length(listOfConstantCols), "constant column(s) in", dataName)
    }
	dataSet[, (listOfConstantCols) := NULL]
	#drop_cols(dataSet, listOfConstantCols, function_name = function_name)
  }
  # Delete columns in double
  if (verbose){
    printl(function_name, ": I check for columns in double")
  }
  listOfDoubles <- whichAreInDouble(dataSet, verbose = verbose)
  if (length(listOfDoubles) > 0){
    if (verbose){
      printl(function_name, ": I delete", length(listOfDoubles), "column(s) that are in double in", dataName)
    }  
	dataSet[, (listOfDoubles) := NULL]
    #drop_cols(dataSet, listOfDoubles, function_name = function_name)
  }
  
  # Delete columns that are bijections
  if (verbose){
    printl(function_name, ": I check for columns that are bijections of another column")
  }
  listOfBijections <- whichAreBijection(dataSet, verbose = verbose)
  if (length(listOfBijections) > 0){
    if (verbose){
      printl(function_name, ": I delete", length(listOfBijections), 
             "column(s) that are bijections of another column in", dataName)
    }  
	dataSet[, (listOfBijections) := NULL]
    #drop_cols(dataSet, listOfBijections, function_name = function_name)
  }
  ## Wrapp up
  return(dataSet)
}


#######################################################################################
############################### Fast round ############################################
#######################################################################################
#' Fast round
#' 
#' Fast round of numeric in a data.table. Will only round numeric, so don't worry about characters. 
#' Also, it compute it column by column so your RAM is safe too.
#' @param dataSet matrix, data.frame or data.table
#' @param digits The number of digits after comma (numeric, default to 2)
#' @param verbose Should the algorithm talk? (logical, default to TRUE)
#' @details
#' It is performing round by reference on dataSet, column by column, only on numercial columns. 
#' So that it avoid to copy dataSet in RAM.
#' @return The same datasets but as a data.table and with numerics rounded
#' @examples
#' # First let's build a very large data.table with random numbers
#' M <- as.data.table(matrix(runif (3e4), ncol = 10))
#' 
#' M_rouded <- fastRound(M, 2)
#' # Lets add some character
#' M[, stringColumn := "a string"] 
#' 
#' # And use our function
#' M_rouded <- fastRound(M, 2)
#' # It still work :) and you don't have to worry about the string.
#' @export
fastRound <- function(dataSet, digits = 2, verbose = TRUE){
  ## Working environement
  function_name = "fastRound"
  
  ## Sanity check
  dataSet <- checkAndReturnDataTable(dataSet)
  if (!is.numeric(digits)){stop(paste0(function_name, ": digits should be an integer"))}
  
  ## Initialization
  digits <- round(digits, 0) # just to be safe
  
  ## Computation
  if (verbose){
    pb <- initPB(function_name, names(dataSet))
  }
  for (col in names(dataSet)){
    if (any(class(dataSet[[col]]) %in% c("numeric", "integer"))){
      set(dataSet, NULL, col, round(dataSet[[col]], digits))
    } 
    if (verbose){
      setPB(pb, col)
    }
  }
  if (verbose){
    close(pb); rm(pb)
  }
  
  ## Wrapp-up
  return(dataSet)
}



#######################################################################################
##################################### Handle NA #######################################
#######################################################################################
#' Handle NA values
#'
#' Function to handle NAs values depending on the class of the column
#' @param dataSet Matrix, data.frame or data.table
#' @param codeToSetNumeric NAs replacement in numeric column, (numeric or code in a string, default to 0)
#' @param codeToSetBooleans NAs replacement in logical column, (logical or code in a string, default to FALSE)
#' @param codeToSetCharacter NAs replacement in character column, (character or code in a string, default to '""')
#' @param verbose Should the algorithm talk (logical, default to TRUE)
#' @details 
#' To preserve RAM this function edit directly the dataSet set.  
#' For factor columns, it will add NA to list of values.
#' @return dataSet as a \code{\link{data.table}} with NAs handled
#' @examples
#' # Build a useful dataSet set for example
#' dataSet <-  data.table(numCol = c(1, 2, 3, NA),
#'                    charCol = c("", "a", NA, "c"),
#'                    booleanCol = c(TRUE, NA, FALSE, NA))
#'
#' # To set NAs to 0, FALSE and "" (respectively for numeric, logical, character)
#' dataSet <- fastHandleNa(dataSet)
#'
#' # In a numeric column to set NAs as "missing"
#' dataSet <-  data.table(numCol = c(1, 2, 3, NA),
#'                    charCol = c("", "a", NA, "c"),
#'                    booleanCol = c(TRUE, NA, FALSE, NA))
#' dataSet <- fastHandleNa(dataSet, codeToSetCharacter = '"missing"')
#' 
#' # In a numeric column, to set NAs to the minimum value of the column
#' dataSet <-  data.table(numCol = c(1, 2, 3, NA),
#'                    charCol = c("", "a", NA, "c"),
#'                    booleanCol = c(TRUE, NA, FALSE, NA))
#' dataSet <- fastHandleNa(dataSet, codeToSetNumeric = "min(dataSet[[col]], na.rm = TRUE)")
#'
#' @export
fastHandleNa <- function(dataSet, codeToSetNumeric = 0, codeToSetBooleans = FALSE, 
                         codeToSetCharacter = '""', verbose = TRUE){
  ## Working environement
  function_name = "fastHandleNa"
  
  ## Sanity check
  dataSet <- checkAndReturnDataTable(dataSet)
  # To do sanity check on code
  
  ## Initialization
  if (verbose){
    pb <- initPB(function_name, names(dataSet))
  }
  
  ## Computation
  for (col in names(dataSet)){ 
    if (any(class(dataSet[[col]]) %in% c("numeric", "integer"))){
      code <- paste0("set(dataSet, which(is.na(dataSet[[col]])), col, ", codeToSetNumeric, ")")
      if (verbose == "debug"){
        print(code)
      }
      try(eval(parse(text = code)))
    }
    if (any(class(dataSet[[col]]) %in% c("logical"))){
      code <- paste("set(dataSet, which(is.na(dataSet[[col]])), col, ", codeToSetBooleans, ")", sep = "")
      if (verbose == "debug"){
        print(code)
      }
      try(eval(parse(text = code)))
    }
    if (any(class(dataSet[[col]]) %in% c("character"))){
      code <- paste("set(dataSet, which(is.na(dataSet[[col]])), col, ", codeToSetCharacter, ")", sep = "")
      if (verbose == "debug"){
        print(code)
      }
      try(eval(parse(text = code)))
    }
    if (any(class(dataSet[[col]]) %in% c("factor"))){
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
  if (verbose){
    close(pb); rm(pb)
  }
  ## Wrapp-up
  return(dataSet)
}

#######################################################################################
############################### Fast is equal function ################################
#######################################################################################
#' Fast checks of equality
#' 
#' Performs quick check if two objects are equal 
#' @param object1 an element, a vector, a data.frame, a data.table
#' @param object2 an element, a vector, a data.frame, a data.table
#' @details 
#' This function is fast for very large vectors, data.frame and data.table. 
#' This function is also very robust; you can compare a lot of stuff without failing.
#' @return logical (TRUE or FALSE) if the two objects are equals.
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
#' dataSet <- data.table(messy_adult)
#' fastIsEqual(dataSet, dataSet)
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
  if (any(class(object1) %in% c("list", "data.table", "data.frame"))){
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
  if (any(class(object1) == "factor")){
    if (!(sum(levels(object1) %in% levels(object2)) == length(levels(object1)) & sum(levels(object2) %in% levels(object1)) == length(levels(object2)))){
      return(FALSE) # To-do Limitation: les levels vides
    }
  }
  
  # Comparaison for short object
  if (length(object1) <= 3){ # (length(object1) <= 3) => (maxPower == 0) =>  We check direcly for equality
    if (any(object1 != object2)){
      return(FALSE)  
    }
    else{
      return(TRUE)
    }
  }
  
  # Comparaison for long object
  maxPower <- round(log(length(object1))/log(10))
  for (i in 1:maxPower){
    I <- (10^(i - 1)):min(10^i - 1, length(object1) - 1)
    if (sum(object1[I] == object2[I], na.rm = TRUE) + sum(is.na(object1[I]) & is.na(object2[I])) != length(I)){
      return(FALSE)
    }
  }
  
  # If every test passed, it's true
  return(TRUE)
}









#######################################################################################
############################### Fast is bijection function ############################
#######################################################################################
fastIsBijection <- function(dataSet){
  ## Sanity check
  if (ncol(dataSet) != 2){
    stop("fastIsBijection: dataSet should be a data.table or a data.frame with 2 columns")
  }
  # Comparaison for long object
  nrows <- nrow(dataSet)
  maxPower <- round(log(nrows)/log(10))
  for (i in 1:maxPower){
    I <- (10^(i - 1)):min(10^i - 1,  nrows - 1)
    n1 <- uniqueN(dataSet[I, 1])
    n2 <- uniqueN(dataSet[I, 2])
    if (n1 != n2){
      return(FALSE)
    }
    temp_data <- dataSet[I,]
    temp_data <- temp_data[!duplicated(temp_data), ]
    
    if (nrow(temp_data) != n1){
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
# @retrun logical.
fastMaxNbElt <- function(object, max_n_values = 1){
  
  ## Initialization
  listOfUnique <- NULL
  maxPower <- floor(log(length(object)) / log(10)) + 1
  i <- 1
  isConstant <- TRUE
  
  ## Computation
  for (i in 1:maxPower){
    I=(10^(i - 1)):min(10^i - 1, length(object))
    listOfUnique <- unique( c( listOfUnique, unique( object[I])))
    if (length(listOfUnique) > max_n_values){
      return(FALSE)
    }
  }
  
  
  ## Wrapp-up
  # if every test passed, return TRUE
  return(TRUE)
}






