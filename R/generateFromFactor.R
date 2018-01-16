#' Recode factor
#' 
#' Recode factors into 3 new columns:
#' \itemize{
#' \item was the value not NA, "NA", "",
#' \item how often this value occures,
#' \item the order of the value (ex: M/F => 2/1 because F comes before M in alphabet).
#' }
#' @param dataSet Matrix, data.frame or data.table
#' @param cols list of character column(s) name(s) of dataSet to transform. To transform all 
#' factors, set it to "auto". (character, default to "auto")
#' @param verbose Should the function log (logical, default to TRUE)
#' @param drop Should \code{cols} be dropped after generation (logical, default to FALSE)
#' @param ... Other arguments such as \code{name_separator} to separate words in new columns names
#' (character, default to ".")
#' @return \code{dataSet} with new columns. \code{dataSet} is edited by \strong{reference}.
#' @examples 
#' # Load data set
#' data(messy_adult)
#' 
#' # transform column "type_employer"
#' messy_adult <- generateFromFactor(messy_adult, cols = "type_employer")
#' head(messy_adult)
#' 
#' # To transform all factor columns:
#' messy_adult <- generateFromFactor(messy_adult, cols = "auto")
#' @import data.table
#' @export
generateFromFactor <- function(dataSet, cols, verbose = TRUE, drop = FALSE, ...){
  ## Working environement 
  function_name <- "generateFromFactor"
  
  ## Sanity check
  dataSet <- checkAndReturnDataTable(dataSet)
  cols <- real_cols(dataSet, cols, function_name, types = "factor")
  is.verbose(verbose)
  
  ## Initialization
  start_time <- proc.time()
  args <- list(...)
  name_separator <- build_name_separator(args)
  
  ## Computation
  for (col in cols){
    # has value 
    new_col <- paste0(col, name_separator, "notnull")
    new_col <- make_new_col_name(new_col, names(dataSet))
    dataSet[, c(new_col) := levels(dataSet[[col]])[col] %in% c(NA, "NA", "")]
    
    # recode with nb of occurence of value
    new_col <- paste0(col, name_separator, "num")
    new_col <- make_new_col_name(new_col, names(dataSet))
    dataSet[, c(new_col) := .N, by = col]
    
    # recode with order of value
    new_col <- paste0(col, name_separator, "order")
    new_col <- make_new_col_name(new_col, names(dataSet))
    col_levels <- levels(dataSet[[col]])
    levels_order <- order(col_levels)
    dataSet[, c(new_col) := levels_order[col] ]
    
    # if asked drop col
    if (isTRUE(drop)){
      dataSet[, c(col) := NULL]
    }
  }
  if (verbose){
    printl(function_name, ": it took me: ", round( (proc.time() - start_time)[[3]], 2), 
           "s to transform ", length(cols), " factor columns into, ", 3 * length(cols), " new columns.")
  }
  ## Wrapp-up
  return(dataSet)
}


## one_hot_encoder
# ----------------
#' One hot encoder
#' 
#' Transform factor column into 0/1 columns with one column per values of the column.
#' @param dataSet Matrix, data.frame or data.table
#' @param encoding Result of funcion \code{\link{build_encoding}}, (list, default to NULL). \cr
#' To perform the same encoding on train and test, it is recommended to compute \code{\link{build_encoding}}
#' before. If it is kept to NULL, build_encoding will be called.
#' @param drop Should \code{cols} be dropped after generation (logical, default to FALSE)
#' @param verbose Should the function log (logical, default to TRUE) 
#' @return \code{dataSet} edited by \strong{reference} with new columns. 
#' @details If you don't want to edit your data set consider sending \code{copy(dataSet)} as an input.\cr
#' Please \strong{be carefull} using this function, it will generate as many columns as there different values 
#' in your column and might use a lot of RAM.
#' @examples
#' data(messy_adult)
#' 
#' # Compute encoding
#' encoding <- build_encoding(messy_adult, cols = c("marital", "occupation"), verbose = TRUE)
#' 
#' # Apply it
#' messy_adult <- one_hot_encoder(messy_adult, encoding = encoding, drop = TRUE)
#' 
#' # Apply same encoding to adult
#' data(adult)
#' adult <- one_hot_encoder(adult, encoding = encoding, drop = TRUE)
#' @export
#' @import data.table
one_hot_encoder <- function(dataSet, encoding = NULL, verbose = TRUE, drop = FALSE){
  ## Working environement
  function_name <- "one_hot_encoder"
  
  ## Sanity check
  dataSet <- checkAndReturnDataTable(dataSet)
  is.verbose(verbose)
  
  ## Initialization
  # Transform char into factor
  if (is.null(encoding)){
    if (verbose){
	  printl(function_name, ": Since you didn't profvide encoding, I compute them with build_encoding.")
    }
    encoding <- build_encoding(dataSet, cols = "auto", verbose = verbose)
  }
  cols <- names(encoding)
  if (verbose){
    printl(function_name, ": I will one hot encode some columns.")
    pb <- initPB(function_name, cols)
  }
  start_time <- proc.time()
  
  ## Computation
  for (col in cols){
    # Log
    if (verbose){
      printl(function_name, ": I am doing column: ", col)
    }
    # Build columns with 0 value (it save time to pre-set the columns)
    new_cols <- encoding[[col]]$new_cols
    dataSet[, (new_cols) := 0]
    # Set the write value
    for (i in 1:length(new_cols)){
      set(dataSet, NULL, new_cols[i], as.integer(dataSet[[col]] == encoding[[col]]$values[i]))
    }
    
    # drop col if asked
    if (isTRUE(drop)){
      dataSet[, c(col) := NULL]
    }
    # Update progress bar
    if (verbose){
      setPB(pb, col)
    }
  }
  
  if (verbose){ 
    gc(verbose = FALSE)
    printl(function_name, ": It took me ", round( (proc.time() - start_time)[[3]], 2), 
           "s to transform ", length(cols), " column(s).")
  }
  ## Wrapp-up
  return(dataSet)
}

#' Compute encoding
#' 
#' Build a list of one hot encoding for each \code{cols}.
#' @param dataSet Matrix, data.frame or data.table
#' @param cols List of numeric column(s) name(s) of dataSet to transform. To transform all 
#' characters, set it to "auto". (character, default to "auto")
#' @param verbose Should the algorithm talk? (Logical, default to TRUE)
#' @param ... Other arguments such as \code{name_separator} to separate words in new columns names
#' (character, default to ".")
#' @return A list where each element name is a column name of data set and each element new_cols 
#' and values the new columns that will be built during encoding.
#' @examples 
#' # Get a data set
#' data(adult)
#' encoding <- build_encoding(adult, cols = "auto", verbose = TRUE)
#' 
#' print(encoding)
#' @export
build_encoding <- function(dataSet, cols = "auto", verbose = TRUE, ...){
  ## Working environement
  function_name <- "build_encoding"
  
  ## Sanity check
  dataSet <- checkAndReturnDataTable(dataSet)
  cols <- real_cols(dataSet, cols, function_name, types = c("factor", "character"))
  is.verbose(verbose)
  
  ## Initialization
  if (verbose){
    pb <- initPB(function_name, cols)
    printl(function_name, ": I will compute encoding on ", length(cols), " character and factor columns.")
    start_time <- proc.time()
  }
  
  # Retrive arg
  args <- list(...)
  name_separator <- build_name_separator(args)
  encoder = list()
  ## Computation
  for (col in cols){
    # Build columns with 0 value (it save time to pre-set the columns)
    if (is.factor(dataSet[[col]])){
      values = levels(dataSet[[col]])
    }
    else{
      values = unique(dataSet[[col]])
    }
    new_cols <- paste0(col, name_separator, values)
    new_cols <- sapply(new_cols, function(x)make_new_col_name(x, names(dataSet)))
    
    # Set the write value
    encoder[[col]] = list(new_cols = new_cols, values = values)
    
    # Update progress bar
    if (verbose){
      setPB(pb, col)  
    }
  }
  if (verbose){
    printl(function_name, ": it took me: ", round( (proc.time() - start_time)[[3]], 2), 
           "s to compute encoding for ", length(cols), " character and factor columns.")
  }
  ## Wrapp-up
  return(encoder)
}

