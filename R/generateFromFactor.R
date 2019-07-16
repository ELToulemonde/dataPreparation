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
generateFromFactor <- function(dataSet, cols = "auto", verbose = TRUE, drop = FALSE, ...){
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
	set(dataSet, NULL, new_col, levels(dataSet[[col]])[col] %in% c(NA, "NA", ""))
    
    # recode with nb of occurence of value
    new_col <- paste0(col, name_separator, "num")
    new_col <- make_new_col_name(new_col, names(dataSet))
    dataSet[, c(new_col) := .N, by = col]
    
    # recode with order of value
    new_col <- paste0(col, name_separator, "order")
    new_col <- make_new_col_name(new_col, names(dataSet))
    col_levels <- levels(dataSet[[col]])
    levels_order <- order(col_levels)
    set(dataSet, NULL, new_col, levels_order[col] )
    # if asked drop col
    if (isTRUE(drop)){
	  set(dataSet, NULL, col, NULL)
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
#' @param type What class of columns is expected? "integer" (0L/1L), "numeric" (0/1), or "logical" (TRUE/FALSE), 
#' (character, default to "integer")
#' @param drop Should \code{cols} be dropped after generation (logical, default to FALSE)
#' @param verbose Should the function log (logical, default to TRUE) 
#' @return \code{dataSet} edited by \strong{reference} with new columns. 
#' @details If you don't want to edit your data set consider sending \code{copy(dataSet)} as an input.\cr
#' Please \strong{be carefull} using this function, it will generate as many columns as there different values 
#' in your column and might use a lot of RAM. To be safe, you can use parameter \code{min_frequency} in \code{\link{build_encoding}}.
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
#' 
#' # To have encoding as logical (TRUE/FALSE), pass it in type argument
#' data(adult)
#' adult <- one_hot_encoder(adult, encoding = encoding, type = "logical", drop = TRUE)
#' @export
#' @import data.table
one_hot_encoder <- function(dataSet, encoding = NULL, type = "integer", verbose = TRUE, drop = FALSE){
  ## Working environement
  function_name <- "one_hot_encoder"
  
  ## Sanity check
  dataSet <- checkAndReturnDataTable(dataSet)
  is.verbose(verbose)
  if (! type %in% c("integer", "logical", "numeric")){
    stop(paste0(function_name, ": type should either be 'integer', 'numeric' or 'logical.'"))
  }
  as_type <- get(paste0("as.", type)) # Build a_type function to transform result type
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
    # Set the write value
    for (i in 1:length(new_cols)){
      set(dataSet, NULL, new_cols[i], as_type(dataSet[[col]] == encoding[[col]]$values[i]))
    }
    
    # drop col if asked
    if (isTRUE(drop)){
	  set(dataSet, NULL, col, NULL)
    }
    # Update progress bar
    if (verbose){
      setPB(pb, col)
    }
  }
  
  if (verbose){ 
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
#' @param min_frequency The minimal share of lines that a category should represent (numeric, 
#' between 0 and 1, default to 0)
#' @param ... Other arguments such as \code{name_separator} to separate words in new columns names
#' (character, default to ".")
#' @details 
#' To avoid creating really large sparce matrices, one can use  param \code{min_frequency} to be
#'  sure that only most representative values will be used to create a new column (and not 
#'  outlayers or mistakes in data). \cr
#'  Setting \code{min_frequency} to something gretter than 0 may cause the function to be slower 
#'  (especially for large dataSet).
#' @return A list where each element name is a column name of data set and each element new_cols 
#' and values the new columns that will be built during encoding.
#' @examples 
#' # Get a data set
#' data(adult)
#' encoding <- build_encoding(adult, cols = "auto", verbose = TRUE)
#' 
#' print(encoding)
#' 
#' # To limit the number of generated columns, one can use min_frequency parameter:
#' build_encoding(adult, cols = "auto", verbose = TRUE, min_frequency = 0.1)
#' # Set to 0.1, it will create columns only for values that are present 10% of the time.
#' @import data.table
#' @export
build_encoding <- function(dataSet, cols = "auto", verbose = TRUE, min_frequency = 0, ...){
  ## Working environement
  function_name <- "build_encoding"
  
  ## Sanity check
  dataSet <- checkAndReturnDataTable(dataSet)
  cols <- real_cols(dataSet, cols, function_name, types = c("factor", "character"))
  is.verbose(verbose)
  is.share(min_frequency, object_name = "min_frequency", function_name = function_name)
  
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
      values <- levels(dataSet[[col]])
    }
    else{
      values <- unique(dataSet[[col]])
    }
    if (min_frequency > 0 ){
      frequency <- dataSet[, .N / nrow(dataSet), by = col]
      to_drop <- frequency[get("V1") < min_frequency, ][[col]] # V1 is created in previous line (= .N / nrow(dataSet)). Get to avoid unwanted devtools::check note.
      values <- setdiff(values, to_drop)
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

#' Target encode
#' 
#' Target encoding is the process of replacing a categorical value with the aggregation of the target variable. 
#' the target variable. \code{target_encode} is used to apply this transformations on a data set. 
#' Function \code{\link{build_target_encoding}} must be used first to compute aggregations.
#' @param dataSet Matrix, data.frame or data.table
#' @param target_encoding result of function \code{\link{build_target_encoding}} (list)
#' @param drop Should \code{col_to_encode} be dropped after generation (logical, default to FALSE)
#' @param verbose Should the algorithm talk? (Logical, default to TRUE)
#' @return \code{dataSet} with new cols of \code{target_encoding} merged to \code{dataSet} 
#' using \code{target_encoding} names as merging key. \code{dataSet} is edited by \strong{reference}.
#' @examples 
#' # Build a data set
#' require(data.table)
#' dataSet <- data.table(student = c("Marie", "Marie", "Pierre", "Louis", "Louis"), 
#'                       grades = c(1, 1, 2, 3, 4))
#' 
#' # Construct encoding
#' target_encoding <- build_target_encoding(dataSet, cols_to_encode = "student", 
#'                                          target_col = "grades", functions = c("mean", "sum"))
#' 
#' # Apply them
#' target_encode(dataSet, target_encoding = target_encoding)
#' @import data.table
#' @export
target_encode <- function(dataSet, target_encoding, drop = FALSE, verbose = TRUE){
  ## Working environement
  function_name <- "target_encode"
  
  ## Sanity check
  dataSet <- checkAndReturnDataTable(dataSet)
  cols_to_encode <- real_cols(dataSet, cols = names(target_encoding), function_name = function_name)
  is.verbose(verbose)
  
  ## Initialization
  if (verbose){ 
    pb <- initPB(function_name, names(dataSet))
    printl(function_name, ": Start to encode columns according to target.")
  }
  
  ## Computation
  for (col in cols_to_encode){
    target_encoding_this_col <- target_encoding[[col]]
    dataSet <- merge(dataSet, target_encoding_this_col, by = col, all.x = TRUE, sort = FALSE)  
    
    if (verbose){
      setPB(pb, col)
    }
  }
  
  
  # drop col if asked
  if (isTRUE(drop)){
    set(dataSet, NULL, cols_to_encode, NULL)
  }
  
  ## Wrapp-up
  return(dataSet)
}


#' Build target encoding
#' 
#' Target encoding is the process of replacing a categorical value with the aggregation of the  
#' target variable. \code{build_target_encoding} is used to compute aggregations.
#' @param dataSet Matrix, data.frame or data.table
#' @param cols_to_encode columns to aggregate according to (list)
#' @param target_col column to aggregate (character)
#' @param functions functions of aggregation (list or character, default to "mean")
#' @param verbose Should the algorithm talk? (Logical, default to TRUE)
#' @return A \code{list} of \code{\link{data.table}} a data.table for each \code{cols_to_encode}
#' each data.table containing a line by unique value of column and \code{len(functions) + 1} columns.
#' @examples 
#' # Build a data set
#' require(data.table)
#' dataSet <- data.table(student = c("Marie", "Marie", "Pierre", "Louis", "Louis"), 
#'                       grades = c(1, 1, 2, 3, 4))
#' 
#' # Perform target_encoding construction
#' build_target_encoding(dataSet, cols_to_encode = "student", target_col = "grades", 
#'                       functions = c("mean", "sum"))
#' @import data.table
#' @export 
build_target_encoding <- function(dataSet, cols_to_encode, target_col, functions = "mean", verbose = TRUE){
  ## Working environement
  function_name <- "build_target_encoding"
  
  ## Sanity check
  dataSet <- checkAndReturnDataTable(dataSet)
  cols_to_encode <- real_cols(dataSet = dataSet, cols = cols_to_encode, function_name = function_name)
  is.col(dataSet, cols = c(target_col), function_name = function_name)
  if (is.character(functions)){functions = c(functions)}
  functions <- is.agg_function(functions, function_name)
  is.verbose(verbose)
  
  ## Initialization
  result <- list()
  if (verbose){ 
    pb <- initPB(function_name, names(dataSet))
    printl(function_name, ": Start to compute encoding for target_encoding according to col: ",
           target_col, ".")
  }
  
  ## Computation
  for (col in cols_to_encode){
    result_this_col <- NULL
    result_tmp <- NULL
    for (fun in functions){
      new_col_name <- paste0(target_col, "_", fun, "_by_", col)
      code <- paste0("result_tmp <- dataSet[, .(", new_col_name, 
                     "= get(fun)(get(target_col))), by = c(col), with=TRUE]")
      try(eval(parse(text = code)))
      if (is.null(result_this_col)){
        result_this_col <- result_tmp
      }
      else{
        set(result_this_col, NULL, new_col_name, result_tmp[[new_col_name]])
      }
    }
    result[[col]] <- result_this_col
    
    if (verbose){
      setPB(pb, col)
    }
  }

  
  ## Wrapp-up
  return(result)
}

