#' Recode factor
#' 
#' Recode factors into 3 new columns: \cr
#' \itemize{
#' \item was the value not NA, "NA", ""
#' \item how often this value occures
#' \item the order of the value (ex: M/F => 2/1 because F comes before M in alphabet)
#' }
#' @param dataSet Matrix, data.frame or data.table
#' @param cols a list of factor colnames of dataSet (or just one) to transform into dates. \cr
#' To transform all factors, set it to "auto"
#' @param verbose should the function log (logical, default to TRUE)
#' @param drop_cols should \code{cols} be dropped after generation (logical, default to FALSE)
#' @param ... Other arguments such as \code{name_separator} to separate words in new coluimns names
#' (character, default to ".")
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
generateFromFactor <- function(dataSet, cols, verbose = TRUE, drop_cols = FALSE, ...){
  ## Working environement 
  function_name <- "generateFromFactor"
  
  ## Sanity check
  dataSet <- checkAndReturnDataTable(dataSet)
  if (all(cols == "auto")){
    cols = names(dataSet)[sapply(dataSet, is.factor)]  
  }
  cols <- real_cols(cols, names(dataSet), function_name = function_name)
  is.verbose(verbose)
  n_transformed <- length(cols)
  start_time <- proc.time()
  ## Initialization
  args <- list(...)
  if (! is.null(args[["name_separator"]])){
    name_separator <- args[["name_separator"]]
  }
  else{
    name_separator <- "."
  }
  ## Computation
  for (col in cols){
    if (! is.factor(dataSet[[col]])){
      warning(paste0(function_name, ": ", col, " isn't a factor, i do nothing."))
      next()
    }
    
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
    if (isTRUE(drop_cols)){
      dataSet[, c(col) := NULL]
    }
  }
  if (verbose){
    printl(function_name, ": it took me: ", round((proc.time() - start_time)[[3]], 2), 
           "s to transform ", n_transformed, " factor columns into, ", 3 * n_transformed, " new columns.")
  }
  ## Wrapp-up
  return(dataSet)
}




