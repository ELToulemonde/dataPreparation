#' Recode character
#' 
#' Recode character into 3 new columns: \cr
#' \itemize{
#' \item was the value not NA, "NA", ""
#' \item how often this value occures
#' \item the order of the value (ex: M/F => 2/1 because F comes before M in alphabet)
#' }
#' @param dataSet Matrix, data.frame or data.table
#' @param cols a list of character colnames of dataSet (or just one) to transform into dates. \cr
#' To transform all characters, set it to "auto"
#' @param verbose should the function log (logical, default to TRUE)
#' @param drop_cols should \code{cols} be dropped after generation (logical, default to FALSE)
#' @param ... Other arguments such as \code{name_separator} to separate words in new coluimns names
#' (character, default to ".")
#' @examples 
#' # Load data set
#' data(messy_adult)
#' messy_adult <- unFactor(messy_adult, verbose = FALSE) # un factor ugly factors
#' 
#' # transform column "mail"
#' messy_adult <- generateFromCharacter(messy_adult, cols = "mail")
#' head(messy_adult)
#' 
#' # To transform all characters columns:
#' messy_adult <- generateFromCharacter(messy_adult, cols = "auto")
#' @import data.table
#' @export
generateFromCharacter <- function(dataSet, cols, verbose = TRUE, drop_cols = FALSE, ...){
  ## Working environement 
  function_name <- "generateFromCharacter"
  
  ## Sanity check
  dataSet <- checkAndReturnDataTable(dataSet)
  if (all(cols == "auto")){
    cols = names(dataSet)[sapply(dataSet, is.character)]  
  }
  cols <- real_cols(cols, names(dataSet), function_name = function_name)
  is.verbose(verbose)
  n_transformed <- length(cols)
  start_time <- proc.time()
  ## Initialization
  args <- list(...)
  name_separator <- build_name_separator(args)
  
  ## Computation
  for (col in cols){
    if (! is.character(dataSet[[col]])){
      warning(paste0(function_name, ": ", col, " isn't a character, i do nothing."))
      n_transformed <- n_transformed - 1
      next()
    }
    
    # has value 
    new_col <- paste0(col, name_separator, "notnull")
    new_col <- make_new_col_name(new_col, names(dataSet))
    dataSet[, c(new_col) := col %in% c(NA, "NA", "")]
    
    # recode with nb of occurence of value
    new_col <- paste0(col, name_separator, "num")
    new_col <- make_new_col_name(new_col, names(dataSet))
    dataSet[, c(new_col) := .N, by = col]
    
    # recode with order of value
    new_col <- paste0(col, name_separator, "order")
    new_col <- make_new_col_name(new_col, names(dataSet))
    colorder <- names(dataSet) # store order to reset it
    unique_values <- sort(unique(dataSet[[col]])) # 
    unique_values <- data.table(col = unique_values, new_col = 1:length(unique_values))
    dataSet <- merge(dataSet, unique_values, by.x = col, by.y = "col")
    setnames(dataSet, "new_col", new_col) # set correct name
    setcolorder(dataSet, c(colorder, new_col)) # reset col order
    
    # if asked drop col
    if (isTRUE(drop_cols)){
      dataSet[, c(col) := NULL]
    }
  }
  if (verbose){
    printl(function_name, ": it took me: ", round((proc.time() - start_time)[[3]], 2), 
           "s to transform ", n_transformed, " character columns into, ", 3 * n_transformed, " new columns.")
  }
  ## Wrapp-up
  return(dataSet)
}




