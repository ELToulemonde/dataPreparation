#' Recode character
#' 
#' Recode character into 3 new columns: \cr
#' \itemize{
#' \item was the value not NA, "NA", "",
#' \item how often this value occures,
#' \item the order of the value (ex: M/F => 2/1 because F comes before M in alphabet).
#' }
#' @param dataSet Matrix, data.frame or data.table
#' @param cols List of character column(s) name(s) of dataSet to transform. To transform all 
#' characters, set it to "auto". (character, default to "auto")
#' @param verbose Should the function log (logical, default to TRUE)
#' @param drop Should \code{cols} be dropped after generation (logical, default to FALSE)
#' @param ... Other arguments such as \code{name_separator} to separate words in new columns names
#' (character, default to ".")
#' @return \code{dataSet} with new columns. \code{dataSet} is edited by \strong{reference}.
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
generateFromCharacter <- function(dataSet, cols, verbose = TRUE, drop = FALSE, ...){
  ## Working environement 
  function_name <- "generateFromCharacter"
  
  ## Sanity check
  dataSet <- checkAndReturnDataTable(dataSet)
  cols <- real_cols(dataSet, cols, function_name, types = "character")
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
    if (isTRUE(drop)){
      dataSet[, c(col) := NULL]
    }
  }
  if (verbose){
    printl(function_name, ": it took me: ", round( (proc.time() - start_time)[[3]], 2), 
           "s to transform ", length(cols), " character columns into, ", 3 * length(cols), " new columns.")
  }
  ## Wrapp-up
  return(dataSet)
}




