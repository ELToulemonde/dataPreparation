#' Unfactor factor with too many values
#' 
#' To unfactorize all columns that have more than a given amount of various values. This
#'  function will be usefull after using some reading functions that put every string as factor.
#' @param dataSet Matrix, data.frame or data.table
#' @param n_unfactor Number of max element in a factor (numeric, default to 53)
#' @param verbose Should the algorithm talk? (logical, default to TRUE)
#' @details 
#' If a factor has (strictly) more than \code{n_unfactor} values it is unfactored. \cr
#' It is recommended to use \code{\link{findAndTransformNumerics}} and \code{\link{findAndTransformDates}} after this function.\cr
#' If \code{n_unfactor} is set to -1, nothing will be performed. \cr
#' If there are a lot of column that have been transformed, you might want to look at the 
#' documentation of your data reader in order to stop transforming everything into a factor.
#' @return Same dataSet (as a data.table) with less factor columns.
#' @examples 
#' # Let's build a dataSet
#' dataSet <- data.frame(true_factor = factor(rep(c(1,2), 13)),
#'                       false_factor = factor(LETTERS))
#'                       
#' # Let's un factorize all factor that have more than 5 different values
#' dataSet <- unFactor(dataSet, n_unfactor = 5)
#' sapply(dataSet, class)
#' # Let's un factorize all factor that have more than 5 different values
#' dataSet <- unFactor(dataSet, n_unfactor = 0)
#' sapply(dataSet, class)
#' 
#' @import data.table
#' @export
unFactor <- function(dataSet, n_unfactor = 53, verbose = TRUE){
  ## Working environement
  function_name <- "unFactor"
  
  ## Sanity check
  if (! is.numeric(n_unfactor)){
    stop(paste0( function_name, ": n_unfactor should be a numeric, you provided a ", class(n_unfactor)))
  }
  else{
    # To be safe
    n_unfactor <- round(n_unfactor) 
  }
  if (n_unfactor == -1){
    return(dataSet)
  }
  is.verbose(verbose)
  checkAndReturnDataTable(dataSet = dataSet)
  
  ## Initialization
  if (verbose){ 
    pb <- initPB(function_name, names(dataSet))
    printl(function_name, ": I will identify variable that are factor but shouldn't be.")
  }
  count <- 0
  start_time <- proc.time()
  
  ## Computation
  for (col in names(dataSet)){
    if (is.factor(dataSet[[col]]) & length(levels(dataSet[[col]])) > n_unfactor){
      if (verbose){
        printl(function_name, ": I unfactor ", col, ".")
      }
      set(dataSet, NULL, col, levels(dataSet[[col]])[dataSet[[col]]])
      count <- count + 1
    }
    
    if (verbose){
      setPB(pb, col)
    }
  }
  if (verbose){ 
    close(pb); rm(pb); gc(verbose = FALSE)
    printl(function_name, ": It took me ", round((proc.time() - start_time)[[3]], 2),
           "s to unfactor ", count, " column(s).")
  }
  
  ## Wrapp-up
  return(dataSet)
}

