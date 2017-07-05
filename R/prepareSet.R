###################################################################################################
############################### prepareSet  #######################################################
###################################################################################################
#' Preparation pipeline
#' 
#' Full pipeline for preparing your dataSet set \cr
#' It will perform the following steps: \cr
#' - Correct set: id dates and numerics that are hiden in string \cr
#' - Transform set: compute differences between every date, if `key` is provided, 
#' will perform aggregate according to this key \cr
#' - Filter set: filter constant, in double or bijection variables. If `digits` is provided, 
#' will round numerics \cr
#' - Handle NA: will perform \code{\link{fastHandleNa}}) \cr
#' - Shape set: will put the result in asked shape (`finalForm`) with acceptable columns format.
#' @param dataSet Matrix, data.frame or data.table
#' @param finalForm "data.table" or "numerical_matrix" (default to data.table)
#' @param verbose Should the algorithm talk? (logical, default to TRUE)
#' @param ... additional parameters to thune pipeline (see details)
#' @details
#' Aditional arguments are available to thune pipeline: 
#' \itemize{
#'   \item \code{key} name of a column of dataSet according to which dataSet should be aggregated (character)
#'   \item \code{analysisDate} A date at which the dataSet should be aggregated 
#' (differences between every date and analysisDate will be computed) (Date)
#'   \item \code{digits} The number of digits after comma (optional, numeric, if set will perform \code{\link{fastRound}})
#'   \item \code{dateFormats} List of format of Dates in dataSet (list of characters)
#'   \item \code{name_separator} string to separate parts of new column names (string)
#' }
#' @return A data.table or a numerical matrix (according to finalForm)  and 
#' @export
#' @import data.table
#' @examples 
#' # Load ugly set
#' data(messy_adult)
#' 
#' # Have a look to set
#' head(messy_adult)
#' 
#' # Compute full pipeline
#' clean_adult <- prepareSet(messy_adult)
#' 
#' # With a reference date
#' adult_agg <- prepareSet(messy_adult, analysisDate = as.Date("2017-01-01"))
#' 
#' # Add aggregation by country
#' adult_agg <- prepareSet(messy_adult, analysisDate = as.Date("2017-01-01"), key = "country")
#' 
#' # With some new aggregation functions
#' power <- function(x){sum(x^2)}
#' adult_agg <- prepareSet(messy_adult, analysisDate = as.Date("2017-01-01"), key = "country", 
#'                         functions = c("min", "max", "mean", "power"))
prepareSet <- function(dataSet, finalForm = "data.table", verbose = TRUE, ...){
  ## Environement
  function_name <- "prepareSet"                                     # For print(s)
  
  ## Sanity check
  dataSet <- checkAndReturnDataTable(dataSet)
  
  ## Initialization
  args <- list(...)
  
  ## Computation
  ### 1. Correct dataSet set
  if(verbose){
	printl(function_name, ": step one: correcting mistakes.")
  }
  # 1.0 Filter useless vars
  dataSet <- fastFilterVariables(dataSet, verbose = verbose)
  
  # 1.1 Id variables
  dataSet <- findAndTransformNumerics(dataSet, verbose = verbose)
  dataSet <- findAndTransformDates(dataSet, formats = args[["dateFormats"]], verbose = verbose) 
  
  ### 2. Transform dataSet set
  if(verbose){
	printl(function_name, ": step two: transforming dataSet.")
  }
  # 2.1 Compute differences between dates
  result <- diffDates(dataSet, analysisDate = args[["analysisDate"]], 
                      name_separator = args[["name_separator"]])
  # get ride of dates columns 
  result <- result[, names(result)[!sapply(result, is.date)], with = FALSE] 
  
  # 2.2 Aggregate by key 
  if(!is.null(args[["key"]])){
    key = args[["key"]]
    result <- aggregateByKey(result, key, verbose = verbose, ...)
  }
  
  ### 3 Filter
  if(verbose){
	printl(function_name, ": step three: filtering dataSet.")
  }
  # 3.1 Get ride of useless variables
  result <- fastFilterVariables(dataSet = result, function_name = function_name, 
                                dataName = "result", verbose = verbose)
  
  # 3.2 Round
  if(!is.null(args[["digits"]])){
    digits = args[["digits"]]
    result <- fastRound(result, digits = digits, verbose = verbose)
  }
  
  ### 4 Handle NA
  if(verbose){
	printl(function_name, ": step four: handling NA.")
  }
  result <- fastHandleNa(result, verbose = verbose)
  
  ### 5 Shape set
  if(verbose){
	printl(function_name, ": step five: shaping result.")
  }
  result <- shapeSet(result, finalForm = finalForm, verbose = verbose)
  
  ## Wrapp-up
  return(result)
  
}
  