###################################################################################################
############################### prepareSet  #######################################################
###################################################################################################
#' Preparation pipeline
#' 
#' Full pipeline for preparing your dataSet set \cr
#' It will perform the following steps: \cr
#' - Correct set: unfactor factor with many values, id dates and numeric that are hiden in string \cr
#' - Transform set: compute differences between every date, transform dates into factors, generate 
#' features from string..., if `key` is provided, will perform aggregate according to this key \cr
#' - Filter set: filter constant, in double or bijection variables. If `digits` is provided, 
#' will round numeric \cr
#' - Handle NA: will perform \code{\link{fastHandleNa}}) \cr
#' - Shape set: will put the result in asked shape (`finalForm`) with acceptable columns format.
#' @param dataSet Matrix, data.frame or data.table
#' @param finalForm "data.table" or "numerical_matrix" (default to data.table)
#' @param verbose Should the algorithm talk? (logical, default to TRUE)
#' @param ... additional parameters to tune pipeline (see details)
#' @details
#' Additional arguments are available to tune pipeline: 
#' \itemize{
#'   \item \code{key} name of a column of dataSet according to which dataSet should be aggregated 
#'      (character)
#'   \item \code{analysisDate} A date at which the dataSet should be aggregated 
#'      (differences between every date and analysisDate will be computed) (Date)
#'   \item \code{n_unfactor} number of max value in a facotr, set it to -1 to disable 
#'   \code{\link{unFactor}} function.  (numeric, default to 53)
#'   \item \code{digits} The number of digits after comma (optional, numeric, if set will perform 
#'      \code{\link{fastRound}})
#'   \item \code{dateFormats} List of format of Dates in dataSet (list of characters)
#'   \item \code{name_separator} string to separate parts of new column names (string)
#'   \item \code{functions}  aggregation functions for numeric columns (list of functions)
#'   \item \code{factor_date_type} aggregation level to factorize date (see 
#'      \code{\link{generateFactorFromDate}}) (character, default to "yearmonth")
#' }
#' @return A data.table or a numerical matrix (according to finalForm)  and 
#' @examples 
#' # Load ugly set
#' \dontrun{
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
#'                         functions = c(min, max, mean, power))
#' }
#' # "##NOT RUN:" mean that this example hasn't been run on CRAN since its long. But you can run it!
#' @import data.table
#' @export
prepareSet <- function(dataSet, finalForm = "data.table", verbose = TRUE, ...){
  ## Environement
  function_name <- "prepareSet"
  
  ## Sanity check
  dataSet <- checkAndReturnDataTable(dataSet)
  is.verbose(verbose)
  
  ## Initialization
  args <- list(...)
  
  ## Computation
  ### 1. Correct dataSet set
  if(verbose){
    printl(function_name, ": step one: correcting mistakes.")
  }
  # 1.0 Filter useless vars
  dataSet <- fastFilterVariables(dataSet, keep_cols = args[["key"]], verbose = verbose)
  
  # 1.1 Unfactor
  if (! is.null(args[["n_unfactor"]])){
    n_unfactor <- args[["n_unfactor"]]
  }
  else{
    n_unfactor <- 53
  }
  dataSet <- unFactor(dataSet, n_unfactor = n_unfactor, verbose = verbose)
  
  # 1.2 Id variables
  dataSet <- findAndTransformNumerics(dataSet, verbose = verbose)
  dataSet <- findAndTransformDates(dataSet, formats = args[["dateFormats"]], verbose = verbose) 
  
  ### 2. Transform dataSet set
  if(verbose){
    printl(function_name, ": step two: transforming dataSet.")
  }
  
  # 2.1 Generate from dates
  date_cols <- names(dataSet)[sapply(dataSet, is.date)]
  if (!is.null(args[["key"]])){ # don't transform key
    date_cols <- date_cols[date_cols != args[["key"]]]
  }
  # 2.1.1 Compute differences between dates
  result <- generateDateDiffs(dataSet, cols = date_cols, analysisDate = args[["analysisDate"]], 
                              name_separator = args[["name_separator"]], verbose = verbose)
  
  # 2.1.2 Build factor from dates month
  factor_date_type <- build_factor_date_type(args)
  result <- generateFactorFromDate(result, cols = date_cols, type = factor_date_type, drop = TRUE,
                                   name_separator = args[["name_separator"]], verbose = verbose)
  
  # 2.2 Generate features from character
  character_cols <- names(result)[sapply(result, is.character)]
  if (!is.null(args[["key"]])){ # don't transform key
    character_cols <- character_cols[character_cols != args[["key"]]]
  }
  result <- generateFromCharacter(result, cols = character_cols, drop = TRUE, name_separator = args[["name_separator"]], verbose = verbose)
  
  # 2.3 Aggregate by key 
  if(!is.null(args[["key"]])){
    key <- args[["key"]]
    result <- aggregateByKey(result, key, verbose = verbose, listNames = match.call()$functions, ...)
  }
  
  ### 3 Filter
  if(verbose){
    printl(function_name, ": step three: filtering dataSet.")
  }
  # 3.1 Get ride of useless variables
  result <- fastFilterVariables(dataSet = result, keep_cols = args[["key"]], verbose = verbose,
                                dataName = "result")
  
  # 3.2 Round
  if(!is.null(args[["digits"]])){
    digits <- args[["digits"]]
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
