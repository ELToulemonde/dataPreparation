#' Compute scales
#' 
#' Build a list of means and standard deviation for each \code{cols}.
#' @param dataSet Matrix, data.frame or data.table
#' @param cols List of numeric column(s) name(s) of dataSet to transform. To transform all 
#' characters, set it to "auto". (character, default to "auto")
#' @param verbose Should the algorithm talk? (Logical, default to TRUE)
#' @return A list where each element name is a column name of data set and each element contains means and sd.
#' @examples 
#' # Get a data set
#' data(adult)
#' scales <- build_scales(adult, cols = "auto", verbose = TRUE)
#' 
#' print(scales)
#' @export
build_scales <- function(dataSet, cols = "auto", verbose = TRUE){
  ## Working environement
  function_name <- "build_scales"
  
  ## Sanity check
  dataSet <- checkAndReturnDataTable(dataSet)
  is.verbose(verbose)
  cols <- real_cols(dataSet, cols, function_name, types = "numeric")
  
  ## Initialization
  if (verbose){
    pb <- initPB(function_name, cols)
    printl(function_name, ": I will compute scale on  ", length(cols), " numeric columns.")
    start_time <- proc.time()
  }
  scales <- list()
  ## Computation
  for (col in cols){
    scales[[col]] =  list(mean = mean(dataSet[[col]], na.rm = TRUE), sd = sd(dataSet[[col]], na.rm = TRUE))
    # Update progress bar
    if (verbose){
      setPB(pb, col)  
    }
  }
  if (verbose){
    printl(function_name, ": it took me: ", round( (proc.time() - start_time)[[3]], 2), 
           "s to compute scale for ", length(cols), " numeric columns.")
  }
  ## Wrapp-up
  return(scales)
}
#' scale
#' 
#' Perform efficient scaling on a data set.
#' @param dataSet Matrix, data.frame or data.table
#' @param scales Result of funcion \code{\link{build_scales}}, (list, default to NULL). \cr
#' To perform the same scaling on train and test, it is recommended to compute \code{\link{build_scales}}
#' before. If it is kept to NULL, build_scales will be called.
#' @param way should scaling or unscaling be performed? (character either "scale" or "unscale", default to "scale")
#' @param verbose Should the algorithm talk? (Logical, default to TRUE)
#' @return \code{dataSet} with columns scaled (or unscaled) by \strong{reference}. Scaled means that each
#'  column mean will be 0 and each column standard deviation will be 1.
#' @details Scaling numeric values is usefull for some machine learning algorithm such as
#'  logistic regression or neural networks. \cr
#'  Unscaling numeric values can be very usefull for most post-model analysis to do so set way to "unscale". \cr
#' This implementation of scale will be faster that \code{\link{scale}} for large data sets.
#' @examples 
#' # Load data
#' data(adult)
#' 
#' # compute scales
#' scales <- build_scales(adult, cols = "auto", verbose = TRUE)
#' 
#' # Scale data set
#' adult <- fastScale(adult, scales = scales, verbose = TRUE)
#' 
#' # Control
#' print(mean(adult$age)) # Almost 0
#' print(sd(adult$age)) # 1
#' 
#' # To unscale it:
#' adult <- fastScale(adult, scales = scales, way = "unscale", verbose = TRUE)
#' 
#' # Control
#' print(mean(adult$age)) # About 38.6
#' print(sd(adult$age)) # About 13.6
#' @export
fastScale <- function(dataSet, scales = NULL, way = "scale", verbose = TRUE){
  ## Working environement
  function_name <- "fastScale"
  
  ## Sanity check
  dataSet <- checkAndReturnDataTable(dataSet)
  is.verbose(verbose)
  .control_scale_way(way)
  scales <- .build_and_control_scale(scales, dataSet, way, verbose = verbose, function_name)
  
  ## Initialization
  # Build scale
  
  cols <- names(scales)
  cols <- real_cols(dataSet, cols, function_name, types = "numeric")
  # verbose
  
  if (verbose){
    pb <- initPB(function_name, cols)
    printl(function_name, ": I will scale ", length(cols), " numeric columns.")
    start_time <- proc.time()
  }
  ## Computation
  for (col in cols){
    if (way == "scale"){
      set(dataSet, NULL, col, (dataSet[[col]] - scales[[col]][["mean"]]) / scales[[col]][["sd"]])  
    }
    else{
      set(dataSet, NULL, col, (dataSet[[col]] * scales[[col]][["sd"]]) + scales[[col]][["mean"]])  
    }
    # Update progress bar
    if (verbose){
      setPB(pb, col)  
    }
  }
  if (verbose){
    printl(function_name, ": it took me: ", round( (proc.time() - start_time)[[3]], 2), 
           "s to ", way, " ", length(cols), " numeric columns.")
  }
  ## Wrapp-up
  return(dataSet)
}


## .control_scale_way
# -------------------
# Control that input way is correct
# @param way should scaling or unscaling be performed? (character either "scale" or "unscale", default to "scale")
# @param function_name internal param for log consitency
.control_scale_way <- function(way, function_name = "control_scale_way"){
  if (!is.character(way)){
    stop(paste0(function_name, ": way should be a character either 'scale' or 'unscale'"))
  }
  if (! way %in% c("scale", "unscale")){
    stop(paste0(function_name, ": way should either be 'scale' or 'unscale'"))
  }
}

## .build_and_control_scale
# --------------------------
# Control that provided scale has the right format
# @param scales provided scale
# @param dataSet Matrix, data.frame or data.table
# @param way should scaling or unscaling be performed? (character either "scale" or "unscale", default to "scale")
# @param function_name internal param for log consitency
# @return scales if everything went well
.build_and_control_scale <- function(scales, dataSet, way, verbose = TRUE, function_name = ".build_and_control_scale"){
  # Null scales
  if (is.null(scales)){
    if (way == "scale"){
      return(build_scales(dataSet, cols = "auto", verbose = verbose))
    }
    else{
      stop(paste0(function_name, ": to unscale, scales must be feeded by user."))
    }
  }
  
  # Control format
  if (! is.list(scales)){
    stop(paste0(function_name, ": scales should be a named list. Please build it using build_scales."))
  }
  
  # If empty
  if (length(scales) == 0){
    return(scales)
  }
  
  # If not empty control content
  if (! all(sapply(scales, names) == c("mean", "sd"))){
    stop(paste0(function_name, ": scales should be a named list of list having 2 elements: mean and sd. Please build it using build_scales."))
  }
  
  if (! all(sapply(scales, function(scale)sapply(scale, is.numeric)))){
    stop(paste0(function_name, ": scales should be a named list of list having 2 numeric elements: mean and sd. Please build it using build_scales."))
  }
  ## Wrapp-up
  return(scales)
}


