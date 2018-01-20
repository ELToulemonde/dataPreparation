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
#' @param verbose Should the algorithm talk? (Logical, default to TRUE)
#' @return \code{dataSet} with columns scaled by \strong{reference}. Scaled means that each
#'  column mean will be 0 and each column standard deviation will be 1.
#' @details Scaling numeric values is usefull for some machine learning algorithm such as
#'  logistic regression or neural networks. \cr
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
#' @export
fastScale <- function(dataSet, scales = NULL, verbose = TRUE){
  ## Working environement
  function_name <- "fastScale"
  
  ## Sanity check
  dataSet <- checkAndReturnDataTable(dataSet)
  is.verbose(verbose)
  
  ## Initialization
  # Build scale
  if (is.null(scales)){
    scales <- build_scales(dataSet, cols = "auto", verbose = verbose)
  }
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
    set(dataSet, NULL, col, (dataSet[[col]] - scales[[col]][["mean"]]) / scales[[col]][["sd"]])
    # Update progress bar
    if (verbose){
      setPB(pb, col)  
    }
  }
  if (verbose){
    printl(function_name, ": it took me: ", round( (proc.time() - start_time)[[3]], 2), 
           "s to scale ", length(cols), " numeric columns.")
  }
  ## Wrapp-up
  return(dataSet)
}
#' unScale
#' 
#' Perform efficient unScaling of the scaled data set which will be output of \code{\link{fastScale}}.
#' @param dataSet Matrix, data.frame or data.table, output of \code{\link{fastScale}}
#' @param scales Result of funcion \code{\link{build_scales}}. \cr
#' To perform the same unscaling on train and test, it is compulsory to compute \code{\link{build_scales}}
#' before.
#' @param verbose Should the algorithm talk? (Logical, default to TRUE)
#' @return \code{dataSet} with columns unScaled by \strong{reference}, this will make the overall 
#' values back to actual values, that were before the scale.
#' @details unScaling numeric values is very usefull for most post-model analysis
#' @examples 
#' # Load data
#' data(adult)
#' 
#' # compute scales
#' scales <- build_scales(adult, cols = "auto", verbose = TRUE)
#' 
#' originalAgeMean = mean(adult$age) 
#' originalAgeSD   = sd(adult$age)
#' 
#' # Scale data set
#' adult <- fastScale(adult, scales = scales, verbose = TRUE)
#' 
#' # Control
#' print(mean(adult$age)) # Almost 0
#' print(sd(adult$age)) # 1
#' 
#' adult <- fastUnScale(adult, scales = scales, verbose = TRUE)
#' 
#' mean(adult$age) == originalAgeMean # TRUE
#' sd(adult$age)   == originalAgeSD   # TRUE
#' @export
fastUnScale <- function(dataSet, scales, verbose = TRUE){
  ## Working environement
  function_name <- "fastUnScale"
  
  ## Sanity check
  dataSet <- checkAndReturnDataTable(dataSet)
  is.verbose(verbose)
  
  ## Initialization
  # Build scale
  if (is.null(scales)){
    scales <- build_scales(dataSet, cols = "auto", verbose = verbose)
  }
  cols <- names(scales)
  cols <- real_cols(dataSet, cols, function_name, types = "numeric")
  # verbose
  
  if (verbose){
    pb <- initPB(function_name, cols)
    printl(function_name, ": I will un-scale ", length(cols), " numeric columns.")
    start_time <- proc.time()
  }
  ## Computation
  thresh <- 10^-2
  for (col in cols){
    if(abs(sd(dataSet[[col]]) - 1) < thresh & mean(dataSet[[col]]) < thresh){
      set(dataSet, NULL, col, (dataSet[[col]] * scales[[col]][["sd"]]) + scales[[col]][["mean"]])
    }
    # Update progress bar
    if (verbose){
      setPB(pb, col)  
    }
  }
  if (verbose){
    printl(function_name, ": it took me: ", round( (proc.time() - start_time)[[3]], 2), 
           "s to scale ", length(cols), " numeric columns.")
  }
  ## Wrapp-up
  return(dataSet)
}

